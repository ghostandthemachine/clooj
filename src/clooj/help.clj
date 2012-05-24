; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.help
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT Reflector)
           (java.lang.reflect Modifier)
           (java.awt Color Point)
           (java.util Vector)
           (javax.swing DefaultListCellRenderer ListSelectionModel)
           (javax.swing.event ListSelectionListener)
           (java.io File))
  (:use [clooj.brackets :only (find-enclosing-brackets)]
        [clooj.utils :only (attach-action-keys attach-child-action-keys
                            on-click awt-event when-lets get-text-str)]
        [clooj.project :only (get-selected-projects)]
        [clooj.collaj :only (raw-data)]
        [clojure.repl :only (source-fn)]
        [clj-inspector.jars :only (clj-sources-from-jar jar-files)]
        [clj-inspector.vars :only (analyze-clojure-source
                                    parse-ns-form
                                   )]
        [seesaw.core]
        [clojure.pprint]
        )
  (:require [clojure.string :as string]
            [cemerick.pomegranate.aether :as aether]))

; from http://clojure.org/special_forms
(def special-forms
  {"def" "(def symbol init?)"
   "if"  "(if test then else?)"
   "do"  "(do exprs*)"
   "let" "(let [bindings* ] exprs*)"
   "quote" "(quote form)"
   "var" "(var symbol)"
   "fn"  "(fn name? [params* ] exprs*)"
   "loop" "(loop [bindings* ] exprs*)"
   "recur" "(recur exprs*)"
   "throw" "(throw expr)"
   "try"   "(try expr* catch-clause* finally-clause?)"
   "catch" "(catch classname name expr*)"
   "monitor-enter" "Avoid!"
   "monitor-exit"  "Avoid!"})

(defn present-item [item]
  (str (:name item) " [" (:ns item) "]"))

(defn make-var-super-map [var-maps]
  (into {}
        (for [var-map var-maps]
          [[(:ns var-map) (:name var-map)] var-map])))

(defn classpath-to-jars [project-path classpath]
  (apply concat
    (for [item classpath]
      (cond (.endsWith item "*") (jar-files (apply str (butlast item)))
            (.endsWith item ".jar") (list (File. item))
            :else (jar-files item)))))

(defn get-sources-from-jars [project-path classpath]
   (->> (classpath-to-jars project-path classpath)
       (mapcat clj-sources-from-jar)
       merge
       vals))

(defn get-sources-from-clj-files [classpath]
  (map slurp
       (apply concat
              (for [item classpath]
                (let [item-file (File. item)]
                  (when (.isDirectory item-file)
                    (filter #(.endsWith (.getName %) ".clj")
                            (file-seq item-file))))))))

(defn get-var-maps [project-path classpath]
  (make-var-super-map
      (mapcat analyze-clojure-source
              (concat
                (get-sources-from-jars project-path classpath)
                (get-sources-from-clj-files classpath)))))

(defn find-form-string [text pos]
  (let [[left right] (find-enclosing-brackets text pos)]
    (when (> (.length text) left)
      (.substring text (inc left)))))

(def non-token-chars [\; \~ \@ \( \) \[ \] \{ \} \  \. \newline \/ \" \'])

(defn local-token-location [text pos]
  (let [n (.length text)
        pos (-> pos (Math/max 0) (Math/min n))]
    [(loop [p (dec pos)]
       (if (or (neg? p)
               (some #{(.charAt text p)} non-token-chars))
         (inc p)
         (recur (dec p))))
     (loop [p pos]
       (if (or (>= p n)
               (some #{(.charAt text p)} non-token-chars))
         p
         (recur (inc p))))]))

(defn head-token [form-string]
  (when form-string
    (second
      (re-find #"(.*?)[\s|\)|$]"
               (str (.trim form-string) " ")))))

(defn current-ns-form [root]
  (-> (select root [:#doc-text-area]) .getText read-string))
  
(defn ns-available-names [root]
  (parse-ns-form (current-ns-form root)))

(defn arglist-from-var-map [m]
  (or
    (when-let [args (:arglists m)]
      (str (-> m :ns) "/" (:name m) ": " args))
    ""))

(defn token-from-caret-pos [text pos]
  (head-token (find-form-string text pos)))

(defn matching-vars [app token]
  (into {} (filter #(= token (second (first %)))
                   (-> @app :repl deref :var-maps deref))))

(defn var-from-token [root current-ns token]
  (when token
    (if (.contains token "/")
      (vec (.split token "/"))
      (or ((ns-available-names root) token)
          [current-ns token]))))

(defn arglist-from-token [root app ns token]
  (or (special-forms token)
    (-> @app :repl deref :var-maps
        deref (get (var-from-token root ns token))
        arglist-from-var-map)))

(defn arglist-from-caret-pos [root app ns text pos]
  (let [token (token-from-caret-pos text pos)]
    (arglist-from-token root app ns token)))

;; tab help

(defonce help-state (atom {:visible false :token nil :pos nil}))

(defn var-map [v]
  (when-let [m (meta v)]
    (let [ns (:ns m)]
      (-> m
          (select-keys [:doc :ns :name :arglists])
          (assoc :source (binding [*ns* ns]
                           (source-fn (symbol (str ns "/" name)))))))))

(defn var-help [var-map]
  (let [{:keys [doc ns name arglists source]} var-map]
    (str name
         (if ns (str " [" ns "]") "") "\n"
         arglists
         "\n\n"
         (if doc
           (str "Documentation:\n" doc)
           "No documentation found.")
         "\n\n"
         (if source
           (str "Source:\n"
                (if doc
                  (.replace source doc "...docs...")
                  source))
           "No source found."))))

(defn create-param-list
  ([method-or-constructor static]
    (str " (["
         (let [type-names (map #(.getSimpleName %)
                               (.getParameterTypes method-or-constructor))
               param-names (if static type-names (cons "this" type-names))]
           (apply str (interpose " " param-names)))
         "])"))
  ([method-or-constructor]
    (create-param-list method-or-constructor true)))

(defn constructor-help [constructor]
  (str (.. constructor getDeclaringClass getSimpleName) "."
       (create-param-list constructor)))

(defn method-help [method]
  (let [stat (Modifier/isStatic (.getModifiers method))]
    (str
      (if stat
        (str (.. method getDeclaringClass getSimpleName)
             "/" (.getName method))
        (str "." (.getName method)))
     (create-param-list method stat)
      " --> " (.getName (.getReturnType method)))))

(defn field-help [field]
  (let [c (.. field getDeclaringClass getSimpleName)]
  (str
    (if (Modifier/isStatic (.getModifiers field))
      (str (.. field getDeclaringClass getSimpleName)
           "/" (.getName field)
           (when (Modifier/isFinal (.getModifiers field))
             (str " --> " (.. field (get nil) toString))))
      (str "." (.getName field) " --> " (.getName (.getType field)))))))

(defn class-help [c]
  (apply str
         (concat
           [(present-item c) "\n  java class"]
           ["\n\nCONSTRUCTORS\n"]
           (interpose "\n"
                      (sort
                        (for [constructor (.getConstructors c)]
                          (constructor-help constructor))))
           ["\n\nMETHODS\n"]
           (interpose "\n"
                      (sort
                        (for [method (.getMethods c)]
                          (method-help method))))
           ["\n\nFIELDS\n"]
           (interpose "\n"
                      (sort
                        (for [field (.getFields c)]
                          (field-help field)))))))

(defn item-help [item]
  (cond (map? item) (var-help item)
        (class? item) (class-help item)))    

(defn set-first-component [split-pane comp]
  (let [loc (.getDividerLocation split-pane)]
    (.setTopComponent split-pane comp)
    (.setDividerLocation split-pane loc)))

(defn clock-num [i n]
  (if (zero? n)
    0
    (cond (< i 0) (dec n)
          (>= i n) 0
          :else i)))

(defn list-size [list]
  (-> list .getModel .getSize))

(defn advance-help-list [root app token index-change-fn]
  (let [help-list (select root [:#completion-list])
        token-pat1 (re-pattern (str "(?i)\\A\\Q" token "\\E"))
        token-pat2 (re-pattern (str "(?i)\\Q" token "\\E"))]
    (if (not= token (@help-state :token))
      (do
        (swap! help-state assoc :token token)
        (.setListData help-list (Vector.))
        (when-let [items (-> app :repl deref :var-maps deref vals)]
          (let [best (sort-by #(.toLowerCase (:name %))
                              (filter
                                #(re-find token-pat1 (:name %))
                                items))
                others (sort-by #(.toLowerCase (:name %))
                                (filter 
                                  #(re-find token-pat2 (.substring (:name %) 1))
                                  items))
                collaj-items (or (raw-data token))]
            (.setListData help-list
                          (Vector. (concat best others collaj-items)))
            (.setSelectedIndex help-list 0)
                   )))
      (let [n (list-size help-list)]
        (when (pos? n)
          (.setSelectedIndex help-list
                             (clock-num
                               (index-change-fn
                                    (.getSelectedIndex help-list))
                               n)))))
    (when (pos? (list-size help-list))
      (set-first-component (select root [:#repl-split-pane])
                           (select root [:#help-text-scroll-pane]))
      (set-first-component (select root [:#doc-split-pane])
                           (select root [:#completion-panel]))
      (config! (select root [:#repl-label]) :text "Documentation")
      (.ensureIndexIsVisible help-list
                             (.getSelectedIndex help-list)))))
  
; (defn get-list-item [app]
;   (-> app :completion-list .getSelectedValue))
  
(defn get-list-item [root]
  (-> (select root [:#completion-list]) .getSelectedValue))

; (defn get-list-artifact [app]
;   (binding [*read-eval* false] 
;     (read-string (:artifact (get-list-item app)))))

(defn get-list-artifact [root]
  (binding [*read-eval* false] 
    (read-string (:artifact (get-list-item root)))))

; (defn get-list-token [app]
;   (let [val (get-list-item app)]
;     (str (:ns val) "/" (:name val))))

(defn get-list-token [root]
  (let [val (get-list-item root)]
    (str (:ns val) "/" (:name val))))

; (defn show-help-text [app choice]
;   (let [help-text (or (when choice (item-help choice)) "")]
;     (config! (select root [:#help-text-area]) :text help-text))
;   (-> app :help-text-scroll-pane .getViewport
;       (.setViewPosition (Point. (int 0) (int 0)))))

(defn show-help-text [root choice]
  (let [help-text (or (when choice (item-help choice)) "")]
    (config! (select root [:#help-text-area]) :text help-text))
  (-> (select root [:#help-text-scroll-pane]) .getViewport
      (.setViewPosition (Point. (int 0) (int 0)))))



; (defn show-tab-help [app text-comp index-change-fn]
;   (awt-event
;     (let [text (get-text-str text-comp)
;           pos (.getCaretPosition text-comp)
;           [start stop] (local-token-location text pos)]
;       (when-let [token (.substring text start stop)]
;         (swap! help-state assoc :pos start :visible true)
;         (advance-help-list app token index-change-fn)))))

(defn show-tab-help [app text-comp index-change-fn]
  (awt-event
    (let [text (get-text-str text-comp)
          pos (.getCaretPosition text-comp)
          [start stop] (local-token-location text pos)]
      (when-let [token (.substring text start stop)]
        (swap! help-state assoc :pos start :visible true)
        (advance-help-list app token index-change-fn)))))

(defn hide-tab-help [root]
  (awt-event
    (when (@help-state :visible)
      (set-first-component (select root [:#repl-split-pane])
                           (select root [:#repl-out-scroll-pane]))
      (set-first-component (select root [:#doc-split-pane])
                           (select root [:#docs-tree-panel]))
      (config! (select root [:#repl-label]) :text "Clojure REPL output"))
    (swap! help-state assoc :visible false :pos nil)))

; (defn hide-tab-help [app]
;   (awt-event
;     (when (@help-state :visible)
;       (set-first-component (app :repl-split-pane)
;                            (app :repl-out-scroll-pane))
;       (set-first-component (app :doc-split-pane)
;                            (app :docs-tree-panel))
;       (.setText (app :repl-label) "Clojure REPL output"))
;     (swap! help-state assoc :visible false :pos nil)))
  
; (defn help-handle-caret-move [app text-comp]
;   (awt-event
;     (when (@help-state :visible)
;       (let [[start _] (local-token-location (get-text-str text-comp) 
;                                             (.getCaretPosition text-comp))]
;         (if-not (= start (@help-state :pos))
;           (hide-tab-help app)
;           (show-tab-help app text-comp identity))))))  

(defn help-handle-caret-move [root text-comp]
  (awt-event
    (when (@help-state :visible)
      (let [[start _] (local-token-location (get-text-str text-comp) 
                                            (.getCaretPosition text-comp))]
        (if-not (= start (@help-state :pos))
          (hide-tab-help root)
          (show-tab-help root text-comp identity))))))

(defn update-ns-form [app]
  (current-ns-form app))

(defn load-dependencies [artifact]
  (println "Loading " artifact)
  (let [deps (cemerick.pomegranate.aether/resolve-dependencies
               :coordinates [artifact]
               :repositories {"clojars" "http://clojars.org/repo"})]
    (aether/dependency-files deps)))
  
; (defn update-token [app text-comp new-token]
;   (awt-event
;     (let [[start stop] (local-token-location
;                          (get-text-str text-comp)
;                          (.getCaretPosition text-comp))
;           len (- stop start)]
;       (when (and (not (empty? new-token)) (-> app :completion-list
;                                               .getModel .getSize pos?))
;         (.. text-comp getDocument
;             (replace start len new-token nil))))))

(defn update-token [root text-comp new-token]
  (awt-event
    (let [[start stop] (local-token-location
                         (get-text-str text-comp)
                         (.getCaretPosition text-comp))
          len (- stop start)]
      (when (and (not (empty? new-token)) (-> (select root [:#completion-list])
                                              .getModel .getSize pos?))
        (.. text-comp getDocument
            (replace start len new-token nil))))))

; (defn setup-tab-help [app text-comp]
;   (attach-action-keys text-comp
;     ["TAB" #(show-tab-help app text-comp inc)]
;     ["shift TAB" #(show-tab-help app text-comp dec)]
;     ["ESCAPE" #(hide-tab-help app)])
;   (attach-child-action-keys text-comp
;     ["ENTER" #(@help-state :visible)
;              #(do (hide-tab-help app)
;                     (load-dependencies app (get-list-artifact app))
;                     (update-token app text-comp (get-list-token app)))]))

(defn setup-tab-help [root text-comp]
  (attach-action-keys text-comp
    ["TAB" #(show-tab-help root text-comp inc)]
    ["shift TAB" #(show-tab-help root text-comp dec)]
    ["ESCAPE" #(hide-tab-help root)])
  (attach-child-action-keys text-comp
    ["ENTER" #(@help-state :visible)
             #(do (hide-tab-help root)
                    (load-dependencies (get-list-artifact root))
                    (update-token root text-comp (get-list-token root)))]))

(defn find-focused-text-pane [root]
  (let [t1 (select root [:#doc-text-area])
        t2 (select root [:#repl-in-text-area])]
    (cond (config t1 :has-focus) t1
          (config t2 :has-focus) t2)))

; (defn setup-completion-list [l app]
;   (doto l
;     (.setBackground (Color. 0xFF 0xFF 0xE8))
;     (.setFocusable false)
;     (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
;     (.setCellRenderer
;       (proxy [DefaultListCellRenderer] []
;         (getListCellRendererComponent [list item index isSelected cellHasFocus]
;           (doto (proxy-super getListCellRendererComponent list item index isSelected cellHasFocus)
;             (.setText (present-item item)))))) 
;     (.addListSelectionListener
;       (reify ListSelectionListener
;         (valueChanged [_ e]
;           (when-not (.getValueIsAdjusting e)
;             (.ensureIndexIsVisible l (.getSelectedIndex l))
;             (show-help-text app (.getSelectedValue l))))))
;     (on-click 2 #(when-let [text-pane (find-focused-text-pane app)]
;                         (update-token app text-pane)))))

(defn setup-completion-list [l app root]
  (doto l
    (.setBackground (Color. 0xFF 0xFF 0xE8))
    (.setFocusable false)
    (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
    (.setCellRenderer
      (proxy [DefaultListCellRenderer] []
        (getListCellRendererComponent [list item index isSelected cellHasFocus]
          (doto (proxy-super getListCellRendererComponent list item index isSelected cellHasFocus)
            (.setText (present-item item)))))) 
    (.addListSelectionListener
      (reify ListSelectionListener
        (valueChanged [_ e]
          (when-not (.getValueIsAdjusting e)
            (.ensureIndexIsVisible l (.getSelectedIndex l))
            (show-help-text app (.getSelectedValue l))))))
    (on-click 2 #(when-let [text-pane (find-focused-text-pane root)]
                        (update-token root text-pane)))))
