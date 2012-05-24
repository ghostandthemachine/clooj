; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.dev-tools
  (:import (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree KeyStroke SpringLayout JTextPane
                        ListSelectionModel
                        UIManager)
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (java.awt Insets Rectangle Window)
           (java.awt.event AWTEventListener FocusAdapter MouseAdapter
                           WindowAdapter KeyAdapter)
           (java.awt AWTEvent Color Font GridLayout Toolkit)
           (java.net URL)
           (java.util Map)
           (java.io File FileReader StringReader
                    BufferedWriter OutputStreamWriter FileOutputStream)
           (org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants TokenMakerFactory)	
           (org.fife.ui.rtextarea RTextScrollPane))
  
  (:use [seesaw core graphics]

        [clojure.pprint :only (pprint)]
        [clooj.brackets]
        [clooj.highlighting]
        [clooj.repl]
        [clooj.search]
        [clooj.help :only (arglist-from-caret-pos show-tab-help setup-tab-help
                           setup-completion-list help-handle-caret-move
                           find-focused-text-pane  
                           token-from-caret-pos)]
        [clooj.project :only (add-project load-tree-selection
                              load-expanded-paths load-project-set
                              save-expanded-paths
                              save-tree-selection get-temp-file
                              get-selected-projects
                              get-selected-file-path
                              remove-selected-project update-project-tree
                              rename-project set-tree-selection
                              get-code-files get-selected-namespace)]
        [clooj.utils :only (clooj-prefs write-value-to-prefs read-value-from-prefs
                            is-mac count-while add-text-change-listener
                            set-selection scroll-to-pos add-caret-listener
                            attach-child-action-keys attach-action-keys
                            get-caret-coords add-menu
                            add-menu-item
                            choose-file choose-directory
                            comment-out uncomment-out
                            indent unindent awt-event persist-window-shape
                            confirmed? create-button is-win
                            get-keystroke printstream-to-writer
                            focus-in-text-component
                            scroll-to-caret when-lets
                            constrain-to-parent make-split-pane
                            gen-map on-click
                            remove-text-change-listeners get-text-str 
                            scroll-to-line get-directories)]
        [clooj.indent :only (setup-autoindent fix-indent-selected-lines)]
        [clooj.style :only (get-monospaced-fonts show-font-window)]
        [clooj.navigate :only (attach-navigation-keys)])
  (:require [clojure.main :only (repl repl-prompt)]
            [clojure.set])
  (:gen-class
   :methods [^{:static true} [show [] void]]))


(def gap 5)

(def embedded (atom false))

(def changing-file (atom false))

(defprotocol DynamicWordHighlighter
  (addWordToHighlight [this word token-type]))

(extend-type RSyntaxTextArea
  DynamicWordHighlighter
  (addWordToHighlight [word token-type]))
    
(defn make-rsyntax-text-area []
  (let [tmf (TokenMakerFactory/getDefaultInstance)
        token-maker (.getTokenMaker tmf "text/clojure")
        token-map (.getWordsToHighlight token-maker)
        rsta (proxy [RSyntaxTextArea] []
               (addWordToHighlight [word token-type]
                                   (do
                                     (.put token-map word token-type)
                                     token-type)))]
      (.. rsta getDocument (setTokenMakerFactory tmf))
    rsta))
  
(defn make-text-area [wrap]
  (doto (RSyntaxTextArea.)
    (.setAnimateBracketMatching false)
    (.setBracketMatchingEnabled false)
    (.setAutoIndentEnabled false)
    ))

(def get-clooj-version
  (memoize
    (fn []
      (try
        (-> (Thread/currentThread) .getContextClassLoader
            (.getResource "clooj/core.class") .toString
            (.replace "clooj/core.class" "project.clj")
            URL. slurp read-string (nth 2))
        (catch Exception _ nil)))))

;; font

(defonce current-font (atom nil))

(defn font [name size]
  (Font. name Font/PLAIN size))

(def default-font
  (cond (is-mac) ["Monaco" 11]
        (is-win) ["Courier New" 12]
        :else    ["Monospaced" 12]))

(defn set-font
  ([root font-name size]
    (let [f (font font-name size)]
      (awt-event
        (write-value-to-prefs clooj-prefs "app-font"
                              [font-name size])
        (config! 
          [(select root [:#doc-text-area]) 
           (select root [:#repl-in-text-area])
           (select root [:#repl-out-text-area]) 
           (select root [:#search-text-area]) 
           ; (select root [:#arglist-label])
           ; (select root [:#help-text-area])
           ; (select root [:#completion-list])
           ]
           :font f)

        (reset! current-font [font-name size]))))
  ([root font-name]
    (let [size (second @current-font)]
      (set-font root font-name size))))

(defn load-font [root]
   (apply set-font root (or (read-value-from-prefs clooj-prefs "app-font")
                     default-font)))
  
(defn resize-font [root fun]
  (let [[name size] @current-font]
    (set-font root name (fun size))))

(defn grow-font [root] (resize-font root inc))

(defn shrink-font [root] (resize-font root dec))

;; caret finding

(def highlight-agent (agent nil))

(def arglist-agent (agent nil))

(def caret-position (atom nil))

; (defn save-caret-position [app]
;   (when-lets [text-area (app :doc-text-area)
;               pos (get @caret-position text-area)
;               file @(:file app)]
;     (when-not (.isDirectory file)
;       (let [key-str (str "caret_" (.hashCode (.getAbsolutePath file)))]
;         (write-value-to-prefs clooj-prefs key-str pos)))))

(defn save-caret-position [root app]
  (when-lets [text-area (select root [:#doc-text-area])
              pos (get @caret-position text-area)
              file @(:file @app)]
    (when-not (.isDirectory file)
      (let [key-str (str "caret_" (.hashCode (.getAbsolutePath file)))]
        (write-value-to-prefs clooj-prefs key-str pos)))))

(defn load-caret-position [root app]
  (when-lets [text-area (select root [:#doc-text-area])
              file @(:file app)]
    (when-not (.isDirectory file)
      (when-lets [key-str (str "caret_" (.hashCode (.getAbsolutePath file)))
                  pos (read-value-from-prefs clooj-prefs key-str)]
        (let [length (.. text-area getDocument getLength)
              pos2 (Math/min pos length)]
          (.setCaretPosition text-area pos2)
          (scroll-to-caret text-area))))))

(defn update-caret-position [text-comp]
  (swap! caret-position assoc text-comp (.getCaretPosition text-comp)))


(defn display-caret-position [root]
  (let [{:keys [row col]} (get-caret-coords (select root [:#doc-text-area]))]
    (config! (select root [:#pos-label]) :text (str " " (inc row) "|" (inc col)))
    root))

(defn handle-caret-move [root app text-comp ns]
  (update-caret-position text-comp)
  (help-handle-caret-move root text-comp)
  (send-off highlight-agent
            (fn [old-pos]
              (try
                (let [pos (@caret-position text-comp)
                      text (get-text-str text-comp)]
                  (when-not (= pos old-pos)
                    (let [enclosing-brackets (find-enclosing-brackets text pos)
                          bad-brackets (find-bad-brackets text)
                          good-enclosures (clojure.set/difference
                                            (set enclosing-brackets) (set bad-brackets))]
                      (awt-event
                        (highlight-brackets text-comp good-enclosures bad-brackets)))))
                (catch Throwable t (.printStackTrace t)))))
  (when ns
    (send-off arglist-agent 
              (fn [old-pos]
                (try
                  (let [pos (@caret-position text-comp)
                        text (get-text-str text-comp)]
                    ; (when-not (= pos old-pos)
                    ;   (let [arglist-text (arglist-from-caret-pos root app ns text pos)]
                    ;     (awt-event (config! (select root [:#arglist-label]) :text arglist-text))))

                    )
                  (catch Throwable t (.printStackTrace t)))))))

(defn activate-caret-highlighter [root app]
  (when-let [text-comp (select root [:#doc-text-area])]
    (let [f #(handle-caret-move root app text-comp (get-file-ns root))] 
      (add-caret-listener text-comp f)
      (add-text-change-listener text-comp f)))
  (when-let [text-comp (select root [:#repl-in-text-area])]
    ; (pprint app)
    (let [f #(handle-caret-move root app text-comp (get-repl-ns app))] ;;;;;;;;;;;
      (add-caret-listener text-comp f)
      (add-text-change-listener text-comp f))))

;; double-click paren to select form

(defn double-click-selector [text-comp]
  (.addMouseListener text-comp
    (proxy [MouseAdapter] []
      (mouseClicked [e]
        (when (== 2 (.getClickCount e))
          (when-lets [pos (.viewToModel text-comp (.getPoint e))
                      c (.. text-comp getDocument (getText pos 1) (charAt 0))
                      pos (cond (#{\( \[ \{ \"} c) (inc pos)
                                (#{\) \] \} \"} c) pos)
                      [a b] (find-enclosing-brackets (get-text-str text-comp) pos)]
            (set-selection text-comp a (inc b))))))))

;; temp files

(defn dump-temp-doc [root orig-f txt]
  (try 
    (when orig-f
      (let [orig (.getAbsolutePath orig-f)
            f (.getAbsolutePath (get-temp-file orig-f))]
        (spit f txt)
        (awt-event (.updateUI (select root [:#docs-tree])))))
       (catch Exception e nil)))

(def temp-file-manager (agent 0))

(defn update-temp [root app]
  (let [text-comp (select root [:#doc-text-area])
        txt (get-text-str text-comp)
        f @(@app :file)]
    (send-off temp-file-manager
              (fn [old-pos]
                (try
                  (when-let [pos (get @caret-position text-comp)]
                    (when-not (= old-pos pos)
                      (dump-temp-doc root f txt))
                    pos)
                     (catch Throwable t (awt-event (.printStackTrace t))))))))
  
(defn setup-temp-writer [root app]
  (let [text-comp (select root [:#doc-text-area])]
    (add-text-change-listener text-comp
      #(when-not @changing-file
         (update-caret-position text-comp)
         (update-temp root app)))))

(declare restart-doc)

(defn file-suffix [^File f]
  (when-lets [name (.getName f)
             last-dot (.lastIndexOf name ".")
             suffix (.substring name (inc last-dot))]
    suffix))
    
(defn text-file? [f]
  (not (some #{(file-suffix f)}
             ["jar" "class" "dll" "jpg" "png" "bmp"])))

(defn setup-tree [root app]
  (let [tree (select root [:#docs-tree])
        save #(save-expanded-paths tree)]
    (doto tree
      (.setRootVisible false)
      (.setShowsRootHandles true)
      (.. getSelectionModel (setSelectionMode TreeSelectionModel/SINGLE_TREE_SELECTION))
      (.addTreeExpansionListener
        (reify TreeExpansionListener
          (treeCollapsed [this e] (save))
          (treeExpanded [this e] (save))))
      (.addTreeSelectionListener
        (reify TreeSelectionListener
          (valueChanged [this e]
            (awt-event
              (save-tree-selection tree (.getNewLeadSelectionPath e))
              (let [f (.. e getPath getLastPathComponent
                            getUserObject)]
                (when (and
                        (not= f @(@app :file))
                        (text-file? f))
                  (restart-doc root app f))))))))))
  
;; build gui

; (defn make-tabbed-pane [text-area label]
;   (tabbed-panel :placement :top :tabs [{:title label :content text-area}]))

(defn make-scroll-pane [text-area]
  (RTextScrollPane. text-area))

(defn setup-search-text-area [root]
  (let [sta (doto (select root [:#search-text-area])
      (.setVisible false)
      (.setBorder (BorderFactory/createLineBorder Color/DARK_GRAY))
      (.addFocusListener (proxy [FocusAdapter] [] (focusLost [_] (stop-find root)))))]
    (add-text-change-listener sta #(update-find-highlight root false))
    (attach-action-keys sta ["ENTER" #(highlight-step root false)]
                            ["shift ENTER" #(highlight-step root true)]
                            ["ESCAPE" #(escape-find root)])))

(defn create-arglist-label []
  (doto (JLabel.)
    (.setVisible true)
    ))

(defn exit-if-closed [^java.awt.Window f]
  (when-not @embedded
    (.addWindowListener f
      (proxy [WindowAdapter] []
        (windowClosing [_]
          (System/exit 0))))))

(def no-project-txt
    "\n Welcome to clooj, a lightweight IDE for clojure\n
     To start coding, you can either\n
       a. create a new project
            (select the Project > New... menu), or
       b. open an existing project
            (select the Project > Open... menu)\n
     and then either\n
       a. create a new file
            (select the File > New menu), or
       b. open an existing file
            (click on it in the tree at left).")
       
(def no-file-txt
    "To edit source code you need to either: <br>
     &nbsp;1. create a new file 
     (select menu <b>File > New...</b>)<br>
     &nbsp;2. edit an existing file by selecting one at left.</html>")

(defn open-project [app]
  (when-let [dir (choose-directory (app :f) "Choose a project directory")]
    (let [project-dir (if (= (.getName dir) "src") (.getParentFile dir) dir)]
      (write-value-to-prefs clooj-prefs "last-open-dir" (.getAbsolutePath (.getParentFile project-dir)))
      (add-project app (.getAbsolutePath project-dir))
      (update-project-tree (:docs-tree app))
      (when-let [clj-file (or (-> (File. project-dir "src")
                                 .getAbsolutePath
                                 (get-code-files ".clj")
                                 first)
                              project-dir)]
        (awt-event (set-tree-selection (app :docs-tree) (.getAbsolutePath clj-file)))))))

(defn attach-global-action-keys [comp app]
  (attach-action-keys comp
    ["cmd1 EQUALS" #(grow-font app)]
    ["cmd1 shift EQUALS" #(grow-font app)]
    ["cmd1 PLUS" #(grow-font app)]                  
    ["cmd2 MINUS" #(.toBack (:frame app))]
    ["cmd2 PLUS" #(.toFront (:frame app))]
    ["cmd2 EQUALS" #(.toFront (:frame app))]
    ["cmd1 shift O" #(open-project app)]
    ["cmd1 K"#(.setText (app :repl-out-text-area) "")]))
  
(defn on-window-activation [win fun]
  (.addWindowListener win
    (proxy [WindowAdapter] []
      (windowActivated [_]
        (fun)))))

;; clooj docs

(defn restart-doc [root app ^File file]
  (send-off temp-file-manager
            (let [f @(:file @app)
                  txt (get-text-str (select root [:#doc-text-area]))]
              (let [temp-file (get-temp-file f)]
                (fn [_] (when (and f temp-file (.exists temp-file))
                          (dump-temp-doc root f txt))
                  0))))
  (await temp-file-manager)
  (let [frame root
        text-area (select root [:#doc-text-area])
        temp-file (get-temp-file file)
        file-to-open (if (and temp-file (.exists temp-file)) temp-file file)
        doc-label (select root [:#doc-label])]
    ;(remove-text-change-listeners text-area)
    (reset! changing-file true)
    (save-caret-position root app)
    (.. text-area getHighlighter removeAllHighlights)
    (if (and file-to-open (.exists file-to-open) (.isFile file-to-open))
      (do (let [txt (slurp file-to-open)
                rdr (StringReader. txt)]
            (.read text-area rdr nil))
          (.setText doc-label (str "Source Editor \u2014 " (.getName file)))
          (.setEditable text-area true)	
          (.setSyntaxEditingStyle text-area
            (if (.endsWith (.getName file-to-open) ".clj")
              SyntaxConstants/SYNTAX_STYLE_CLOJURE
              SyntaxConstants/SYNTAX_STYLE_NONE)))
      (do (.setText text-area no-project-txt)
          (.setText doc-label (str "Source Editor (No file selected)"))
          (.setEditable text-area false)))
    (update-caret-position text-area)
    (setup-autoindent text-area)
    (reset! (@app :file) file)
    (switch-repl root app (first (get-selected-projects root)))
    (apply-namespace-to-repl root app)))

(defn save-file [root app]
  (try
    (let [f @(@app :file)
          ft (File. (str (.getAbsolutePath f) "~"))]
      (with-open [writer (BufferedWriter.
                           (OutputStreamWriter.
                             (FileOutputStream. f)
                             "UTF-8"))]
        (.write (select root [:#doc-text-area]) writer))
      (send-off temp-file-manager (fn [_] 0))
      (.delete ft)
      (.updateUI (select root [:#docs-tree])))
    (catch Exception e (JOptionPane/showMessageDialog
                         nil "Unable to save file."
                         "Oops" JOptionPane/ERROR_MESSAGE))))

(def project-clj-text (.trim
  "
  (defproject PROJECTNAME \"1.0.0-SNAPSHOT\"
  :description \"FIXME: write\"
  :dependencies [[org.clojure/clojure \"1.3.0\"]])
  "))
      
(defn specify-source [project-dir title default-namespace]
  (when-let [namespace (JOptionPane/showInputDialog nil
                         "Please enter a fully-qualified namespace"
                         title
                         JOptionPane/QUESTION_MESSAGE
                         nil
                         nil
                         default-namespace)]
    (let [tokens (map munge (.split namespace "\\."))
          dirs (cons "src" (butlast tokens))
          dirstring (apply str (interpose File/separator dirs))
          name (last tokens)
          the-dir (File. project-dir dirstring)]
      (.mkdirs the-dir)
      [(File. the-dir (str name ".clj")) namespace])))
      
(defn create-file [root project-dir default-namespace]
   (when-let [[file namespace] (specify-source project-dir
                                          "Create a source file"
                                          default-namespace)]
     (let [tree (select root [:#docs-tree])]
       (spit file (str "(ns " namespace ")\n"))
       (update-project-tree (select root [:#docs-tree]))
       (set-tree-selection tree (.getAbsolutePath file)))))

(defn new-project-clj [project-dir]
  (let [project-name (.getName project-dir)
        file-text (.replace project-clj-text "PROJECTNAME" project-name)]
    (spit (File. project-dir "project.clj") file-text)))

(defn new-project [root]
  (try
    (when-let [dir (choose-file root "Create a project directory" "" false)]
      (awt-event
        (let [path (.getAbsolutePath dir)]
          (.mkdirs (File. dir "src"))
          (new-project-clj dir)
          (add-project {} path)
          (update-project-tree (select root [:#docs-tree]))
          (set-tree-selection (select root [:#docs-tree]) path)
          (create-file root dir (str (.getName dir) ".core")))))
      (catch Exception e (do (JOptionPane/showMessageDialog nil
                               "Unable to create project."
                               "Oops" JOptionPane/ERROR_MESSAGE)
                           (.printStackTrace e)))))
  
(defn rename-file [root app]
  (when-let [old-file @(@app :file)]
    (let [tree (select root [:#docs-tree])
          [file namespace] (specify-source
                             (first (get-selected-projects root))
                             "Rename a source file"
                             (get-selected-namespace tree))]
      (when file
        (.renameTo @(app :file) file)
        (update-project-tree (:docs-tree app))
        (awt-event (set-tree-selection tree (.getAbsolutePath file)))))))

(defn delete-file [root]
  (let [path (get-selected-file-path root)]
    (when (confirmed? "Are you sure you want to delete this file?\nDeleting cannot be undone." path)
      (loop [f (File. path)]
        (when (and (empty? (.listFiles f))
                   (let [p (-> f .getParentFile .getAbsolutePath)]
                     (or (.contains p (str File/separator "src" File/separator))
                         (.endsWith p (str File/separator "src")))))
          (.delete f)
          (recur (.getParentFile f))))
      (update-project-tree (select root [:#docs-tree])))))

(defn remove-project [app]
  (when (confirmed? "Remove the project from list? (No files will be deleted.)"
                    "Remove project")
    (remove-selected-project app)))

(defn revert-file [app]
  (when-let [f @(:file app)]
    (let [temp-file (get-temp-file f)]
      (when (.exists temp-file))
        (let [path (.getAbsolutePath f)]
          (when (confirmed? "Revert the file? This cannot be undone." path)
            (.delete temp-file)
            (update-project-tree (:docs-tree app))
            (restart-doc app f))))))

(defn- dir-rank [dir]
  (get {"src" 0 "test" 1 "lib" 2} (.getName dir) 100))

(defn- find-file [project-path relative-file-path]
  (let [classpath-dirs (sort-by dir-rank < (get-directories (File. project-path)))
        file-candidates (map 
                          #(File. (str (.getAbsolutePath %) File/separatorChar relative-file-path)) 
                          classpath-dirs)]
    (first (filter #(and (.exists %) (.isFile %)) file-candidates))))

(defn goto-definition [root ns app]
  (let [text-comp (select root [:#doc-text-area])
        pos (.getCaretPosition text-comp)
        text (.getText text-comp)
        src-file (:file (meta (do 
                                            (token-from-caret-pos ns text pos) nil)))
        line (:line (meta (do (find-ns (symbol ns))
                                        (token-from-caret-pos ns text pos) nil)))
        project-path (first (get-selected-projects root))
        file (find-file project-path src-file)]
    (when (and file line)
      (when (not= file @(:file @app))
        (restart-doc app file)
        (set-tree-selection (select root [:#docs-tree]) (.getAbsolutePath file)))
      (scroll-to-line text-comp line))))

(defn make-menus [root app]
  (when (is-mac)
    (System/setProperty "apple.laf.useScreenMenuBar" "true"))
  (let [menu-bar (menubar)]
    (config! root :menubar menu-bar)
    (let [file-menu
          (add-menu menu-bar "File" "F"
            ["New" "N" "cmd1 N" #(create-file root app (first (get-selected-projects root)) "")]
            ["Save" "S" "cmd1 S" #(save-file root app)]
            ["Move/Rename" "M" nil #(rename-file root app)]
            ["Revert" "R" nil #(revert-file app)]
            ["Delete" nil nil #(delete-file app)])]
          (when-not (is-mac)
            (add-menu-item file-menu "Exit" "X" nil #(System/exit 0))))
      (add-menu menu-bar "Project" "P"
        ["New..." "N" "cmd1 shift N" #(new-project app)]
        ["Open..." "O" "cmd1 shift O" #(open-project app)]
        ["Move/Rename" "M" nil #(rename-project app)]
        ["Remove" nil nil #(remove-project app)])
      (add-menu menu-bar "Source" "U"
        ["Comment-out" "C" "cmd1 SEMICOLON" #(comment-out (select root [:#doc-text-area]))]
        ["Uncomment-out" "U" "cmd1 shift SEMICOLON" #(uncomment-out (select root [:#doc-text-area]))]
        ["Fix indentation" "F" "cmd1 BACK_SLASH" #(fix-indent-selected-lines (select root [:#doc-text-area]))]
        ["Indent lines" "I" "cmd1 CLOSE_BRACKET" #(indent (select root [:#doc-text-area]))]
        ["Unindent lines" "D" "cmd1 OPEN_BRACKET" #(indent (select root [:#doc-text-area]))]
        ["Name search/docs" "S" "TAB" #(show-tab-help app (find-focused-text-pane app) inc)]
        ;["Go to definition" "G" "cmd1 D" #(goto-definition root (get-file-ns app) app)]
        )
      (add-menu menu-bar "REPL" "R"
        ["Evaluate here" "E" "cmd1 ENTER" #(send-selected-to-repl root app)]
        ["Evaluate entire file" "F" "cmd1 E" #(send-doc-to-repl root app)]
        ["Apply file ns" "A" "cmd1 shift A" #(apply-namespace-to-repl root app)]
        ["Clear output" "C" "cmd1 K" #(.setText (select root [:#repl-out-text-area]) "")]
        ["Restart" "R" "cmd1 R" #(restart-repl root app
                              (first (get-selected-projects root)))]
        ["Print stack trace for last error" "T" "cmd1 T" #(print-stack-trace root app)])
      (add-menu menu-bar "Search" "S"
        ["Find" "F" "cmd1 F" #(start-find app)]
        ["Find next" "N" "cmd1 G" #(highlight-step app false)]
        ["Find prev" "P" "cmd1 shift G" #(highlight-step app true)])
      (add-menu menu-bar "Window" "W"
        ["Go to REPL input" "R" "cmd1 3" #(.requestFocusInWindow (select root [:#repl-in-text-area]))]
        ["Go to Editor" "E" "cmd1 2" #(.requestFocusInWindow (select root [:#doc-text-area]))]
        ["Go to Project Tree" "P" "cmd1 1" #(.requestFocusInWindow (select root [:#docs-tree]))]
        ["Increase font size" nil "cmd1 PLUS" #(grow-font app)]
        ["Decrease font size" nil "cmd1 MINUS" #(shrink-font app)]
        ["Choose font..." nil nil #(apply show-font-window
                                          root set-font @current-font)])))
      
    
(defn add-visibility-shortcut [app]
  (let [shortcuts [(map get-keystroke ["cmd2 EQUALS" "cmd2 PLUS"])]]
    (.. Toolkit getDefaultToolkit
      (addAWTEventListener
        (proxy [AWTEventListener] []
          (eventDispatched [e]
            (when (some #{(KeyStroke/getKeyStrokeForEvent e)}
                     shortcuts)
              (.toFront (:frame app)))))
        AWTEvent/KEY_EVENT_MASK))))



;; testing

(defn get-text [current-app]
  (get-text-str (@current-app :doc-text-area)))

