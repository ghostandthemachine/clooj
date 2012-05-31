; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.dev-tools
  
  (:use [clooj filetree utils help]
        [seesaw core font])
  (:import (java.net URL)
           (org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants TokenMakerFactory Theme)
           (java.awt Toolkit AWTEvent)
           (java.awt.event WindowAdapter AWTEventListener)
           (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree KeyStroke SpringLayout JTextPane
                        ListSelectionModel
                        UIManager))
  (:require [clojure.java.io :as io]))

(def gap 5)

(def embedded (atom false))

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

(def default-clooj-font
  (cond (is-mac) ["Monaco" 12]
        (is-win) ["Courier New" 13]
        :else    ["Monospaced" 13]))

(defn set-font
  ([app font-name size]
    (let [f (font font-name size)]
      (awt-event
        (write-value-to-prefs clooj-prefs "app-font"
                              [font-name size])
        (dorun (map #(.setFont (app %) f)
                    [:doc-text-area :repl-in-text-area
                     :repl-out-text-area :arglist-label
                     :search-text-area :help-text-area
                     :completion-list]))
        (reset! current-font [font-name size]))))
  ([app font-name]
    (let [size (second @current-font)]
      (set-font app font-name size))))

(defn load-font [app]
   (apply set-font app (or (read-value-from-prefs clooj-prefs "app-font")
                     default-clooj-font)))
  
(defn resize-font [app fun]
  (let [[name size] @current-font]
    (set-font app name (fun size))))

(defn grow-font [app] (resize-font app inc))

(defn shrink-font [app] (resize-font app dec))

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

(defn goto-definition [ns app handlers]
  (let [text-comp (:doc-text-area app)
        pos (.getCaretPosition text-comp)
        text (.getText text-comp)
        src-file (:file (meta (do 
                                            (token-from-caret-pos ns text pos) nil)))
        line (:line (meta (do (find-ns (symbol ns))
                                        (token-from-caret-pos ns text pos) nil)))
        project-path (first (get-selected-projects app))
        file ((handlers :find-file) project-path src-file)]
    (when (and file line)
      (when (not= file @(:file app))
        (restart-doc app file handlers)
        (set-tree-selection (:docs-tree app) (.getAbsolutePath file)))
      (scroll-to-line text-comp line))))
    
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

;; startup
(defn startup [app]
  (Thread/setDefaultUncaughtExceptionHandler
    (proxy [Thread$UncaughtExceptionHandler] []
      (uncaughtException [thread exception]
                       (println thread) (.printStackTrace exception))))

  (doall (map #(add-project app %) (load-project-set)))
  (let [frame (app :frame)]
    (persist-window-shape clooj-prefs "main-window" frame) 
    (on-window-activation frame #(update-project-tree (app :docs-tree))))
  (load-font app)
  ;; set theme
  (let [doc-ta (app :doc-text-area)
      repl-in-ta (app :repl-in-text-area)
      repl-out-ta (app :repl-out-text-area)
      theme (Theme/load (io/input-stream "src/clooj/themes/eclipse.xml"))]
      (.apply theme doc-ta)
      (.apply theme repl-in-ta)
      (.apply theme repl-out-ta))
  (let [tree (app :docs-tree)]
    (load-expanded-paths tree)
    (load-tree-selection tree))
    (app :frame))

;; testing
(defn get-text [current-app]
  (get-text-str (@current-app :doc-text-area)))

