; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.core
 (:use  [seesaw.core :exclude [text-area-options]]
        [seesaw.graphics]
        [seesaw.color]
        [clojure.pprint :only (pprint)]
        [clooj.dev-tools]
        [clooj.brackets]
        [clooj.highlighting]
        [clooj.repl]
        [clooj.search]
        [clooj.help]
        [clooj.project]
        [clooj.utils]
        [clooj.indent]
        [clooj.style]
        [clooj.navigate]
        [clooj.rsyntax])
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
           (org.fife.ui.rtextarea RTextScrollPane)))

(def app (atom nil))

(defn make-tabs [doc-tree completion]
  (tabbed-panel
    :tabs [{ :title "Docs" :content doc-tree }
           { :title "Completion"  :content completion}]))

        
(defn create-app []
  (let [
        arglist-label (label
                        :id :arglist-label)
        
        search-text-area (text 
                            :size [200 :by 12]
                            :id :search-text-area)
        pos-label (label 
                    :font (font "COURIER" 13)
                    :id :pos-label)
        position-search-panel (border-panel 
                                :west pos-label 
                                :center search-text-area
                                :hgap 15
                                :id :position-search-panel)

        doc-label (label 
                    :text "Source Editor"
                    :id :doc-label)        
        doc-text-area (text-area 
                            :wrap-lines? false
                            :id :doc-text-area)
        doc-scroll-pane (make-scroll-pane doc-text-area)
        doc-text-panel (vertical-panel :items [doc-label doc-scroll-pane position-search-panel])
        
        help-text-area (text-area
                            :id :help-text-area
                            :wrap-lines? true
                            :editable? false
                            :background (color 0xFF 0xFF 0xE8))

        help-text-scroll-pane (scrollable help-text-area)

        completion-label (label
                            :text "Name search"
                            :id :completion-label)
        
        completion-list (listbox 
                            :id :completion-list)

        completion-scroll-pane (scrollable completion-list)
        completion-panel (vertical-panel 
                            :id :completion-panel
                            :items [completion-label completion-scroll-pane])

        docs-tree (tree
                      :id :docs-tree
                      :model (DefaultTreeModel. nil))

        docs-tree-scroll-pane (scrollable docs-tree)

        docs-tree-label (border-panel 
                            :west (label "Projects")
                            :size [200 :by 15]
                            :vgap 5)

        docs-tree-panel (vertical-panel 
                            :items [docs-tree-label 
                                    docs-tree-scroll-pane])

        navigation-tab-panel (make-tabs docs-tree-panel completion-panel)

        doc-split-pane (left-right-split
                            navigation-tab-panel
                            doc-text-panel
                            :divider-location 0.2)

        repl-out-text-area (text-area 
                                :wrap-lines? false
                                :id :repl-out-text-area
                                :editable? false)

        repl-out-writer (make-repl-writer repl-out-text-area)
        
        repl-out-scroll-pane (scrollable repl-out-text-area)
        repl-output-vertical-panel (vertical-panel 
                                        :items [repl-out-scroll-pane])

        repl-in-text-area (text-area 
                                :wrap-lines? false
                                :id :repl-in-text-area)

        repl-input-vertical-panel (vertical-panel :items [repl-in-text-area])

        repl-split-pane (top-bottom-split 
                            repl-output-vertical-panel 
                            repl-input-vertical-panel
                            :divider-location 0.7)
                
        split-pane (top-bottom-split 
                        doc-split-pane 
                        repl-split-pane 
                        :divider-location 0.7)

        root (frame 
                :title "Sketchpad" 
                :width 950 
                :height 700 
                :on-close :exit
                :minimum-size [500 :by 350]
                :content split-pane)


        ]


    (reset! app {:file (atom nil)
                       :repl (atom (create-outside-repl repl-out-writer nil))
                       :changed false})

    (doto doc-text-area
      attach-navigation-keys)
    
    (setup-completion-list completion-list app root)

    
    (double-click-selector doc-text-area)
    
    (doto repl-in-text-area
      double-click-selector
      attach-navigation-keys)

    (.setSyntaxEditingStyle repl-in-text-area
                            SyntaxConstants/SYNTAX_STYLE_CLOJURE)

    (setup-search-text-area root)
    
    (add-caret-listener doc-text-area #(display-caret-position root))
    
    (activate-caret-highlighter root app)
    
    (attach-action-keys doc-text-area
      ["cmd1 ENTER" #(send-selected-to-repl root app)])
    
    (dorun (map #(attach-global-action-keys % app)
                [docs-tree doc-text-area repl-in-text-area repl-out-text-area (.getContentPane root)]))
    
    (setup-autoindent doc-text-area)
    root))

(defonce current-app (atom nil))
 
(defn add-behaviors [root app]
  (Thread/setDefaultUncaughtExceptionHandler
    (proxy [Thread$UncaughtExceptionHandler] []
      (uncaughtException [thread exception]
                       (println thread) (.printStackTrace exception))))

  (native!)
  (reset! current-app @app)
  (make-menus root current-app)
  (add-visibility-shortcut current-app)
  (setup-autoindent (select root [:#repl-in-text-area]))

  
  (add-repl-input-handler root current-app)

  (setup-tab-help root (select root [:#repl-in-text-area]))

  (doall (map #(add-project current-app %) (load-project-set)))
  (let [frame root]
    (persist-window-shape clooj-prefs "main-window" frame) 
    (.setVisible frame true)
    (on-window-activation frame #(update-project-tree (select root [:#docs-tree]))))
  (setup-temp-writer root current-app)
  (setup-tree root current-app)
  (let [tree (select root [:#docs-tree])]
    (load-expanded-paths tree)
    (load-tree-selection tree))
  (load-font root))

(defn -show []
  (reset! embedded true)
  (if (not @current-app)
    (add-behaviors create-app current-app)
    (.setVisible (:frame @current-app) true)))

(defn -main [& args]
  (invoke-later (show! (add-behaviors (create-app) app))))

