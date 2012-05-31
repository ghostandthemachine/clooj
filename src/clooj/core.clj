; Copyright (c) 2011, Arthur Edelstein
; All rights reserved.
; Eclipse Public License 1.0
; arthuredelstein@gmail.com

(ns clooj.core
    (:use [seesaw core graphics color border font]
          [clooj repl help utils navigate text-editor filetree menus doc-browser dev-tools indent])
    (:require [seesaw.rsyntax :as rsyntax]
              [clj-rsyntax.core :as cr])
    (:import [org.fife.ui.rtextarea RTextScrollPane]))
    

(def clooj-handlers  {:update-caret-position update-caret-position 
                      :save-caret-position save-caret-position
                      :setup-autoindent setup-autoindent
                      :switch-repl switch-repl
                      :get-selected-projects get-selected-projects
                      :apply-namespace-to-repl apply-namespace-to-repl
                      :find-file find-file})

(defn create-app []
  (let [app-init  (atom {})
        editor    (text-editor app-init)
        file-tree (file-tree app-init)
        repl      (repl app-init)
        doc-view  (doc-view app-init)
        doc-nav   (doc-nav app-init)
        doc-split-pane (left-right-split
                         file-tree
                         editor
                         :divider-location 0.25
                         :resize-weight 0.25
                         :divider-size 5)
        split-pane (left-right-split 
                        doc-split-pane 
                        repl
                        :divider-location 0.66
                        :resize-weight 0.66
                        :divider-size 5)
        frame (frame 
                :title "Clooj" 
                :width 950 
                :height 700 
                :on-close :exit
                :minimum-size [500 :by 350]
                :content split-pane)
        app (merge {:file      (atom nil)
                    :repl      (atom (create-outside-repl (@app-init :repl-out-writer) nil))
                    :changed   false}
                    @app-init
                    clooj-handlers
                    (gen-map
                      frame
                      doc-split-pane
                      split-pane))]
    app))

(defn add-behaviors
  [app]
    ;; docs
    (setup-completion-list (app :completion-list) app)    
    (setup-tab-help app (app :doc-text-area))
    ;;editor
    (setup-autoindent (app :doc-text-area))
    (doto (app :doc-text-area) attach-navigation-keys)
    (double-click-selector (app :doc-text-area))
    (add-caret-listener (app :doc-text-area) #(display-caret-position app))
    (setup-search-text-area app)
    (activate-caret-highlighter handle-caret-move app :doc-text-area)
    (activate-caret-highlighter handle-caret-move app :repl-in-text-area)
    (setup-temp-writer app)
    (attach-action-keys (app :doc-text-area)
      ["cmd1 ENTER" #(send-selected-to-repl app)])
    ;; repl
    (setup-autoindent (app :repl-in-text-area))
    (setup-tab-help app (app :repl-in-text-area))
    (add-repl-input-handler app)
    (doto (app :repl-in-text-area)
            double-click-selector
            attach-navigation-keys)
    ;; file tree
    (setup-tree app)
    ;; global
    (add-visibility-shortcut app)
    (dorun (map #(attach-global-action-keys % app)
                [(app :docs-tree) 
                 (app :doc-text-area) 
                 (app :repl-in-text-area) 
                 (app :repl-out-text-area) 
                 (.getContentPane (app :frame))])))

(defonce current-app (atom nil))

(defn -show []
  (reset! embedded true)
  (if (not @current-app)
    (startup create-app current-app)
    (.setVisible (:frame @current-app) true)))

(defn -main [& args]
  (reset! embedded false)
  (reset! current-app (create-app))
  (add-behaviors @current-app)
  (make-menus @current-app)
  (invoke-later
    (-> 
      (startup @current-app) 
      show!)))




