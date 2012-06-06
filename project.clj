(defproject clooj "0.3.4.2-SNAPSHOT"
  :description "the start of a seesaw version of clooj, a small IDE for clojure"
  :dependencies [[clojure "1.3.0"]
                 [clj-inspector "0.0.12"]
                 [seesaw "1.4.1" :exclusions [com.miglayout/miglayout
 							                 com.jgoodies/forms
 						                     org.swinglabs.swingx/swingx-core]]
                 [org.fife.ui/rsyntaxtextarea "2.0.3"]
                 [com.cemerick/pomegranate "0.0.11"]]
  :jvm-opts ~(if (= (System/getProperty "os.name") "Mac OS X") ["-Xdock:name=Clooj"] [])
  :main clooj.core)
