(defproject clooj "0.3.4.1-SNAPSHOT"
  :description "the start of a seesaw version of clooj, a small IDE for clojure"
  :main clooj.core
  :dependencies [[clojure "1.3.0"]
                 [clj-inspector "0.0.12"]
                 [slamhound "1.2.0"]
                 [com.cemerick/pomegranate "0.0.11"]
                 [com.fifesoft/rsyntaxtextarea "2.0.2"]
                 [ghostandthemachine/seesaw "1.4.2-SNAPSHOT"]
                 [clj-rsyntax "0.0.1-SNAPSHOT"]
                 [org.fife.ui/rsyntaxtextarea "2.0.3"]]
  :jvm-opts ~(if (= (System/getProperty "os.name") "Mac OS X") ["-Xdock:name=Clooj"] []))
