(defproject vm-translator "0.1.0-SNAPSHOT"
  :description "VM Translator for Nand2Tetris II"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[me.raynes/fs "1.4.6"]
                 [org.clojure/clojure "1.8.0"]
                 [superstring "2.1.0"]]
  :main ^:skip-aot vm-translator.core
  :target-path "target/%s"
  :resource-paths ["resources"]
  :profiles {:dev {:dependencies [[zprint "0.2.16"]]
                   :resource-paths ["test-resources"]}
             :uberjar {:aot :all}})
