(defproject documan "0.1.0-SNAPSHOT"
  :description "Tool to generate documents and diagrams."
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [dali "0.7.4"]
                 [org.clojure/data.json "1.0.0"]]
  :main documan.core
  :aot [documan.core]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
