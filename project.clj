(defproject latte "0.0.1-SNAPSHOT"
  :description "lumber: all things Clojure about trees"
  :url "https://github.com/fredokun/lumber.git"
  :license {:name "MIT Licence"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [clj-by-example "0.1.0"]]
  :codox {:output-path "docs"
          :metadata {:doc/format :markdown}
          :namespaces []}
  :plugins [[lein-codox "0.10.0"]])

