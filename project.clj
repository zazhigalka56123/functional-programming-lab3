(defproject interpolation "0.1.0-SNAPSHOT"
  :description "Streaming interpolation with linear, Newton, and Lagrange algorithms"
  :url "https://github.com/example/interpolation"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.1.230"]]
  :main interpolation.core
  :plugins [[lein-cljfmt "0.8.2"]]
  :aliases {"lint" ["cljfmt" "check"]
            "format" ["cljfmt" "fix"]})
