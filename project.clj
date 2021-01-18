;; GENERATED by script/update-project.clj, DO NOT EDIT
;; To change dependencies, update deps.edn and run script/update-project.clj.
;; To change other things, edit project.template.clj and run script/update-project.clj.

(defproject clj-kondo "2020.12.13-SNAPSHOT"
  :description "Tidy your code with clj-kondo."
  :url "https://github.com/clj-kondo/clj-kondo"
  :scm {:name "git"
        :url "https://github.com/clj-kondo/clj-kondo"}
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}
  :source-paths ["src" "parser" "inlined"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [com.cognitect/transit-clj "1.0.324"]
                 [io.lambdaforge/datalog-parser "0.1.7"]
                 [cheshire/cheshire "5.10.0"]
                 [nrepl/bencode "1.1.0"]
                 [borkdude/sci "0.2.1"]]
  :profiles {:clojure-1.9.0 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :clojure-1.10.2-alpha1 {:dependencies [[org.clojure/clojure "1.10.2-alpha1"]]}
             :native-image {:dependencies [[borkdude/clj-reflector-graal-java11-fix "0.0.1-graalvm-20.3.0"]
                                           [borkdude/sci.impl.reflector "0.0.1-java11"]]}
             :test {:dependencies [[org.clojure/clojurescript "1.10.520"]
                                   [clj-commons/conch "0.9.2"]
                                   [jonase/eastwood "0.3.11"]
                                   [borkdude/missing.test.assertions "0.0.2"]]
                    :source-paths ["src" "parser" "inlined" "extract"]}
             :uberjar {:global-vars {*assert* false}
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.skip-macros=true"]
                       :main clj-kondo.main
                       :aot :all}}
  :aliases {"clj-kondo" ["run" "-m" "clj-kondo.main"]}
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])
