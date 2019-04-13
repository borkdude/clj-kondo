(ns clj-kondo.impl.ExtractJava
  {:no-doc true}
  (:gen-class
   :methods [#^{:static true} [start [com.sun.javadoc.RootDoc] boolean]])
  (:require
   [clojure.java.io :as io]
   [cognitect.transit :as transit])
  (:import [javax.tools ToolProvider DocumentationTool]))

(set! *warn-on-reflection* true)

(def extracted (atom nil))

(defn -start [^com.sun.javadoc.RootDoc root]
  (reset! extracted
          (vec (for [^com.sun.javadoc.ClassDoc c (.classes root)
                     :when (contains? #{"Thread" "System" "Integer" "String"} (.name c))
                     ^com.sun.javadoc.MethodDoc m (.methods c)
                     :when (.isStatic m)]
                 {:class (.qualifiedName c)
                  :method (.name m)
                  :arity (count (.parameters m))})))
  true)

(def sconj (fnil conj #{}))

(defn -main []
  (println "JAVA_HOME:" (System/getProperty "java.home"))
  (println "Extracting Java...")
  (let [dt (ToolProvider/getSystemDocumentationTool)]
    (.run dt nil nil nil
          (into-array ["-doclet" "clj_kondo.impl.ExtractJava"
                       "-public"
                       "--source-path" "/Users/Borkdude/git/jdk/src/java.base/share/classes"
                       "java.lang"
                       #_#_"--source-path" "/tmp/"
                       #_"my.pack"])))
  (let [extracted-java
        (reduce (fn [acc entry]
                  (let [ns (:class entry)
                        def (:method entry)
                        qdef (symbol ns def)]
                    (update acc (symbol ns)
                            #(-> %
                                 (assoc-in [qdef :qname] qdef)
                                 (update-in [qdef :fixed-arities] sconj (:arity entry))))))
                {}
                @extracted)]
      (println "Writing out built-in cache...")
      (doseq [[ns v] extracted-java]
        (let [file (io/file (str "resources/clj_kondo/impl/cache/built_in/clj/" ns ".transit.json"))]
          (io/make-parents file)
          (let [bos (java.io.ByteArrayOutputStream. 1024)
                writer (transit/writer (io/output-stream bos) :json)]
            (transit/write writer v)
            (io/copy (.toByteArray bos) file))))))

;;;; Scratch

(comment
  )
