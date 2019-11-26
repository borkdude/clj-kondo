(ns clj-kondo.impl.core
  "Implementation details of clj-kondo.core"
  {:no-doc true}
  (:require
   [clj-kondo.impl.analyzer :as ana]
   [clj-kondo.impl.config :as config]
   [clj-kondo.impl.utils :refer [one-of print-err! map-vals assoc-some]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import [java.util.jar JarFile JarFile$JarFileEntry]))

(set! *warn-on-reflection* true)

(def dev? (= "true" (System/getenv "CLJ_KONDO_DEV")))

(def version
  (str/trim
   (slurp (io/resource "CLJ_KONDO_VERSION"))))

(defn format-output [config]
  (if-let [^String pattern (-> config :output :pattern)]
    (fn [filename row col level message]
      (-> pattern
          (str/replace "{{filename}}" filename)
          (str/replace "{{row}}" (str row))
          (str/replace "{{col}}" (str col))
          (str/replace "{{level}}" (name level))
          (str/replace "{{LEVEL}}" (str/upper-case (name level)))
          (str/replace "{{message}}" message)))
    (fn [filename row col level message]
      (str filename ":" row ":" col ": " (name level) ": " message))))

;;;; process config

(declare read-edn-file)

(defn opts [^java.io.File cfg-file]
  {:readers
   {'include
    (fn [file]
      (let [f (io/file (.getParent cfg-file) file)]
        (if (.exists f)
          (read-edn-file f)
          (binding [*out* *err*]
            (println "WARNING: included file" (.getCanonicalPath f) "does not exist.")))))}})

(defn read-edn-file [^java.io.File f]
  (try (edn/read-string (opts f) (slurp f))
       (catch Exception e
         (binding [*out* *err*]
           (println "WARNING: error while reading"
                    (.getCanonicalPath f) (format "(%s)" (.getMessage e)))))))

(defn resolve-config [cfg-dir config]
  (reduce config/merge-config! config/default-config
          [(when cfg-dir
             (let [f (io/file cfg-dir "config.edn")]
               (when (.exists f)
                 (read-edn-file f))))
           (when config
             (cond (map? config) config
                   (or (str/starts-with? config "{")
                       (str/starts-with? config "^"))
                   (edn/read-string config)
                   ;; config is a string that represents a file
                   :else (read-edn-file config)))]))

;;;; process cache

(def empty-cache-opt-warning "WARNING: --cache option didn't specify directory, but no .clj-kondo directory found. Continuing without cache. See https://github.com/borkdude/clj-kondo/blob/master/README.md#project-setup.")

(defn resolve-cache-dir [cfg-dir cache cache-dir]
  (when-let [cache-dir (or cache-dir
                           ;; for backward compatibility
                           (when-not (true? cache)
                             cache)
                           (when cfg-dir (io/file cfg-dir ".cache")))]
    (io/file cache-dir version)))

;;;; find cache/config dir

(defn- lineage [^java.io.File file]
  (lazy-seq
    (when file
      (cons file (lineage (.getParentFile file))))))

(defn source-file? [filename]
  (when-let [[_ ext] (re-find #"\.(\w+)$" filename)]
    (one-of (keyword ext) [:clj :cljs :cljc :edn])))

(defn- single-file-lint? [lint]
  (and (= 1 (count lint))
       (.isFile (io/file (first lint)))
       (source-file? (str (first lint)))))

(defn- possible-config-dir-locations [lint]
  (concat
    (when (single-file-lint? lint)
      (lineage (.getParentFile (io/file (first lint)))))
    (lineage (io/file (System/getProperty "user.dir")))))

(defn config-dir [lint]
  (transduce (comp (map #(io/file % ".clj-kondo"))
                   (filter #(.exists ^java.io.File %)))
             (fn
               ([] nil)
               ([result] result)
               ([_ ^java.io.File cfg-dir]
                (if (.isDirectory cfg-dir)
                  (reduced cfg-dir)
                  (throw (Exception. (str cfg-dir " must be a directory"))))))
             (possible-config-dir-locations lint)))

;;;; jar processing

(defn sources-from-jar
  [^java.io.File jar-file canonical?]
  (with-open [jar (JarFile. jar-file)]
    (let [entries (enumeration-seq (.entries jar))
          entries (filter (fn [^JarFile$JarFileEntry x]
                            (let [nm (.getName x)]
                              (and (not (.isDirectory x)) (source-file? nm)))) entries)]
      ;; Important that we close the `JarFile` so this has to be strict see GH
      ;; issue #542. Maybe it makes sense to refactor loading source using
      ;; transducers so we don't have to load the entire source of a jar file in
      ;; memory at once?
      (mapv (fn [^JarFile$JarFileEntry entry]
              {:filename (str (when canonical?
                                (str (.getCanonicalPath jar-file) ":"))
                              (.getName entry))
               :source (slurp (.getInputStream jar entry))}) entries))))

;;;; dir processing

(defn sources-from-dir
  [dir canonical?]
  (let [files (file-seq dir)]
    (keep (fn [^java.io.File file]
            (let [nm (if canonical?
                       (.getCanonicalPath file)
                       (.getPath file))
                  can-read? (.canRead file)
                  source? (and (.isFile file) (source-file? nm))]
              (cond
                (and can-read? source?)
                {:filename nm
                 :source (slurp file)}
                (and (not can-read?) source?)
                (print-err! (str nm ":0:0:") "warning: can't read, check file permissions")
                :else nil)))
          files)))

;;;; file processing

(defn lang-from-file [file default-language]
  (if-let [[_ ext] (re-find #"\.(\w+)$" file)]
    (let [k (keyword ext)]
      (or (get #{:clj :cljs :cljc :edn} k)
          default-language))
    default-language))

(def path-separator (System/getProperty "path.separator"))

(defn classpath? [f]
  (str/includes? f path-separator))

(defn process-file [ctx filename default-language canonical?]
  (try
    (let [file (io/file filename)]
      (cond
        (.exists file)
        (if (.isFile file)
          (if (str/ends-with? (.getPath file) ".jar")
            ;; process jar file
            (map #(ana/analyze-input ctx (:filename %) (:source %)
                                     (lang-from-file (:filename %) default-language)
                                     dev?)
                 (sources-from-jar file canonical?))
            ;; assume normal source file
            [(ana/analyze-input ctx (if canonical?
                                      (.getCanonicalPath file)
                                      filename) (slurp file)
                                (lang-from-file filename default-language)
                                dev?)])
          ;; assume directory
          (map #(ana/analyze-input ctx (:filename %) (:source %)
                                   (lang-from-file (:filename %) default-language)
                                   dev?)
               (sources-from-dir file canonical?)))
        (= "-" filename)
        [(ana/analyze-input ctx "<stdin>" (slurp *in*) default-language dev?)]
        (classpath? filename)
        (mapcat #(process-file ctx % default-language canonical?)
                (str/split filename
                           (re-pattern path-separator)))
        :else
        [{:findings [{:level :warning
                      :filename (if canonical?
                                  (.getCanonicalPath file)
                                  filename)
                      :type :file
                      :col 0
                      :row 0
                      :message "file does not exist"}]}]))
    (catch Throwable e
      (if dev? (throw e)
          [{:findings [{:level :warning
                        :filename (if canonical?
                                    (.getCanonicalPath (io/file filename))
                                    filename)
                        :type :file
                        :col 0
                        :row 0
                        :message "could not process file"}]}]))))

(defn process-files [ctx files default-lang]
  (let [canonical? (-> ctx :config :output :canonical-paths)]
    (mapcat #(process-file ctx % default-lang canonical?) files)))

;;;; index defs and calls by language and namespace

(defn mmerge
  "Merges maps no deeper than two levels"
  [a b]
  (merge-with merge a b))

(defn format-vars [vars]
  (map-vals (fn [meta]
              (-> meta
                  (select-keys [:row :col
                                :macro :private :deprecated
                                :fixed-arities :varargs-min-arity
                                :name :ns :top-ns :imported-ns :imported-var
                                :arities])))
            vars))

(defn namespaces->indexed [namespaces]
  (when namespaces
    (map-vals (fn [{:keys [:vars :proxied-namespaces]}]
                (assoc-some (format-vars vars)
                            :proxied-namespaces proxied-namespaces))
              namespaces)))

(defn namespaces->indexed-cljc [namespaces lang]
  (when namespaces
    (map-vals (fn [v]
                (let [vars (:vars v)]
                  {lang (format-vars vars)}))
              namespaces)))

(defn namespaces->indexed-defs [ctx]
  (let [namespaces @(:namespaces ctx)
        clj (namespaces->indexed (get-in namespaces [:clj :clj]))
        cljs (namespaces->indexed (get-in namespaces [:cljs :cljs]))
        cljc-clj (namespaces->indexed-cljc (get-in namespaces [:cljc :clj])
                                           :clj)
        cljc-cljs (namespaces->indexed-cljc (get-in namespaces [:cljc :cljs])
                                            :cljs)]
    {:clj {:defs clj}
     :cljs {:defs cljs}
     :cljc {:defs (mmerge cljc-clj cljc-cljs)}}))

(defn index-defs-and-calls [ctx defs-and-calls]
  ;; (prn ">" defs-and-calls)
  (let [indexed-defs (namespaces->indexed-defs ctx)]
    (reduce
     (fn [acc {:keys [:used-namespaces :lang] :as _m}]
       (-> acc
           (update-in [lang :used-namespaces] into used-namespaces)))
     indexed-defs
     defs-and-calls)))

;;;; summary

(def zinc (fnil inc 0))

(defn summarize [findings]
  (reduce (fn [acc {:keys [:level]}]
            (update acc level zinc))
          {:error 0 :warning 0 :info 0 :type :summary}
          findings))

;;;; filter/remove output

(defn filter-findings [config findings]
  (let [print-debug? (:debug config)
        filter-output (not-empty (-> config :output :include-files))
        remove-output (not-empty (-> config :output :exclude-files))]
    (for [{:keys [:filename :type] :as f} findings
          :let [level (when type (-> config :linters type :level))
                ;; _ (when-not level (println "warning: " type " has no level!"))
                ]
          :when (and level (not= :off level))
          :when (if (= :debug type)
                  print-debug?
                  true)
          :when (if filter-output
                  (some (fn [pattern]
                          (re-find (re-pattern pattern) filename))
                        filter-output)
                  true)
          :when (not-any? (fn [pattern]
                            (re-find (re-pattern pattern) filename))
                          remove-output)]
      (assoc f :level level))))

;;;; Scratch

(comment
  )
