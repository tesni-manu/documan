(ns documan.core
  (:gen-class)
  (:require [documan.utils :as utils]
            [clojure.string :as str]
            [documan.sequence-diagram :as seq]))

;-------------------------------------------------------------------------------

(defn get-files-only-given-extension [path extension]
  (->> path
       clojure.java.io/file
       file-seq
       (filterv #(.isFile %))
       (filterv #(str/ends-with? (.getName %) extension))))

;-------------------------------------------------------------------------------

(defn src->edn
  "Transpile the given source file and save it as edn in the specified path."
  [path filename document-data]
  (let [lines (str/split-lines document-data)
        tokens (utils/parse-tokens (first lines))
        type (keyword (str/lower-case (first tokens)))
        version (second tokens)
        title (last tokens)
        strip-comments (fn [lines]
                         (let [is-not-empty-line? #(not= "" (str/trim %))
                               is-not-comment-line? #(not (str/starts-with?
                                                            (str/trim %) "//"))
                               strip-line-comments #(first (str/split % #"//"))]
                           (->> lines
                                (filterv is-not-empty-line?)
                                (filterv is-not-comment-line?)
                                (mapv strip-line-comments))))
        other-lines (strip-comments (rest lines))]
    (case type
      ; TODO: add more types here
      :sequence-diagram (seq/src->edn (str path "/sequence-diagrams/")
                                      filename
                                      version
                                      title
                                      other-lines))))

(defn all-src->edn [project-path]
  "Transpile edn files for all source code files in the project."
  (println "Transpiling all in:" (str project-path "/src/"))
  (doseq [file (get-files-only-given-extension
                 (str project-path "/src/") ".documan")]
    (src->edn
      (str project-path "/edn/")
      (str/replace (.getName file) ".documan" "")
      (slurp (.getAbsolutePath file)))))

;-------------------------------------------------------------------------------

(defn edn->document
  "Generate document and save it in the specified path and filename."
  [path filename document-data]
  (let [type (:type document-data)]
    (case type
      ; TODO: add more types here
      :sequence-diagram (seq/edn->document
                          (str path "/sequence-diagrams/")
                          filename document-data))))

(defn all-edn->document [project-path]
  "Generates documents for all edn files in the project."
  (println "Generating all in:" (str project-path "/edn/"))
  (doseq [file (get-files-only-given-extension
                 (str project-path "/edn/") ".edn")]
    (edn->document
      (str project-path "/dst/")
      (str/replace (.getName file) ".edn" "")
      (read-string (slurp (.getAbsolutePath file))))))

;-------------------------------------------------------------------------------

(defn -main
  "Generates documents for the specified project."
  [& args]
  (let [project-path (first args)
        flags (vec (rest args))
        flag-contains? (fn [flags flag]
                         (or
                           (empty? flags)
                           (not (zero? (count (filter #(= % flag) flags))))))]
    (if (empty? project-path)
      (println "Please specify the project path as first argument.")
      (do
        (if (flag-contains? flags "-t")
          (all-src->edn project-path))
        (if (flag-contains? flags "-g")
          (all-edn->document project-path))))))

;-------------------------------------------------------------------------------