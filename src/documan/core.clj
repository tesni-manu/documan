(ns documan.core
  (:gen-class)
  (:require [clojure.string :as str]
            [documan.sequence-diagram :as seq]))

(defn get-files-only-given-extension [path extension]
  (->> path
       clojure.java.io/file
       file-seq
       (filter #(.isFile %))
       (filter #(str/ends-with? (.getName %) extension))))

(defn transpile-document
  "Transpile the given source file and save it as edn in the specified path and filename."
  [path filename document-data]
  (let [lines (str/split-lines document-data)
        first-line (first lines)
        type (read-string first-line)
        title (str/join " " (rest (str/split first-line #" ")))
        other-lines (rest lines)]
    (case type
      ; TODO: add more types here
      :sequence-diagram (seq/transpile-diagram
                          (str path "/sequence-diagrams/")
                          filename
                          title
                          other-lines))))

(defn transpile-all-documents [project-path]
  "Transpile edn files for all source code files in the project."
  (println "Transpiling all in:" (str project-path "/src/"))
  (doseq [file (get-files-only-given-extension (str project-path "/src/") ".documan")]
    (transpile-document
      (str project-path "/edn/")
      (str/replace (.getName file) ".documan" "")
      (slurp (.getAbsolutePath file)))))

(defn generate-document
  "Generate document and save it in the specified path and filename."
  [path filename document-data]
  (let [type (:type document-data)]
    (case type
      ; TODO: add more types here
      :sequence-diagram (seq/generate-diagram
                          (str path "/sequence-diagrams/")
                          filename document-data))))

(defn generate-all-documents [project-path]
  "Generates documents for all edn files in the project."
  (println "Generating all in:" (str project-path "/edn/"))
  (doseq [file (get-files-only-given-extension (str project-path "/edn/") ".edn")]
    (generate-document
      (str project-path "/dst/")
      (str/replace (.getName file) ".edn" "")
      (read-string (slurp (.getAbsolutePath file))))))

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
          (transpile-all-documents project-path))
        (if (flag-contains? flags "-g")
          (generate-all-documents project-path))))))