(ns documan.core
  (:gen-class)
  (:require [clojure.string :as str]
            [documan.sequence-diagram :as seq]))

(defn get-files-with-extension [path extension]
  (->> path
       clojure.java.io/file
       file-seq
       (filter #(.isFile %))
       (filter #(str/ends-with? (.getName %) extension))))

(defn generate-document
  "Generate document and save it in the specified path and filename"
  [path filename document-data]
  (let [type (:type document-data)]
    (case
      ; TODO: add more types here
      :sequence-diagram (seq/generate-diagram
                          (str path "/sequence-diagrams/")
                          filename document-data))))

(defn generate-all-documents [project-path]
  "Generates documents for all clj files in the project."
  (println "Generating all in:" project-path)
  (doseq [file (get-files-with-extension (str project-path "/clj/") ".clj")]
    (generate-document
      (str project-path "/dst/")
      (str/replace (.getName file) ".clj" "")
      (read-string (slurp (.getAbsolutePath file))))))

(defn -main
  "Generates documents for the specified project."
  [& args]
  (let [project-path (first args)
        flags (vec (rest args))
        flag-contains? (fn [flags val]
                         (or
                           (empty? flags)
                           (contains? flags val)))]
    (if (empty? project-path)
      (println "Please specify the project path as first argument.")
      (do
        ; TODO: add transpiling step here
        (if (flag-contains? flags "-g")
          (generate-all-documents project-path))))))