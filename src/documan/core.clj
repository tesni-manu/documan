(ns documan.core
  (:gen-class)
  (:require [documan.utils :as du]
            [clojure.string :as str]
            [documan.sequence-diagram :as ds]))

;-----------------------------------------------------------------------------------------------------------------------

(defn src->edn
  "Transpile the given source file and save it as edn in the specified path."
  [path filename document-data config]
  (let [lines (str/split-lines document-data)
        tokens (du/parse-tokens (first lines))
        type (keyword (str/lower-case (first tokens)))
        version (second tokens)
        title (last tokens)
        other-lines (du/strip-comments (rest lines))]
    (case type
      ; TODO: add more types here
      :sequence-diagram (ds/src->edn (str path "/sequence-diagrams/") filename version title other-lines config))))

(defn all-src->edn
  "Transpile edn files for all source code files in the project."
  [project-path config]
  (println "Transpiling all in:" (str project-path "/src/"))
  (doseq [file (du/get-files-only-given-extension (str project-path "/src/") ".documan")]
    (src->edn (str project-path "/edn/")
              (str/replace (.getName file) ".documan" "")
              (slurp (.getAbsolutePath file))
              config)))

;-----------------------------------------------------------------------------------------------------------------------

(defn edn->document
  "Generate document and save it in the specified path and filename."
  [path filename document-data config]
  (let [type (:type document-data)]
    (case type
      ; TODO: add more types here
      :sequence-diagram (ds/edn->document (str path "/sequence-diagrams/") filename document-data config))))

(defn all-edn->document
  "Generates documents for all edn files in the project."
  [project-path config]
  (println "Generating all in:" (str project-path "/edn/"))
  (doseq [file (du/get-files-only-given-extension
                 (str project-path "/edn/") ".edn")]
    (edn->document (str project-path "/dst/")
                   (str/replace (.getName file) ".edn" "")
                   (read-string (slurp (.getAbsolutePath file)))
                   config)))

;-----------------------------------------------------------------------------------------------------------------------

(defn -main
  "Generates documents for the specified project."
  [& args]
  (let [project-path (first args)
        flags (vec (rest args))
        flag-contains? (fn [flags flag] (or (empty? flags) (not (zero? (count (filter #(= % flag) flags))))))]
    (if (empty? project-path)
      (println "Please specify the project path as first argument.")
      (do
        (let [config (du/load-config project-path)]
          (if (flag-contains? flags "-t") (all-src->edn project-path config))
          (if (flag-contains? flags "-g") (all-edn->document project-path config)))))))

;-----------------------------------------------------------------------------------------------------------------------
