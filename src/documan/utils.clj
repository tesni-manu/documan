(ns documan.utils
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

;-----------------------------------------------------------------------------------------------------------------------

(defn load-config
  "Loads the config.json file in the given path and return its contents as a map"
  [path]
  (if (.exists (io/file (str path "/config.json")))
    (let [config (slurp (str path "/config.json"))]
      (json/read-str config :key-fn keyword))
    {}))

;-----------------------------------------------------------------------------------------------------------------------

(defn parse-tokens
  "Parses the given string and returns tokens.
  For example:
    def greet = say \"Hello World!\"
    returns [\"def\" \"greet\" \"=\" \"say\" \"Hello World!\"]
  Spaces and commas are treated as delimiters."
  [line]
  (let [chars (str/split line #"")
        not-blank-token? #(not (str/blank? %))
        whitespace? (fn [char is-string-token] (and (not is-string-token) (or (= char ",") (str/blank? char))))
        collect-token (fn [coll char]
                        (let [is-string-token (last coll), tokens (pop coll)]
                          (if (whitespace? char is-string-token)
                            (conj tokens "" false)
                            (conj (pop tokens)
                                  (str (peek tokens) char)
                                  (if (= "\"" char) (not is-string-token) is-string-token)))))
        strip-double-quotes (fn [token] (if (= "\"" (subs token 0 1)) (subs token 1 (dec (count token))) token))]
    (->> chars
         (reduce collect-token ["" false])
         (filterv not-blank-token?)
         (mapv strip-double-quotes))))

;-----------------------------------------------------------------------------------------------------------------------

(defn get-files-only-given-extension
  "Returns the files in the specified path with the given extension"
  [path extension]
  (->> path
       io/file
       file-seq
       (filterv #(.isFile %))
       (filterv #(str/ends-with? (.getName %) extension))))

;-----------------------------------------------------------------------------------------------------------------------

(defn strip-comments
  "Removes empty lines, comment lines and end-of-line comments from the given list of lines"
  [lines]
  (let [is-not-empty-line? #(not= "" (str/trim %))
        is-not-comment-line? #(not (str/starts-with? (str/trim %) "//"))
        strip-line-comments #(first (str/split % #"//"))]
    (->> lines
         (filterv is-not-empty-line?)
         (filterv is-not-comment-line?)
         (mapv strip-line-comments))))

;-----------------------------------------------------------------------------------------------------------------------

(defn map->vec [m] (vec (flatten (into (vector) m))))
(defn ceil [n] (int (Math/ceil n)))

;-----------------------------------------------------------------------------------------------------------------------
