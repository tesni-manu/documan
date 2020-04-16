(ns documan.utils
  (:gen-class)
  (:require [clojure.string :as str]))

;-------------------------------------------------------------------------------

(defn parse-tokens
  "Parses the given string and returns tokens.
  For example:
    def greet = say \"Hello World!\"
    returns [\"def\" \"greet\" \"=\" \"say\" \"Hello World!\"]
  Spaces and commas are treated as delimiters."
  [line]
  (let [chars (str/split line #"")
        not-blank-token? #(not (str/blank? %))
        whitespace? (fn [char is-string-token]
                      (and (not is-string-token)
                           (or (= char ",") (str/blank? char))))
        collect-token (fn [coll char]
                        (let [is-string-token (last coll)
                              tokens (pop coll)]
                          (if (whitespace? char is-string-token)
                            (conj tokens "" false)
                            (conj (pop tokens)
                                  (str (peek tokens) char)
                                  (if (= "\"" char)
                                    (not is-string-token)
                                    is-string-token)))))
        strip-double-quotes (fn [token]
                              (if (= "\"" (subs token 0 1))
                                (subs token 1 (dec (count token)))
                                token))]
    (mapv strip-double-quotes
          (filterv not-blank-token?
                   (reduce collect-token ["" false] chars)))))

;-------------------------------------------------------------------------------
