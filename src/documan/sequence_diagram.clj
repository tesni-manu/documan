(ns documan.sequence-diagram
  (:require [dali.io :as io]))

(defn create-sequence-diagram [diagram]
  "Creates dali document for the sequence diagram with the given data"
  [:dali/page
   [:circle
    {:stroke :indigo :stroke-width 4 :fill :darkorange}
    [30 30] 20]])

(defn generate-diagram
  "Generates a sequence diagram with the given data in the specified path and filename"
  [path filename diagram]
  (io/render-svg
    (create-sequence-diagram diagram)
    (str path filename ".svg")))

