(ns documan.sequence-diagram
  (:require [clojure.string :as str]
            [dali.io :as io]
            [dali.layout.stack]
            [dali.layout.surround]))

(defn create-edn
  "Creates the diagram edn from the given source code."
  [title source]
  ; TODO
  {:type :sequence-diagram :title title})

(defn transpile-diagram
  "Transpile the given source diagram into edn format."
  [path filename title source]
  (println "Transpiling diagram... " filename)
  (let [edn (create-edn title source)]
    (clojure.pprint/pprint edn (clojure.java.io/writer (str path filename ".edn")))))

(defn create-sequence-diagram
  "Creates dali document for the sequence diagram with the given data."
  [diagram]
  (let [width 1200
        height 700
        margin 40
        padding 30
        gap 5
        body-left margin
        body-top (+ margin gap)
        body-width (- width (* 2 margin))
        body-height (- height (* 2 margin))
        title-color "#555"]
    [:dali/page {:width  width
                 :height height}
     ;_____________________________________________________
     ; title

     [:text {:id          :title
             :font-family "Sans Serif"
             :stroke      {:paint title-color
                           :width 0.3}
             :fill        title-color
             :font-size   16
             :x           margin
             :y           margin}
      (:title diagram)]

     ;_____________________________________________________
     ; bounding rectangle

     [:rect {:id     :bounding-rectangle
             :stroke {:paint :black
                      :width 0.2}
             :fill   :white}
      [body-left body-top]
      [body-width body-height]]

     ;_____________________________________________________
     ; objects

     ; TODO
     ;_____________________________________________________
     ; flow

     ; TODO
     ]))

(defn generate-diagram
  "Generates a sequence diagram with the given data in the specified path and filename."
  [path filename diagram]
  (println "Generating diagram... " filename)
  (io/render-svg
    (create-sequence-diagram diagram)
    (str path filename ".svg")))

