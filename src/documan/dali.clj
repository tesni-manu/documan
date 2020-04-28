(ns documan.dali
  (:gen-class)
  (:require [documan.utils :as du]
            [clojure.string :as str]
            [dali.io :as io]
            [dali
             [prefab :as prefab]
             [syntax :as d]]
            [dali.layout.stack :as stack]
            [dali.layout.align :as align]
            [dali.layout.surround :as surround]))

;-----------------------------------------------------------------------------------------------------------------------

(defn group [& args] (conj [:g {}] args))

;-----------------------------------------------------------------------------------------------------------------------
(defn draw-rectangle [& {:as args}]
  (let [{:keys [id x y w h fill stroke filter radius]}
        (merge {:x 0, :y 0, :w 150, :h 100, :fill :lightgrey, :stroke {:paint :black :width 1}, :filter :none, :radius 0}
               args)]
    [:rect {:id id, :class [:box-text], :fill fill, :filter filter, :stroke stroke} [x y] [w h] radius]))

;-----------------------------------------------------------------------------------------------------------------------

(defn draw-text-stack [& {:as args}]
  (let [{:keys [texts color font size gap x y]}
        (merge {:texts [""], :font "sans-serif", :color {:text :black :fill :white}, :size 12, :gap 4} args)]
    (vec (concat [:dali/stack {:direction :down :gap gap}]
                 (map #(vector :text {:fill color, :font-family font, :font-size size, :x x, :y y} %) texts)))))

;-----------------------------------------------------------------------------------------------------------------------

(defn draw-text-box [& {:as args}]
  (let [{:keys [align x h text]} (merge {:text "", :align :center, :x 0, :h 100} args)]
    [:dali/align {:axis align}
     (apply draw-rectangle (conj (du/map->vec args)))
     (apply draw-text-stack
            (conj (du/map->vec args) :texts (mapv str/trim (str/split text #"\\n")), :x x, :y (du/ceil (/ h 2))))]))

;-----------------------------------------------------------------------------------------------------------------------

(def draw-label (partial draw-text-box :fill :none :stroke :none))

;-----------------------------------------------------------------------------------------------------------------------

(defn draw-line [& {:as args}]
  (let [{:keys [stroke w x1 y1 x2 y2]} (merge {:stroke :darkgrey, :w 1, :x1 0, :y1 0, :x2 100, :y2 100} args)]
    [:line {:stroke stroke, :width w} [x1 y1] [x2 y2]]))

;-----------------------------------------------------------------------------------------------------------------------

(defn draw-connector [& {:as args}]
  (let [{:keys [arrow x1 y1 x2 y2 text line-color w h size]}
        (merge {:arrow :triangle, :line-color "#bbbbbb", :w 2, :h 30, :size 9} args)]
    [:dali/stack {:gap -4}
     [:polyline {:dali/marker-end {:id arrow :fill line-color}} [x1 y1] [x2 y2]]
     [:dali/stack {:gap 0}
      (apply draw-line (conj (du/map->vec args) :w w, :stroke line-color))
      (if (not (empty? text))
        (apply draw-label (conj (du/map->vec args) :w (max 0 (- x2 x1 20)), :h h, :size size, :text text)))]]))

;-----------------------------------------------------------------------------------------------------------------------

(def draw-call (partial draw-connector :line-color :black))
(def draw-return (partial draw-connector :arrow :sharp :line-color "#999"))
(def draw-message (partial draw-connector :line-color :black :arrow :very-sharp))

;-----------------------------------------------------------------------------------------------------------------------

(defn draw-async-connector [& {:as args}]
  (let [{:keys [arrow target line-color w h fill size stroke]}
        (merge {:arrow  :very-sharp, :line-color "#0078d4", :size 10, :w 24, :h 30, :fill "#fada5e"
                :stroke {:paint :black :width 0.1}} args)]
    [:dali/stack {:gap 2 :direction :right :anchor :bottom-left}
     (apply draw-connector (conj (du/map->vec args) :arrow arrow :line-color line-color :fill :none))
     (apply draw-text-box (conj (du/map->vec args) :text target, :w w, :h h, :fill fill, :size size, :stroke stroke))]))

;-----------------------------------------------------------------------------------------------------------------------

(defn doc
  "Returns a dali page which can be rendered as an SVG file"
  [w h & args]
  (vec (concat [:dali/page {:width w :height h}
                [:defs
                 (prefab/drop-shadow-effect :ds {:opacity 0.3 :offset [3 3] :radius 3})
                 (prefab/sharp-arrow-marker :sharp {:scale 1})
                 (prefab/triangle-arrow-marker :triangle {:scale 1})
                 (prefab/sharp-arrow-marker :very-sharp {:width 8 :height 18})]]
               args)))

;-----------------------------------------------------------------------------------------------------------------------

(defn render-svg-file
  "Renders the given dali document into the specified path as an SVG file"
  [doc file]
  (io/render-svg doc file))

;-----------------------------------------------------------------------------------------------------------------------
