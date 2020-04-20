(ns documan.sequence-diagram
  (:gen-class)
  (:require [documan.utils :as utils]
            [clojure.string :as str]
            [dali.io :as io]
            [dali
             [prefab :as prefab]
             [syntax :as d]]
            [dali.layout.stack :as stack]
            [dali.layout.align :as align]
            [dali.layout.surround :as surround]))

;-------------------------------------------------------------------------------

(defn create-edn-data
  "Creates the diagram edn from the given source code lines."
  [version title source-lines]
  (let [lines (mapv utils/parse-tokens source-lines)

        ; Syntax: <object-id> = <tag> [<tag>...] "<object-name>"
        object-def? #(and (= "=" (second %)) (not= "(" (last %)))

        assert-valid-id #(if (= :already-returned %)
                           (throw (Exception. (str "Invalid id: " %))))
        to-edn-object-def (fn [tokens]
                            (let [id (keyword (first tokens))]
                              (assert-valid-id id)
                              {:id   id
                               :name (last tokens)
                               :tags (mapv keyword
                                           (pop (vec (rest (rest tokens)))))}))
        objects (->> lines
                     (filterv object-def?)
                     (mapv to-edn-object-def))
        flow? #(not (object-def? %))
        process-flow (fn [payload tokens]
                       (let [invalid-syntax #(throw
                                               (Exception.
                                                 (str "Invalid: " tokens)))
                             stack (:stack payload)
                             flows (:flows payload)
                             flags (:flags payload)
                             push-on-stack #(conj stack (keyword %))
                             this-flow (peek flows)
                             add-steps (fn [& new-steps]
                                         (conj
                                           (pop flows)
                                           (assoc this-flow
                                             :steps
                                             (vec (concat (:steps this-flow)
                                                          new-steps)))))

                             is-flow-start-cmd? #(and (= "=" (second tokens))
                                                      (= "(" (last tokens)))
                             is-flow-end-cmd? #(= ")" (first tokens))

                             is-call-start-cmd? #(= "=>" (first tokens))
                             is-call-end-cmd? #(= "}" (first tokens))
                             is-return-cmd? #(= "return" (first tokens))

                             is-send-message-cmd? #(= "->" (first tokens))
                             is-async-call-flow-cmd? #(= ">>" (first tokens))

                             add-id #(assoc % :id
                                              (keyword
                                                (str/replace
                                                  (str (:from %) "-"
                                                       (:to %) "|"
                                                       (:id this-flow))
                                                  ":" "")))
                             get-return-call (fn
                                               ([call]
                                                (add-id {:from (:to call)
                                                         :to   (:from call)
                                                         :type :return}))
                                               ([from to]
                                                (add-id {:from to
                                                         :to   from
                                                         :type :return}))
                                               ([from to description]
                                                (add-id {:from        to
                                                         :to          from
                                                         :type        :return
                                                         :description description})))]

                         (cond
                           ; Syntax: <flow-id> = <object-id> "<flow-name>" (
                           (is-flow-start-cmd?)

                           (if-not (empty? stack)
                             (invalid-syntax)
                             (let [id (keyword (first tokens))]
                               (assert-valid-id id)
                               {:flows (conj flows
                                             {:id    id
                                              :name  (nth tokens 3)
                                              :from  (keyword (nth tokens 2))
                                              :steps []})
                                :stack [(keyword (nth tokens 2))]
                                :flags #{}}))

                           ;----------------------------------------------------
                           ; Syntax: => <object-id> "<call-name>" {
                           (is-call-start-cmd?)

                           (if (empty? stack)
                             (invalid-syntax)
                             (let [from (peek stack)
                                   to-self? (= "self" (second tokens))
                                   to (if to-self?
                                        from
                                        (keyword (second tokens)))
                                   description (nth tokens 2)
                                   has-nested-calls? (and (= 4 (count tokens))
                                                          (= "{" (last tokens)))
                                   this-call (add-id {:from        from
                                                      :to          to
                                                      :type        :call
                                                      :description description})]
                               (if to-self?
                                 (assoc payload :flows (add-steps this-call))
                                 (if has-nested-calls?
                                   (assoc payload
                                     :flows (add-steps this-call)
                                     :stack (push-on-stack to))
                                   (assoc payload
                                     :flows (add-steps
                                              this-call
                                              (get-return-call this-call)))))))

                           ;----------------------------------------------------
                           ; Syntax: }
                           (is-call-end-cmd?)

                           (if (contains? flags :already-returned)
                             (assoc payload
                               :flags (disj flags :already-returned))
                             (assoc payload
                               :flows (add-steps
                                        (get-return-call (peek (pop stack))
                                                         (peek stack)))
                               :stack (pop stack)))

                           ;----------------------------------------------------
                           ; Syntax: return "<value>"
                           (is-return-cmd?)

                           (let [to (peek stack)
                                 from (peek (pop stack))
                                 new-stack (pop stack)]
                             (assoc payload
                               :flows (add-steps (get-return-call
                                                   from to (second tokens)))
                               :flags (conj flags :already-returned)
                               :stack new-stack))

                           ;----------------------------------------------------
                           ; Syntax: -> <object-id> "<message>"
                           (is-send-message-cmd?)

                           (if (empty? stack)
                             (invalid-syntax)
                             (let [from (peek stack)
                                   to (keyword (second tokens))
                                   description (last tokens)
                                   this-call (add-id {:from        from
                                                      :to          to
                                                      :type        :message
                                                      :description description})]
                               (assoc payload :flows (add-steps this-call))))

                           ;----------------------------------------------------
                           ; Syntax: >> <flow-id> "<message>"
                           (is-async-call-flow-cmd?)

                           (if (empty? stack)
                             (invalid-syntax)
                             (let [from (peek stack)
                                   to (keyword (second tokens))
                                   description (last tokens)
                                   this-call (add-id {:from        from
                                                      :to          to
                                                      :type        :flow
                                                      :description description})]
                               (assoc payload :flows (add-steps this-call))))

                           ;----------------------------------------------------
                           ; Syntax: )
                           (is-flow-end-cmd?)

                           (assoc payload :stack [] :flags #{})

                           ;----------------------------------------------------

                           :else (invalid-syntax))))
        flows (->> lines
                   (filterv flow?)
                   (reduce process-flow
                           {:flows [] :stack [] :flags #{}})
                   (:flows))]

    {:type    :sequence-diagram
     :version version
     :title   title
     :objects objects
     :flows   flows}))

;-------------------------------------------------------------------------------

(defn src->edn
  "Transpile the given source diagram into edn format."
  [path filename version title source]
  (println "Transpiling diagram... " filename)
  (let [edn (create-edn-data version title source)]
    (clojure.pprint/pprint
      edn
      (clojure.java.io/writer (str path filename ".edn")))))

;-------------------------------------------------------------------------------

(defn map->vec [m] (vec (flatten (into (vector) m))))
(defn ceil [n] (int (Math/ceil n)))
(defn group [& args]
  (conj [:g {}] args))

;-------------------------------------------------------------------------------

(defn draw-rectangle [& {:as args}]
  (let [{:keys [id x y w h fill stroke filter radius]}
        (merge {:x      0
                :y      0
                :w      150
                :h      100
                :fill   :lightgrey
                :stroke {:paint :black :width 1}
                :filter :none
                :radius 0} args)]

    [:rect {:id     id
            :class  [:box-text]
            :fill   fill
            :filter filter
            :stroke stroke}
     [x y]
     [w h] radius]))

;-------------------------------------------------------------------------------

(defn draw-text-stack [& {:as args}]
  (let [{:keys [texts color font size gap x y]}
        (merge {:texts [""]
                :font  "sans-serif"
                :color {:text :black :fill :white}
                :size  12
                :gap   4} args)]
    (vec (concat [:dali/stack {:direction :down :gap gap}]
                 (map #(vector :text
                               {:fill        color
                                :font-family font
                                :font-size   size
                                :x           x
                                :y           y}
                               %) texts)))))

;-------------------------------------------------------------------------------

(defn draw-text-box [& {:as args}]
  (let [{:keys [align x h text]} (merge {:text  ""
                                         :align :center
                                         :x     0
                                         :h     100} args)]
    [:dali/align {:axis align}
     (apply draw-rectangle (conj (map->vec args)))
     (apply draw-text-stack
            (conj (map->vec args)
                  :texts (mapv str/trim (str/split text #"\\n"))
                  :x x
                  :y (ceil (/ h 2))))]))

(def draw-label (partial draw-text-box :fill :none :stroke :none))

;-------------------------------------------------------------------------------

(defn draw-line [& {:as args}]
  (let [{:keys [stroke w x1 y1 x2 y2]}
        (merge {:stroke :darkgrey
                :w      1
                :x1     0
                :y1     0
                :x2     100
                :y2     100} args)]
    [:line {:stroke stroke
            :width  w}
     [x1 y1]
     [x2 y2]]))

;-------------------------------------------------------------------------------

(defn draw-connector [& {:as args}]
  (let [{:keys [arrow x1 y1 x2 y2 text line-color w]}
        (merge {:arrow :triangle :line-color "#bbbbbb" :w 2} args)]
    [:dali/stack {:gap -4}
     [:polyline
      {:dali/marker-end
       {:id arrow :fill line-color}}
      [x1 y1] [x2 y2]]
     [:dali/stack {:gap 0}
      (apply draw-line (conj (map->vec args)
                             :w w
                             :stroke line-color))
      (apply draw-label (conj (map->vec args)
                              :w (max 0 (- x2 x1 20))
                              :h 30
                              :size 9
                              :text text))]]))

(def draw-call (partial draw-connector :line-color :black))
(def draw-return (partial draw-connector :arrow :very-sharp :line-color "#999"))

;-------------------------------------------------------------------------------

(defn create-sequence-diagram
  "Creates dali document for the given sequence diagram."
  [diagram]
  (let [theme {:actor    {:fill "#ffcccc" :text :black}
               :gui      {:fill "#ccffe5" :text :black}
               :server   {:fill "#cce5ff" :text :black}
               :external {:fill "#475c4e" :text :white}}

        objects (:objects diagram)
        obj-meta (atom {})
        flows (:flows diagram)
        get-flow-by-id (fn [id] (filter #(= id (:id %)) flows))
        flow-step-count (reduce #(+ %1 (count (:steps %2))) 0 flows)
        gap 40
        padding 60
        object-width 100
        object-title-height 50
        flow-item-height 50
        flow-item-width 16
        flow-item-offset (ceil (/ flow-item-width 2))
        content-width (+ (* object-width (count objects))
                         (* gap (dec (count objects))))
        content-height (+ object-title-height
                          gap
                          (* flow-item-height (inc flow-step-count))
                          gap)
        margin 40
        diagram-width (+ padding content-width padding)
        diagram-height (+ padding content-height padding)
        page-width (max 800 (+ margin diagram-width margin))
        page-height (max 600 (+ margin diagram-height margin))
        diagram-left (ceil (/ (- page-width diagram-width) 2))
        diagram-top (ceil (/ (- page-height diagram-height) 2))
        content-left (+ diagram-left padding)
        content-top (+ diagram-top padding)
        content-bottom (- (+ diagram-top diagram-height) padding)
        draw-obj (fn [idx obj]
                   (let [obj-id (:id obj)
                         x (+ content-left
                              (* idx (+ object-width gap)))
                         y content-top
                         x1 (+ x (ceil (/ object-width 2)))
                         y1 (+ y object-title-height)
                         y2 content-bottom
                         current-theme ((first (:tags obj)) theme)]
                     (swap! obj-meta assoc obj-id {:x x1 :y []})
                     (group
                       ; Object title text-box
                       (draw-text-box :id obj-id
                                      :fill (:fill current-theme)
                                      :filter "url(#ds)"
                                      :stroke {:paint :black
                                               :width 0.1}
                                      :x x
                                      :y y
                                      :w object-width
                                      :h object-title-height
                                      :text (:name obj)
                                      :color (:text current-theme)
                                      :size 11)
                       ; Object's vertical line
                       (draw-line :stroke :darkgrey
                                  :w 1
                                  :x1 x1
                                  :y1 y1
                                  :x2 x1
                                  :y2 y2))))
        draw-flow (fn [payload flow]
                    (let [meta @obj-meta
                          y (:y payload)
                          steps (:steps flow)
                          flow-obj-id (:from flow)
                          this-meta (flow-obj-id meta)
                          dali-el (:dali-el payload)
                          flow-id (:id flow)
                          flow-title-el (draw-label :id (:id flow)
                                                    :x (- (:x this-meta)
                                                          object-width
                                                          flow-item-offset)
                                                    :y y
                                                    :w object-width
                                                    :h object-title-height
                                                    :text (:name flow))
                          flow-lines (map-indexed
                                       (fn [idx step]
                                         (let [step-type (:type step)
                                               from (:from step)
                                               to (:to step)]
                                           (cond
                                             (and (= :call step-type)
                                                  (= from to))
                                             (let [x1 (+ flow-item-offset
                                                         (:x (from meta)))
                                                   y1 (+ y gap
                                                         (* idx flow-item-height)
                                                         -15)
                                                   x2 (+ x1 (* 2.5 gap))
                                                   y2 y1
                                                   x3 x2
                                                   y3 (+ y2 15 (ceil (/ flow-item-height 4)))
                                                   x4 x1
                                                   y4 y3
                                                   x0 (+ x1 object-width)
                                                   y0 (- y1 40)]
                                               (group
                                                 (draw-line :x1 x1, :y1 y1
                                                            :x2 x2, :y2 y2
                                                            :stroke {:paint :black
                                                                     :width 1})
                                                 (draw-line :x1 x2, :y1 y2
                                                            :x2 x3, :y2 y3
                                                            :stroke {:paint :black
                                                                     :width 1})
                                                 (draw-call :id flow-id
                                                            :x1 x3
                                                            :y1 y3
                                                            :x2 x4
                                                            :y2 y4
                                                            :text (:description step))))

                                             (= :call step-type)
                                             (let [x1 (+ flow-item-offset
                                                         (:x (from meta)))
                                                   y1 (+ y gap
                                                         (* idx flow-item-height))
                                                   x2 (- (:x (to meta))
                                                         flow-item-offset)
                                                   y2 y1]
                                               (if (= from
                                                      (:id (first objects)))
                                                 (swap! obj-meta assoc
                                                        from
                                                        (assoc
                                                          (from @obj-meta)
                                                          :y
                                                          (conj (:y (from @obj-meta))
                                                                (- y1 flow-item-height)))))
                                               (swap! obj-meta assoc
                                                      to
                                                      (assoc
                                                        (to @obj-meta)
                                                        :y
                                                        (conj (:y (to @obj-meta)) y1)))
                                               (draw-call :id flow-id
                                                          :x1 x1
                                                          :y1 y1
                                                          :x2 x2
                                                          :y2 y2
                                                          :text (:description step)))

                                             (= :return step-type)
                                             (let [x1 (- (:x (from meta))
                                                         flow-item-offset)
                                                   y1 (+ y gap
                                                         (* idx flow-item-height))
                                                   x2 (+ (:x (to meta))
                                                         flow-item-offset)
                                                   y2 y1
                                                   desc (:description step)]
                                               (swap! obj-meta assoc
                                                      from
                                                      (assoc
                                                        (from @obj-meta)
                                                        :y
                                                        (conj (:y (from @obj-meta)) y1)))
                                               (if (= to (:id (first objects)))
                                                 (swap! obj-meta assoc
                                                        to
                                                        (assoc
                                                          (to @obj-meta)
                                                          :y
                                                          (conj (:y (to @obj-meta))
                                                                (+ y1 flow-item-height)))))
                                               (draw-return :id flow-id
                                                            :x1 x1
                                                            :y1 y1
                                                            :x2 x2
                                                            :y2 y2
                                                            :text (if (nil? desc)
                                                                    "return" desc)))

                                             ; TODO: Self & message
                                             :else nil))) steps)
                          flow-bars (map (fn [obj]
                                           (let [bars-meta @obj-meta
                                                 meta-data ((:id obj) bars-meta)
                                                 x (:x meta-data)
                                                 y (mapv vec (partition 2 (:y meta-data)))]
                                             (apply group (mapv (fn [bar]
                                                                  (let [x1 (- x flow-item-offset)
                                                                        x2 (+ x flow-item-offset)
                                                                        y1 (first bar)
                                                                        y2 (second bar)]
                                                                    (draw-rectangle :id (str "bar-" (:id obj))
                                                                                    :fill "#733D1F"
                                                                                    :stroke :none
                                                                                    :x x1
                                                                                    :y y1
                                                                                    :w (- x2 x1)
                                                                                    :h (- y2 y1))))
                                                                y))))
                                         objects)]

                      (assoc payload
                        :dali-el
                        (vec (concat (conj dali-el flow-title-el)
                                     (filter some? flow-lines)
                                     flow-bars)))))]

    [:dali/page {:width page-width :height page-height}
     [:defs
      (prefab/drop-shadow-effect :ds {:opacity 0.2 :offset [3 3] :radius 3})
      (prefab/sharp-arrow-marker :sharp {:scale 1})
      (prefab/triangle-arrow-marker :triangle {:scale 1})
      (prefab/sharp-arrow-marker :very-sharp {:width 8 :height 18})]
     ; title
     (draw-label :id :title
                 :text (:title diagram)
                 :size 16
                 :x 40
                 :y 0
                 :w 150
                 :h 70
                 :align :left)
     ; bounding rectangle
     (draw-rectangle :id :bounding-rectangle
                     :stroke :none
                     :fill "#efefff"
                     :x diagram-left
                     :y diagram-top
                     :w diagram-width
                     :h diagram-height)
     ; objects
     (apply group (map-indexed (fn [idx obj] (draw-obj idx obj)) objects))
     ; flow
     (apply group (->> flows
                       (reduce draw-flow
                               {:idx     0
                                :y       (+ content-top object-title-height gap)
                                :dali-el []})
                       (:dali-el)))]))

;-------------------------------------------------------------------------------

(defn edn->document
  "Generates a sequence diagram with the given data in the specified path."
  [path filename diagram]
  (println "Generating diagram... " filename)
  (io/render-svg
    (create-sequence-diagram diagram)
    (str path filename ".svg")))

;-------------------------------------------------------------------------------
