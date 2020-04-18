(ns documan.sequence-diagram
  (:gen-class)
  (:require [documan.utils :as utils]
            [clojure.string :as str]
            [dali.io :as io]
            [dali [prefab :as prefab]]
            [dali.layout.stack :as stack]
            [dali.layout.align :as align]
            [dali.layout.surround :as surround]))

;-------------------------------------------------------------------------------

(defn create-edn
  "Creates the diagram edn from the given source code."
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

                             get-return-call (fn
                                               ([call]
                                                {:from (:to call)
                                                 :to   (:from call)
                                                 :type :return})
                                               ([from to]
                                                {:from to
                                                 :to   from
                                                 :type :return})
                                               ([from to description]
                                                {:from        to
                                                 :to          from
                                                 :type        :return
                                                 :description description}))]

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
                                   this-call {:from        from
                                              :to          to
                                              :type        :call
                                              :description description}]
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
                                   this-call {:from        from
                                              :to          to
                                              :type        :message
                                              :description description}]
                               (assoc payload :flows (add-steps this-call))))

                           ;----------------------------------------------------
                           ; Syntax: >> <flow-id> "<message>"
                           (is-async-call-flow-cmd?)

                           (if (empty? stack)
                             (invalid-syntax)
                             (let [from (peek stack)
                                   to (keyword (second tokens))
                                   description (last tokens)
                                   this-call {:from        from
                                              :to          to
                                              :type        :flow
                                              :description description}]
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

(defn transpile-diagram
  "Transpile the given source diagram into edn format."
  [path filename version title source]
  (println "Transpiling diagram... " filename)
  (let [edn (create-edn version title source)]
    (clojure.pprint/pprint
      edn
      (clojure.java.io/writer (str path filename ".edn")))))

;-------------------------------------------------------------------------------

(defn create-sequence-diagram
  "Creates dali document for the given sequence diagram."
  [diagram]
  (let [theme {:actor    {:bg "#ffcccc" :text :black}
               :gui      {:bg "#ccffe5" :text :black}
               :server   {:bg "#cce5ff" :text :black}
               :external {:bg "#777777" :text :white}}

        ceil #(int (Math/ceil %))
        objects (:objects diagram)
        flows (:flows diagram)
        flow-step-count (reduce #(+ %1 (count (:steps %2))) 0 flows)
        gap 40
        object-title-height 100
        object-width 100
        object-title-height 60
        flow-item-height 50
        content-width (+ gap (* (+ gap object-width) (count objects)))
        content-height (+ (* 2 gap) object-title-height
                          (* flow-item-height flow-step-count))
        margin 40
        page-width (max 800 (+ (* 2 margin) content-width))
        page-height (max 700 (+ (* 2 margin) content-height))
        diagram-left margin
        diagram-top (+ margin 5)
        diagram-width (- page-width (* 2 margin))
        diagram-height (- page-height (* 2 margin))
        horizontal-padding (ceil (/ (- diagram-width content-width) 2))
        vertical-padding (ceil (/ (- diagram-height content-height) 2))
        title-color "#555"
        draw-text-stack (fn [texts color]
                          (vec (concat [:dali/stack {:direction :down :gap 6}]
                                       (map #(vector
                                               :text {:fill        color
                                                      :font-family "Sans Serif"
                                                      :font-size   12} %)
                                            texts))))
        draw-obj (fn [idx obj]
                   (let [x (+ diagram-left horizontal-padding gap
                              (* idx (+ object-width gap)))
                         y (+ diagram-top vertical-padding gap)
                         x1 (+ x (ceil (/ object-width 2)))
                         y1 (+ y object-title-height)
                         y2 (+ y1 (* flow-step-count flow-item-height))
                         current-theme ((first (:tags obj)) theme)]
                     [:g {}

                      ; Object title text-box
                      [:dali/align {:axis :center}
                       [:rect {:id     (:id obj)
                               :class  [:box-text]
                               :fill   (:bg current-theme)
                               :filter "url(#ds)"
                               :stroke {:paint :black
                                        :width 0.1}}
                        [x y]
                        [object-width object-title-height] 2]
                       (draw-text-stack
                         (mapv str/trim (str/split (:name obj) #"\\n"))
                         (:text current-theme))]

                      ; Object's vertical line
                      [:line {:stroke :darkgrey
                              :width  1}
                       [x1 y1]
                       [x1 y2]]]))
        draw-flows (fn []
                     ; TODO
                     )]

    [:dali/page {:width page-width :height page-height}
     [:defs
      (prefab/drop-shadow-effect :ds {:opacity 0.2 :offset [3 3] :radius 3})]
     ;_____________________________________________________
     ; title
     [:text {:id          :title
             :font-family "Sans Serif"
             :stroke      {:paint title-color :width 0.3}
             :fill        title-color
             :font-size   16
             :x           margin
             :y           margin}
      (:title diagram)]

     ;_____________________________________________________
     ; bounding rectangle
     [:rect {:id     :bounding-rectangle
             :stroke {:paint :darkgrey
                      :width 0.2}
             :fill   :white}
      [diagram-left diagram-top]
      [diagram-width diagram-height]]

     ;_____________________________________________________
     ; objects
     (vec (concat [:g] (map-indexed (fn [idx obj] (draw-obj idx obj)) objects)))

     ;_____________________________________________________
     ; flow
     (vec (concat [:g] (draw-flows)))]))

;-------------------------------------------------------------------------------

(defn generate-diagram
  "Generates a sequence diagram with the given data in the specified path."
  [path filename diagram]
  (println "Generating diagram... " filename)
  (io/render-svg
    (create-sequence-diagram diagram)
    (str path filename ".svg")))

;-------------------------------------------------------------------------------
