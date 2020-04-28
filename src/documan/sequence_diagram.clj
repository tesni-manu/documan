(ns documan.sequence-diagram
  (:gen-class)
  (:require [documan.utils :as utils]
            [documan.dali :as dd]
            [clojure.string :as str]
            [documan.utils :as du]))

;-----------------------------------------------------------------------------------------------------------------------

(defn create-edn-data
  "Creates the diagram edn from the given source code lines."
  [version title source-lines config]
  (let [lines (mapv utils/parse-tokens source-lines)

        ;Syntax: <object-id> = <tag> [<tag>...] "<object-name>"
        object-def? #(and (= "=" (second %)) (not= "(" (last %)))

        assert-valid-id #(if (= :already-returned %) (throw (Exception. (str "Invalid id: " %))))
        to-edn-object-def (fn [tokens]
                            (let [id (keyword (first tokens))]
                              (assert-valid-id id)
                              {:id id, :name (last tokens), :tags (mapv keyword (pop (vec (rest (rest tokens)))))}))
        objects (->> lines
                     (filterv object-def?)
                     (mapv to-edn-object-def))
        flow? #(not (object-def? %))
        process-flow (fn [payload tokens]
                       (let [invalid-syntax #(throw (Exception. (str "Invalid: " tokens)))
                             stack (:stack payload), flows (:flows payload), flags (:flags payload)
                             push-on-stack #(conj stack (keyword %)), this-flow (peek flows)
                             add-steps (fn [& new-steps]
                                         (conj (pop flows)
                                               (assoc this-flow :steps (vec (concat (:steps this-flow) new-steps)))))
                             is-flow-start-cmd? #(and (= "=" (second tokens)) (= "(" (last tokens)))
                             is-flow-end-cmd? #(= ")" (first tokens))
                             is-call-start-cmd? #(= "=>" (first tokens))
                             is-call-end-cmd? #(= "}" (first tokens))
                             is-return-cmd? #(= "return" (first tokens))
                             is-send-message-cmd? #(= "->" (first tokens))
                             is-async-call-flow-cmd? #(= ">>" (first tokens))
                             add-id #(assoc % :id (keyword (str/replace (str (:from %) "-" (:to %) "|"
                                                                             (:id this-flow)) ":" "")))
                             return-call (fn
                                           ([call]
                                            (add-id {:from (:to call), :to (:from call), :type :return}))
                                           ([from to]
                                            (add-id {:from to, :to from, :type :return}))
                                           ([from to description]
                                            (add-id {:from to, :to from, :type :return, :description description})))]
                         (cond
                           ; Syntax: <flow-id> = <object-id> "<flow-name>" (
                           (is-flow-start-cmd?) (if-not (empty? stack)
                                                  (invalid-syntax)
                                                  (let [id (keyword (first tokens))]
                                                    (assert-valid-id id)
                                                    {:flows (conj flows {:id   id, :name (nth tokens 3)
                                                                         :from (keyword (nth tokens 2)), :steps []})
                                                     :stack [(keyword (nth tokens 2))]
                                                     :flags #{}}))

                           ;--------------------------------------------------------------------------------------------
                           ; Syntax: => <object-id> "<call-name>" {
                           (is-call-start-cmd?) (if (empty? stack)
                                                  (invalid-syntax)
                                                  (let [from (peek stack)
                                                        to-self? (= "self" (second tokens))
                                                        to (if to-self? from (keyword (second tokens)))
                                                        description (nth tokens 2)
                                                        has-nested-calls? (and (= 4 (count tokens)) (= "{" (last tokens)))
                                                        this-call (add-id {:from from, :to to
                                                                           :type :call, :description description})]
                                                    (if to-self?
                                                      (assoc payload :flows (add-steps this-call))
                                                      (if has-nested-calls?
                                                        (assoc payload :flows (add-steps this-call)
                                                                       :stack (push-on-stack to))
                                                        (assoc payload :flows (add-steps this-call
                                                                                         (return-call this-call)))))))

                           ;--------------------------------------------------------------------------------------------
                           ; Syntax: }
                           (is-call-end-cmd?) (if (contains? flags :already-returned)
                                                (assoc payload :flags (disj flags :already-returned))
                                                (assoc payload :flows
                                                               (add-steps (return-call (peek (pop stack)) (peek stack)))
                                                               :stack (pop stack)))

                           ;--------------------------------------------------------------------------------------------
                           ; Syntax: return "<value>"
                           (is-return-cmd?) (let [to (peek stack)
                                                  from (peek (pop stack))
                                                  new-stack (pop stack)]
                                              (assoc payload :flows (add-steps (return-call from to (second tokens)))
                                                             :flags (conj flags :already-returned)
                                                             :stack new-stack))

                           ;--------------------------------------------------------------------------------------------
                           ; Syntax: -> <object-id> "<message>"
                           (is-send-message-cmd?) (if (empty? stack)
                                                    (invalid-syntax)
                                                    (let [from (peek stack)
                                                          to (keyword (second tokens))
                                                          description (last tokens)
                                                          this-call (add-id {:from from, :to to
                                                                             :type :message, :description description})]
                                                      (assoc payload :flows (add-steps this-call))))

                           ;--------------------------------------------------------------------------------------------
                           ; Syntax: >> <flow-id> "<message>"
                           (is-async-call-flow-cmd?) (if (empty? stack)
                                                       (invalid-syntax)
                                                       (let [from (peek stack)
                                                             to (keyword (second tokens))
                                                             description (last tokens)
                                                             this-call (add-id {:from from, :to to
                                                                                :type :flow, :description description})]
                                                         (assoc payload :flows (add-steps this-call))))

                           ;--------------------------------------------------------------------------------------------
                           ; Syntax: )
                           (is-flow-end-cmd?) (assoc payload :stack [] :flags #{})

                           ;--------------------------------------------------------------------------------------------

                           :else (invalid-syntax))))

        flows (->> lines
                   (filterv flow?)
                   (reduce process-flow {:flows [] :stack [] :flags #{}})
                   (:flows))]

    {:type :sequence-diagram, :version version, :title title, :objects objects, :flows flows}))

;-----------------------------------------------------------------------------------------------------------------------

(defn src->edn
  "Transpile the given source diagram into edn format."
  [path filename version title source config]
  (println "Transpiling diagram... " filename)
  (let [edn (create-edn-data version title source config)]
    (clojure.pprint/pprint edn (clojure.java.io/writer (str path filename ".edn")))))

;-----------------------------------------------------------------------------------------------------------------------

(defn create-sequence-diagram
  "Creates dali document for the given sequence diagram."
  [diagram config]
  (let [theme (merge {:bg               "#efefef"
                      :actor            {:fill "#fdab9f" :text :black :bar "#8b0000"}
                      :gui              {:fill "#bcffdb" :text :black :bar "#4f7942"}
                      :server           {:fill "#d8f9ff" :text :black :bar "#187bcd"}
                      :external         {:fill "#57595d" :text :white :bar "#232323"}
                      :flow-title       {:text "#0078d4"}
                      :flow-placeholder {:fill "#fada5e" :text :black :line-color "#0078d4"}}
                     (:theme config))

        ; Mutable data to hold the array of x, y1 and y2 values used to render bars
        obj-meta (atom {})

        objects (:objects diagram), flows (map-indexed #(assoc %2 :num (inc %1)) (:flows diagram))
        get-flow-by-id (fn [id] (first (filter #(= id (:id %)) flows)))
        flow-step-count (reduce #(+ %1 (count (:steps %2))) 0 flows)
        gap 40, padding 60, margin 40, object-width 100, object-title-height 50
        flow-item-height 50, flow-item-width 16, flow-item-offset (utils/ceil (/ flow-item-width 2))
        content-width (+ (* object-width (count objects)) (* gap (dec (count objects))))
        content-height (+ object-title-height gap (* flow-item-height (inc flow-step-count))
                          (* (+ gap flow-item-height) (dec (count flows))) gap)
        diagram-width (+ padding content-width padding), diagram-height (+ padding content-height padding)
        page-width (max 800 (+ margin diagram-width margin)), page-height (max 600 (+ margin diagram-height margin))
        diagram-left (utils/ceil (/ (- page-width diagram-width) 2))
        diagram-top (utils/ceil (/ (- page-height diagram-height) 2))
        content-left (+ diagram-left padding), content-top (+ diagram-top padding)
        content-bottom (- (+ diagram-top diagram-height) padding)

        draw-obj (fn [idx obj]
                   (let [obj-id (:id obj), x (+ content-left (* idx (+ object-width gap))), y content-top
                         x1 (+ x (utils/ceil (/ object-width 2))), y1 (+ y object-title-height), y2 content-bottom
                         current-theme ((first (:tags obj)) theme)]
                     ; x value is fixed for each object. y values will be pushed while rendering flow lines.
                     (swap! obj-meta assoc obj-id {:x x1 :y []})
                     (dd/group
                       ; Object title text-box
                       (dd/draw-text-box :id obj-id, :fill (:fill current-theme), :filter "url(#ds)", :x x, :y y,
                                         :stroke {:paint :black :width 0.1}, :w object-width, :h object-title-height
                                         :text (:name obj), :color (:text current-theme) :size 12)

                       ; Object's vertical line
                       (dd/draw-line :stroke :darkgrey, :w 1, :x1 x1, :y1 y1, :x2 x1, :y2 y2))))

        draw-flow-title #(dd/draw-label :id (:id %1), :x (- (:x %3) object-width flow-item-offset), :size 12
                                        :y (- %4 5), :w object-width, :h object-title-height
                                        :color (:text (:flow-title theme))
                                        :text (if (= 1 (count flows)) (:name %1) (str "#" (inc %2) " " (:name %1))))

        draw-lines (fn [steps y flow-id]
                     (map-indexed
                       (fn [idx step]
                         (let [step-type (:type step), from (:from step), to (:to step)
                               is-first-step (= idx 0), is-last-step (= idx (dec (count steps)))
                               push-y (fn [obj-id y]
                                        (let [this-obj-meta (obj-id @obj-meta), current-y (:y this-obj-meta)]
                                          (swap! obj-meta assoc obj-id
                                                 (assoc this-obj-meta :y (conj current-y y)))))]
                           (cond
                             ; Self (ie. processing)
                             (and (= :call step-type)
                                  (= from to))
                             (let [x1 (+ flow-item-offset (:x (from @obj-meta)))
                                   y1 (+ y gap (* idx flow-item-height))
                                   x2 (+ x1 (* 2.5 gap)), y2 y1
                                   x3 x2, y3 (+ y2 (utils/ceil (/ flow-item-height 4)))
                                   x4 x1, y4 y3]
                               (if is-first-step (push-y from (- y1 flow-item-height)))
                               (if is-last-step (push-y from (+ y1 flow-item-height)))
                               (dd/group
                                 (dd/draw-line :x1 x1, :y1 y1, :x2 x2, :y2 y2, :stroke {:paint :black, :width 1})
                                 (dd/draw-line :x1 x2, :y1 y2, :x2 x3, :y2 y3 :stroke {:paint :black, :width 1})
                                 (dd/draw-call :id flow-id, :x1 x3, :y1 y3, :x2 x4, :y2 y4, :text (:description step))))

                             ;------------------------------------------------------------------------------------------
                             ; Call
                             (= :call step-type)
                             (let [x1 (+ flow-item-offset (:x (from @obj-meta))), y1 (+ y gap (* idx flow-item-height))
                                   x2 (- (:x (to @obj-meta)) flow-item-offset), y2 y1]
                               (if is-first-step (push-y from (- y1 flow-item-height)))
                               (push-y to y1)
                               (dd/draw-call :id flow-id, :x1 x1, :y1 y1, :x2 x2, :y2 y2, :text (:description step)))

                             ;------------------------------------------------------------------------------------------
                             ; Return from a call
                             (= :return step-type)
                             (let [x1 (- (:x (from @obj-meta)) flow-item-offset), y1 (+ y gap (* idx flow-item-height))
                                   x2 (+ (:x (to @obj-meta)) flow-item-offset), y2 y1, desc (:description step)]
                               (if is-last-step (push-y to (+ y1 flow-item-height)))
                               (push-y from y1)
                               (dd/draw-return :id flow-id, :x1 x1, :y1 y1, :x2 x2, :y2 y2, :text (if (nil? desc) "" desc)))

                             ;------------------------------------------------------------------------------------------
                             ; Message without any processing at the destination
                             (= :message step-type)
                             (let [x1 (+ flow-item-offset (:x (from @obj-meta))), y1 (+ y gap (* idx flow-item-height))
                                   x2 (- (:x (to @obj-meta)) flow-item-offset), y2 y1]
                               (if is-first-step (push-y from (- y1 flow-item-height)))
                               (if is-last-step (push-y from (+ y1 flow-item-height)))
                               (push-y to y1)
                               (push-y to (du/ceil (+ y1 (/ flow-item-height 2))))
                               (dd/draw-message :id flow-id, :x1 x1, :y1 y1, :x2 x2, :y2 y2, :text (:description step)))

                             ;------------------------------------------------------------------------------------------
                             ; Flow (message to another flow). We can only show a placeholder for the target flow
                             (= :flow step-type)
                             (let [flow-dest (get-flow-by-id to)
                                   x1 (+ flow-item-offset (:x (from @obj-meta))), y1 (+ y gap (* idx flow-item-height))
                                   x2 (+ x1 (* 2.5 gap)), y2 y1]
                               (if is-first-step (push-y from (- y1 flow-item-height)))
                               (if is-last-step (push-y from (+ y1 flow-item-height)))
                               (dd/draw-async-connector :id flow-id, :x1 x1, :y1 y1, :x2 x2, :y2 y2
                                                        :target (str "#" (:num flow-dest)),
                                                        :line-color (:line-color (:flow-placeholder theme))
                                                        :fill (:fill (:flow-placeholder theme))
                                                        :text (:description step)))

                             ;------------------------------------------------------------------------------------------
                             :else nil))) steps))

        draw-flow (fn [payload flow]
                    (let [idx (:idx payload), y (:y payload), steps (:steps flow), flow-obj-id (:from flow)
                          this-meta (flow-obj-id @obj-meta), dali-doc (:dali-doc payload), flow-id (:id flow)
                          flow-title-el (draw-flow-title flow idx this-meta y)
                          flow-lines (draw-lines steps y flow-id)]
                      (assoc payload :idx (inc idx), :y (+ y gap (* (inc (count steps)) flow-item-height))
                                     :dali-doc (vec (concat (conj dali-doc flow-title-el) flow-lines)))))

        draw-flow-bars #(map (fn [obj]
                               (let [meta-data ((:id obj) @obj-meta), x (:x meta-data)
                                     y (mapv vec (partition 2 (:y meta-data)))]
                                 (apply dd/group
                                        (mapv (fn [bar]
                                                (let [x1 (- x flow-item-offset), x2 (+ x flow-item-offset)
                                                      y1 (first bar), y2 (second bar)]
                                                  (dd/draw-rectangle
                                                    :id (str "bar-" (:id obj)), :fill (:bar ((first (:tags obj)) theme))
                                                    :stroke :none, :x x1, :y y1, :w (- x2 x1), :h (- y2 y1))))
                                              y))))
                             objects)]

    (dd/doc page-width page-height
            ; title
            (dd/draw-label :id :title, :text (:title diagram), :size 16, :x 40, :y 0, :w 150, :h 70, :align :left)

            ; bounding rectangle
            (dd/draw-rectangle :id :bounding-rectangle, :stroke :none, :fill (:bg theme)
                               :x diagram-left, :y diagram-top, :w diagram-width, :h diagram-height)

            ; objects
            (apply dd/group (map-indexed (fn [idx obj] (draw-obj idx obj)) objects))

            ; flow
            (apply dd/group (->> flows
                                 (reduce draw-flow {:idx 0, :y (+ content-top object-title-height gap), :dali-doc []})
                                 (:dali-doc)))
            ; flow-bars
            (draw-flow-bars))))

;-----------------------------------------------------------------------------------------------------------------------

(defn edn->document
  "Generates a sequence diagram with the given data in the specified path."
  [path filename diagram config]
  (println "Generating diagram... " filename)
  (dd/render-svg-file (create-sequence-diagram diagram config) (str path filename ".svg")))

;-----------------------------------------------------------------------------------------------------------------------
