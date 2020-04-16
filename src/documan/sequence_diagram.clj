(ns documan.sequence-diagram
  (:gen-class)
  (:require [documan.utils :as utils]
            [clojure.string :as str]
            [dali.io :as io]
            [dali.layout.stack :as stack]
            [dali.layout.surround :as surround]))

;-------------------------------------------------------------------------------

(defn create-edn
  "Creates the diagram edn from the given source code."
  [version title source-lines]
  (let [lines (mapv utils/parse-tokens source-lines)
        object-def? #(= "=" (second %))
        to-edn-object-def (fn [tokens]
                            {:id   (keyword (first tokens))
                             :name (last tokens)
                             :tags (mapv keyword
                                         (pop (vec (rest (rest tokens)))))})
        objects (->> lines
                     (filterv object-def?)
                     (mapv to-edn-object-def))
        flow? #(not (object-def? %))
        process-flow (fn [coll tokens]
                       (let [invalid-syntax #(throw
                                               (Exception.
                                                 (str "Invalid: " tokens)))
                             stack (peek coll)
                             push-on-stack #(conj stack (keyword %))
                             flows (pop coll)
                             flow-start-cmd? (= "(" (second tokens))
                             call-start-cmd? (= "=>" (first tokens))
                             call-end-cmd? (= "}" (first tokens))
                             return-cmd? (= "return" (first tokens))
                             flow-end-cmd? (= ")" (first tokens))
                             return-call (fn
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
                           ; Syntax: <id> (
                           flow-start-cmd?

                           (if-not (empty? stack)
                             (invalid-syntax)
                             (conj flows (push-on-stack (first tokens))))

                           ;----------------------------------------------------
                           ; Syntax: => <id> "<name>" {
                           call-start-cmd?

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
                                 (conj flows this-call stack)
                                 (if has-nested-calls?
                                   (conj flows this-call (push-on-stack to))
                                   (conj flows
                                         this-call
                                         (return-call this-call)
                                         stack)))))

                           ;----------------------------------------------------
                           ; Syntax: }
                           call-end-cmd?

                           (if (nil? (peek stack))
                             (conj flows (pop stack))
                             (let [to (peek stack)
                                   from (peek (pop stack))]
                               (conj flows (return-call from to) (pop stack))))

                           ;----------------------------------------------------
                           ; Syntax: return "<value>"
                           return-cmd?

                           (let [to (peek stack)
                                 from (peek (pop stack))]
                             (conj flows
                                   (return-call from to (second tokens))
                                   (conj (pop stack) nil)))

                           ;----------------------------------------------------
                           ; Syntax: )
                           flow-end-cmd? flows

                           ;----------------------------------------------------

                           :else (invalid-syntax))))
        flows (->> lines
                   (filterv flow?)
                   (reduce process-flow [[]]))]

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

;-------------------------------------------------------------------------------

(defn generate-diagram
  "Generates a sequence diagram with the given data in the specified path."
  [path filename diagram]
  (println "Generating diagram... " filename)
  (io/render-svg
    (create-sequence-diagram diagram)
    (str path filename ".svg")))

;-------------------------------------------------------------------------------
