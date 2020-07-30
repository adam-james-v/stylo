(ns stylo.ext.fabric
  (:require [clojure.string :as s]
            [stylo.draw :as draw]
            #_[hiccup.core :refer [h html]]
            #_[hiccup.def :refer [defelem]]
            #_[hiccup.page :as page]
            #_[hiccup.form :as form]
            #_[hiccup.element :as elem]))

(def fabric-styles
  [:style "
.ln{stroke:#2e3440;stroke-width:1.5;}
.ln-d{stroke:#2e3440;stroke-width:1.5;stroke-linecap:round;stroke-dasharray:4, 5;}
rect, line, path, polygon, polyline {vector-effect:non-scaling-stroke;}
.attn{fill:rgb(234,82,111);}
.clr{fill:rgba(0,0,0,0);}
"])

(defn sq
  ([s]
   (sq s nil))
  ([s class]
   [:rect {:class ["ln" (if class class "clr")]
           :width s
           :height s}]))

(defn rct
  ([[x y]]
   (rct [x y] nil))
  ([[x y] class]
   [:rect {:class ["ln" (if class class "clr")]
           :width x
           :height y}]))

(defn hst
  ([s]
   (hst s nil))
  ([s class]
   [:polygon {:class ["ln" (if class class "clr")]
              :points (draw/pt-str [[0 s] [s 0] [0 0]])}]))

(defn hst-pts
  [s]
  [[0 0] [s 0] [0 s]])

(defn diamond
  "draw a diamond of width and height with width offset and height offset factors."
  ([[w h wof hof]]
   (diamond [w h wof hof] nil))
  ([[w h wof hof] class]
   (let [wod (* w wof)
         hod (* h hof)]
     [:polygon {:class ["ln" (if class class "clr")]
                :points (draw/pt-str [[wod 0]
                                 [w hod]
                                 [wod h]
                                 [0 hod]])}])))

(defn diamond-pts
  [x y y2]
  (let [x2 (/ x 2.0)]
    [[x2 0]
     [x y2]
     [x2 y]
     [0 y2]]))

(defn stack
  [elem n]
  (let [spc 0.185
        tfrms (map #(vector (* % spc) (* % spc)) (range n))]
    [:g {}
     (map 
      (fn [[x y]] [:g {:transform (draw/translate-str x y)} elem]) 
      tfrms)]))

(def scale-1-to-1 146)

(defn polygon-template
  [name pts]
  (list
   (draw/polygon-d pts)
   (draw/polygon (draw/offset pts 0.25))
   (draw/mv (draw/bb-center pts) (draw/label name))
   (map #(draw/mv % (draw/sc 0.25 (draw/dot [0 0]))) pts)))

(defn diamond-template
  [name w h hof]
  (let [pts (diamond-pts w h hof)
        half-w (/ w 2.0)]
    (concat
     (polygon-template name pts)
     (draw/ln-d [half-w 0] [half-w h]))))

(defn colourize-template
  [template colour]
  (assoc-in (second template) [1 :class] (str "ln " colour)))

(defn seamless-piece
  [template colour]
  (assoc-in (first template) [1 :class] (str "ln " colour)))

(def bb-heading
  [:h2 
   [:img {:style "width:50px;padding-right:10px;display:inline;"
          :src "./berry.png"}] 
   "The Blueberry Method"])

(def bb-instructions
  [:ol
   [:li "Draw a diagonal line on the wrong side of 4 squares designated for the Blueberry."]
   [:li "Place a Blueberry square RST onto the corners that meet in the middle of 4 selected units. Sew on the diagonal lines. Press two towards the corner, and then press the opposite two away from the corner. This will help to nest the seams together when completing The Blueberry. Trim."]
   [:li "Sew the seams together and your Blueberry will be complete."]])

(defn bb-a
  [berry-b]
  (list
   (sq 1.25 berry-b)
   (draw/ln-d [-0.1 1.35] [1.35 -0.1])))

(defn bb-b
  [berry-b col]
  (list
   (sq 2.5 col)
   (draw/mv [1.25 1.25] (bb-a berry-b))))

(defn bb-c
  [berry-b col]
  (list
   (sq 2.5 col)
   (draw/mv [1.25 1.25] (bb-a berry-b))
   (draw/mv [1.4 1.4] (draw/rot 180 [0.625 0.625] (hst 1.25 "trim"))
            (draw/mv [0.075 0.075] (draw/rot 180 [0.55 0.55] (hst 0.95 berry-b))))))

(defn bb-d
  [berry-f col out?]
  (list
   (sq 2.5 col)
   (draw/mv [1.25 1.25] (draw/rot 180 [0.625 0.625] (hst 1.25 berry-f))
       (if out?
         [:g {:transform "scale(0.625)"} (draw/arw [-0.1 -0.1] [1.35 1.35])]
         [:g {:transform "scale(0.625)"} (draw/arw [1.35 1.35] [-0.1 -0.1])]))))

(defn bb-e
  [berry-f col]
  (list
   (sq 2 col)
   (draw/mv [1.25 1.25] (draw/rot 180 [0.375 0.375] (hst 0.75 berry-f)))))

(defn bb-method
  "Blueberry Method Snippet expecting strings of CSS classes for each colour in the diagram.

  berry-f is the front berry colour.
  berry-b is the back berry colour.
  a is top left square colour.
  b is top right square colour.
  c is bottom right square colour.
  d is bottom left square colour."
  [berry-f berry-b col-a col-b col-c col-d]
  [:div.bb-method
   bb-heading
   bb-instructions
   [:div {:class "figure"}
    (draw/svg [700 160 22.5]
         (draw/mv [0.1 1.25]
             (bb-a berry-b)
             (draw/mv [1.75    0] (draw/rot  90 [0.625 0.625] (bb-a berry-b)))
             (draw/mv [1.75 1.75] (draw/rot 180 [0.625 0.625] (bb-a berry-b)))
             (draw/mv [   0 1.75] (draw/rot 270 [0.625 0.625] (bb-a berry-b))))
         (draw/mv [0.5 5.375] (draw/label "mark on"))
         (draw/mv [0.175 6.25] (draw/label "wrong side"))
         
         (draw/mv [5 0]
             (bb-b berry-b col-a)
             (draw/mv [3 0] (draw/rot  90 [1.25 1.25] (bb-b berry-b col-b)))
             (draw/mv [3 3] (draw/rot 180 [1.25 1.25] (bb-b berry-b col-c)))
             (draw/mv [0 3] (draw/rot 270 [1.25 1.25] (bb-b berry-b col-d))))
         (draw/mv [5.825 6.25] (draw/label "sew diagonals"))
         
         (draw/mv [12.25 0]
             (bb-c berry-b col-a)
             (draw/mv [3 0] (draw/rot  90 [1.25 1.25] (bb-c berry-b col-b)))
             (draw/mv [3 3] (draw/rot 180 [1.25 1.25] (bb-c berry-b col-c)))
             (draw/mv [0 3] (draw/rot 270 [1.25 1.25] (bb-c berry-b col-d))))
         (draw/mv [14.625 6.25] (draw/label "trim"))
         
         (draw/mv [19.5 0]
             (bb-d berry-f col-a false)
             (draw/mv [3 0] (draw/rot  90 [1.25 1.25] (bb-d berry-f col-b true)))
             (draw/mv [3 3] (draw/rot 180 [1.25 1.25] (bb-d berry-f col-c false)))
             (draw/mv [0 3] (draw/rot 270 [1.25 1.25] (bb-d berry-f col-d true))))
         (draw/mv [21.625 6.25] (draw/label "press"))
         
         (draw/mv [26.75 0.5]
             (bb-e berry-f col-a)
             (draw/mv [2 0] (draw/rot  90 [1 1] (bb-e berry-f col-b)))
             (draw/mv [2 2] (draw/rot 180 [1 1] (bb-e berry-f col-c)))
             (draw/mv [0 2] (draw/rot 270 [1 1] (bb-e berry-f col-d))))
         (draw/mv [27.5 6.25] (draw/label "sew seams")))

    [:p "Blueberry Point Method"]]])
