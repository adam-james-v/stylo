(ns stylo.fabric
  (:require [clojure.string :as s]
            [hiccup.core :refer [h html]]
            [hiccup.def :refer [defelem]]
            [hiccup.page :as page]
            [hiccup.form :as form]
            [hiccup.element :as elem]
            [stylo.draw :refer :all]))

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
  ([s fabric]
   [:rect {:class ["ln" (if fabric fabric "clr")]
           :width s
           :height s}]))

(defn rct
  ([[x y]]
   (rct [x y] nil))
  ([[x y] fabric]
   [:rect {:class ["ln" (if fabric fabric "clr")]
           :width x
           :height y}]))

(defn hst
  ([s]
   (hst s nil))
  ([s fabric]
   [:polygon {:class ["ln" (if fabric fabric "clr")]
              :points (pt-str [[0 s] [s 0] [0 0]])}]))

(defn hst-pts
  [s]
  [[0 0] [s 0] [0 s]])

(defn diamond
  "draw a diamond of width and height with width offset and height offset factors."
  ([[w h wof hof]]
   (diamond [w h wof hof] nil))
  ([[w h wof hof] fabric]
   (let [wod (* w wof)
         hod (* h hof)]
     [:polygon {:class ["ln" (if fabric fabric "clr")]
                :points (pt-str [[wod 0]
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

(defn dot
  [[x y]]
  [:circle {:class "attn"
            :cx x :cy y :r 0.175}])

(defn stack
  [elem n]
  (let [spc 0.185
        tfrms (map #(vector (* % spc) (* % spc)) (range n))]
    [:g {}
     (map 
      (fn [[x y]] [:g {:transform (translate-str x y)} elem]) 
      tfrms)]))
