(ns stylo.style.base
  (:require [garden.core :refer [css]]
            [garden.stylesheet :refer [at-media]]
            [garden.units :as u]))

(def draw
  [[:.ln {:stroke "#2e3440" :stroke-width (u/px 2)}]
   [:.clr {:fill "none"}]
   [:.attn {:fill "rgb(234, 82, 111)"}]
   [:.attn-ln {:stroke "rgb(234, 82, 111)"
               :stroke-width (u/px 3)}]
   [:.face {:fill "#a3be8c"
           :fill-rule "evenodd"}]
   [:rect :line :path :polygon :polyline :circle {:vector-effect "non-scaling-stroke"}]])

(def tables
  [[:table {:width "auto"}]
   [:th :td {:vertical-align "top"
             :padding (u/px 5)
             :border "1px solid #ddd"}]
   [:table [:ul {:list-style-type "none"
                  :padding-left (u/px 4)
                  :margin 0}]]
   [:table [:li:before {:content "▢ "}]]
   [:table [:p {:margin 0}]]])

(def figures
  [[:.figure {:padding-left (u/px 7)}]
   [:.figure [:p {:font-size "smaller"
                  :font-style "italic"}]]])

(def code
  [[:code.block {:padding (u/px 8)
                 :background "#2e3440"
                 :color "#dedee9"
                 :white-space "pre-wrap"
                 :display "inline-block"
                 :width (u/percent 100)}]])

(def hidden
  [[:.hidden {:display "none"}]])

(def pagebreak
  [(at-media {:print ""}
             [:.pagebreak {:page-break-after "always"}])])

(def style
  (concat
   draw
   tables
   figures
   code
   hidden
   pagebreak))

(def style-str
  (css style))
