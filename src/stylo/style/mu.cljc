(ns stylo.style.mu
  (:require [garden.core :refer [css]]
            [garden.stylesheet :refer [at-media]]
            [garden.units :as u]
            [stylo.style.base :as base]))

(def license-str ""
"
/*!
MIT License

Copyright (c) 2016 Fabien Sa

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

")

(def attrib-str "" 
"
/*
| The mu framework v0.3.1
| BafS 2016-2018
*/

")

(def fonts
  [[:* :*:after :*:before {:box-sizing "border-box"}]
   [:body {:font "18px/1.6 Georgia, \"Times New Roman\", Times, serif"}]
   [:table :input {:font-size (u/px 16)}]
   [:button :select {:font-size (u/px 14)}]
   [:h1 :h2 :h3 :h4 :h5 :h6 {:font-family "\"Helvetica Neue\", Helvetica, Arial, sans-serif"
                             :line-height 1.2}]])

(def page-setup
  [[:body {:margin [[(u/px 40) "auto"]]
           :max-width (u/px 760)
           :color "#444"
           :padding [[0 (u/px 20)]]}]
   [:a {:color "#07c"
        :text-decoration "none"}]
   [:a:hover {:color "#059"
              :text-decoration "underline"}]
   [:hr {:border 0
         :margin [[(u/px 25) 0]]}]])

(def tables
  [[:table {:border-spacing 0
            :border-collapse "collapse"
            :text-align "left"
            :padding-bottom (u/px 25)}]
   [:td :th {:padding (u/px 5)
             :vertical-align "bottom"}]
   [:td :th :hr {:border-bottom [[(u/px 1) "solid" "#ddd"]]}]])

(def blocks
  [[:pre {:padding (u/px 8)
          :white-space "pre-wrap"}]])

(def inputs
  [[:button :select {:background "#ddd"
                     :border 0
                     :padding [[(u/px 9) (u/px 20)]]}]
   [:input {:padding (u/px 5)
            :vertical-align "bottom"}]
   [:button:hover {:background "#eee"}]
   [:textarea {:border-color "#ccc"}]])

(def grid
  [[:.row {:display "block"
           :width "auto"
           :min-height (u/px 1)}]
   [:.row:after {:content "\"\""
                 :display "table"
                 :clear "both"}]
   [:.row :.c {:float "left"}]
   [:table :.g2 :.g3 :.g3-2 :.m2 :.m3 :.m3-2 {:width (u/percent 100)}]
   
   [(at-media {:min-width (u/px 768)})
    [:.g2 {:width (u/percent 50)}]
    [:.m2 {:margin-left (u/percent 50)}]
    [:.g3 {:width (u/percent 33.33)}]
    [:.g3-2 {:width (u/percent 66.66)}]
    [:.m3 {:margin-left (u/percent 33.33)}]
    [:.m3-2 {:margin-left (u/percent 66.66)}]]])

(def style-str
  (str
   license-str
   attrib-str
   (css (concat
         fonts
         base/style
         page-setup
         tables
         blocks
         inputs
         grid))))
