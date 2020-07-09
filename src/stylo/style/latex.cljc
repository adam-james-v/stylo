(ns stylo.style.latex
  (:require [garden.core :refer [css]]
            [garden.stylesheet :refer [at-media]]
            [garden.units :as u]))

(def license-str ""
"
/*!
MIT License

Copyright (c) 2017 davidrzs

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
"/*
latex.css
https://github.com/davidrzs/latexcss
MIT-License
*/

")

(def lmr-font-str (slurp "resources/latin-modern-roman-font.css"))

(def page-setup
  [[:body {:background-color "white"
           :font-size (u/pt 13)
           :font-family "'Latin Modern Roman', serif"}
    {:counter-reset "theorem"}
    {:counter-reset "lemma"}
    {:counter-reset "definition"}]
   [:h1 :h2 :h3 :h4 :h5 :h6 {:border "none"
                             :font-weight "bold"}]
   [:a :a:visited {:color "#a00"}]
   [:ul {:list-style "disc"}]])

(def content-box
  [[:body {:max-width (u/px 720)
           :margin [[(u/em 2) "auto"]]}]
   [:h1:first-of-type {:text-align "center"
                       :display "block"}]])

(def article-body
  [[:body {:text-align "justify"
           :-moz-hyphens "auto"
           :hyphens "auto"
           :padding [[0 (u/em 1)]]}]
   [:dl :dd {:text-align "center"}]])

(def mobile
  [(at-media {:max-width (u/em 43.75)}
             [:body {:padding 0}])])

(def author
  [:.author {:margin-top (u/px 8)
             :margin-bottom (u/px 8)
             :font-variant-caps "small-caps"
             :text-align "center"}])

(def theorem
  [[:.theorem {:counter-increment "theorem"
               :display "block"
               :margin [(u/px 12) 0]
               :font-style "italic"}]
   [:.theorem:before {:content "\"Theorem \" counter(theorem) \".\""
                      :font-weight "bold"
                      :font-style "normal"}]])

(def lemma
  [[:.lemma {:counter-increment "lemma"
             :display "block"
             :margin [(u/px 12) 0]
             :font-style "italic"}]
   [:.lemma:before {:content "\"Lemma \" counter(lemma) \".\""
                    :font-weight "bold"
                    :font-style "normal"}]])

(def definition
  [[:.definition {:counter-increment "definition"
                  :display "block"
                  :margin [(u/px 12) 0]
                  :font-style "normal"}]
   [:.definition:before {:content "\"Definition \" counter(definition) \".\""
                         :font-weight "bold"
                         :font-style "normal"}]])

(def proof
  [[:.proof {:display "block"
             :margin [(u/px 12) 0]
             :font-style "normal"}]
   [:.proof:before {:content "\"Proof. \""
                    :font-style "italic"}]
   [:.proof:after {:content "\"\\25FB\""
                   :float "right"}]])

(def special-classes
  (concat
   author
   theorem
   lemma
   definition
   proof))
(def style-str
  (str
   attrib-str
   lmr-font-str
   (css (concat
         page-setup
         content-box
         article-body
         mobile
         special-classes))))
