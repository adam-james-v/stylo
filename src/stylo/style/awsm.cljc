(ns stylo.style.awsm
  (:require [garden.core :refer [css]]
            [garden.stylesheet :refer [at-media]]
            [garden.selectors :as s]
            [garden.units :as u]))

(def license-str ""
"
/*!
MIT License

Copyright (c) 2015 Igor Adamenko http://igoradamenko.com

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
/*!
 * awsm.css v3.0.4 (https://igoradamenko.github.io/awsm.css/)
 * Copyright 2015 Igor Adamenko <mail@igoradamenko.com> (https://igoradamenko.com)
 * Licensed under MIT (https://github.com/igoradamenko/awsm.css/blob/master/LICENSE.md)
 */

")

(def fonts
  [[:html {:font-family "system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell, \"PT Sans\", \"Open Sans\", \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Helvetica, Arial, sans-serif"
           :font-size (u/percent 100)
           :line-height 1.4
           :background "white"
           :color "black"
           :-webkit-overflow-scrolling "touch"}]])

(def body-sizing
  [[:body {:margin (u/em 1.2)
           :font-size (u/rem 1)}]
   [(at-media {:min-width (u/rem 20)}) [:body {:font-size "calc(1rem + 0.00625 * (100vw - 20rem))"}]]
   [(at-media {:min-width (u/rem 40)}) [:body {:font-size (u/rem 1.125)}]]
   [[:body :header] [:body :footer] [:body :article] {:position "relative"
                                                      :max-width (u/rem 40)
                                                      :margin [[0 auto]]}]
   [(s/> :body :header) {:margin-bottom (u/em 3.5)}]
   [(s/> :body "header h1") {:margin 0
                             :font-size (u/em 1.5)}]
   [(s/> :body "header p") {:margin 0
                            :font-size (u/em 0.85)}]
   [(s/> :body :footer) {:margin-top (u/em 6)
                         :padding-bottom (u/em 1.5)
                         :text-align "center"
                         :font-size (u/rem 0.8)
                         :color "#aaaaaa"}]])

(def sections
  [[(s/+ :section :section) {:margin-top (u/em 2)}]
   [(s/+ :article :article) {}]
   [[:article :header :p] {}]
   [[:article :header (s/+ :p :h1)] [:article :header (s/+ :p :h2)] {:margin-top (u/em (- 0.25))}]
   [[:article :header (s/+ :h1 :p)] [:article :header (s/+ :h2 :p)] {:margin-top (u/em 0.25)}]
   [[:article :header :h1 :a] [:article :header :h2 :a] {:color "black"}]
   [[:article :header :h1 :a:visited] [:article :header :h2 :a:visited] {:color "#aaaaaa"}]
   [[:article :header :h1 :a:visited:hover] [:article :header :h2 :a:visited:hover] {:color "#f00000"}]
   [(s/> :article :footer) {:margin-top (u/em 1.5)
                            :font-size (u.em 0.85)}]])

(def nav
  [[:nav {:margin [[(u/em 1) 0]]}]
   [[:nav :ul] {:list-style "none"
                :margin 0
                :padding 0}]
   [[:nav :li] {:display "inline-block"
                :margin-right (u/em 1)
                :margin-bottom (u/em 0.25)}]
   [[:nav :a:visited] {:color "#0064cl"}]
   [[:nav :a:hover] {:color "#f00000"}]])

(def lists
  [[:ul :ol {:margin-top 0
             :padding-top 0
             :padding-left (u/em 2.5)}]
   [(s/+ "ul li" :li) (s/+ "ol li" :li) {:margin-top (u/em 0.25)}]
   [(s/> "ul li" :details) (s/> "ol li" :details) {:margin 0}]
   [(s/+ :p :ul) (s/+ :p :ol) {:margin-top (u/em (- 0.75))}]])

(def headings
  [[:h1 :h2 :h3 :h4 :h5 :h6 {:margin [[(u/em 1.25) 0 0]]
                             :line-height 1.2}]
   [:h1 {:font-size (u/em 2.5)}]
   [:h2 {:font-size (u/em 1.75)}]
   [:h3 {:font-size (u/em 1.25)}]
   [:h4 {:font-size (u/em 1.15)}]
   [:h5 {:font-size (u/em 1)}]
   [:h6 {:font-size (u/em 1)
         :margin-top (u/em 1)
         :color "#aaaaaa"}]])

(def paragraphs
  [[:p {:margin [[(u/em 1) 1]]
        :-webkit-hyphens "auto"
        :-ms-hyphens "auto"
        :hyphens "auto"}]
   [:p:first-child {:margin-top 0}]
   [:p:last-child {:margin-bottom 0}]])

(def page-setup
  (concat body-sizing
          sections
          nav
          lists
          headings
          paragraphs))

(def links
  [[:a {:color "#0064cl"}]
   [:a:visited {:color "#8d39d0"}]
   [:a:hover :a:active {:outline-width 0}]
   [:a:hover {:color "#f00000"}]
   [[:a :abbr] {:font-size (u/em 1)}]])

(def images-and-figures
  [[:img :picture {:display "block"
                   :max-width (u/percent 100)
                   :margin [[0 "auto"]]}]
   [:audio :video {:width (u/percent 100)
                   :max-width (u/percent 100)}]
   [:figure {:margin [[(u/em 1) 0 (u/em 0.5)]]
             :padding 0}]
   [(s/+ :figure :p) {:margin-top (u/em 0.5)}]
   [[:figure :figcaption] {:opacity 0.65
                           :font-size (u/em 0.85)}]
   [[:p :img] [:p :picture] {:float "right"
                             :margin-bottom (u/em 0.5)
                             :margin-left (u/em 0.5)}]
   [[:p :picture :img] {:float "none"
                        :margin 0}]])

(def block-elements
  [[:dd {:margin-bottom (u/em 1)
         :margin-left 0
         :padding-left (u/em 2.5)}]
   [:dt {:font-weight 700}]
   [:blockquote {:margin 0
                 :padding-left (u/em 2.5)}]
   [:aside {:margin [[(u/em 0.5) 0]]}]
   [(at-media {:min-width (u/rem 65)})
    [:aside {:position "absolute"
             :right (u/rem (- 12.5))
             :width (u/rem 9.375)
             :max-width (u/rem 9.375)
             :margin 0
             :padding-left (u/em 0.5)
             :font-size (u/em 0.8)
             :border-left [[(u/px 1) "solid" "#f2f2f2"]]}]]
   [:aside:first-child {:margin-top 0}]
   [:aside:last-child {:margin-bottom 0}]
   [:abbr {:margin-right (u/em (- 0.075))
           :text-decoration "none"
           :-webkit-hyphens "none"
           :-ms-hyphens "none"
           :hyphens "none"
           :letter-spacing (u/em 0.075)
           :font-size (u/em 0.9)}]
   [:code :kbd :var :samp {:font-family "Consolas, \"Lucida Console\", Monaco, monspace"
                           :font-style "normal"}]
   [:pre {:overflow-x "auto"
          :font-size (u/em 0.8)
          :background "rgba(0,0,0,0.15)"
          :background-attachment "scroll scroll"
          :background-size "1px 100%, 1px 100%"
          :background-repeat "no-repeat, no-repeat"}]
   [(s/> :pre :code) {:display "inline-block"
                      :overflow-x "visible"
                      :box-sizing "border-box"
                      :min-width (u/percent 100)
                      :border-right [[(u/px 3) "solid" "white"]]
                      :border-left [[(u/px 1) "solid" "white"]]}]
   [:hr {:height (u/px 1)
         :margin [[(u/em 2) 0]]
         :border 0
         :background "#f2f2f2"}]])

(def blocks
  (concat links
          images-and-figures
          block-elements))

(def tables
  [[:table {:display "inline-block"
            :border-spacing 0
            :border-collapse "collapse"
            :overflow-x "auto"
            :max-width (u/percent 100)
            :text-align "left"
            :vertial-align "top"
            :background "rgba(0,0,0,0.15)"
            :background-attachment "scroll scroll"
            :background-size "1px 100%, 1px 100%"
            :background-repeat "no-repeat, no-repeat"}]
   [[:table :th] {:line-height 1.2}]
   [[:table :caption] {:font-size (u/em 0.9)
                       :background "white"}]
   [[:table :td] [:table :th] {:padding [[(u/em 0.35) (u/em 0.75)]]
                               :vertical-align "top"
                               :font-size (u/em 0.9)
                               :border [[(u/px 1) "solid" "#f2f2f2"]]
                               :border-top 0
                               :border-left 0}]
   [[:table :td:first-child] [:table :th:first-child] {:padding-left 0}]
   [[:table :td:last-child] [:table :th:last-child] {:padding-right 0
                                                     :border-right 0}]])

(def form-elements
  [[:form {:margin-right "auto"
           :margin-left "auto"}]
   [(at-media {:min-width (u/rem 40)})
    [:form {:max-width (u/percent 80)}]]
   [[:form :select] [:form :label] {:display "block"}]
   [[:form "label:not(:first-child)"] {:margin-top (u/em 1)}]
   [[:form :p :label] {:display "inline"}]
   [[:form :p (s/+ :label :label)] {:margin-left (u/em 1)}]
   [[:form (s/+ :legend:first-child :label)] {:margin-top 0}]
   [[:form :select] [:form "input[type]"] [:form :textarea] {:margin-bottom (u/em 1)}]
   [[:form "input[type=checkbox]"] [:form "input[type=radio]"] {:margin-bottom 0}]
   [:fieldset {:margin 0
               :padding [[(u/em 0.5) (u/em 1)]]
               :border [[(u/px 1) "solid" "#aaaaaa"]]}]
   [:legend {:color "#aaaaaa"}]
   [:button :select {:outline "none"
                     :box-sizing "border-box"
                     :height (u/em 2)
                     :margin 0
                     :padding [["calc(0.25em - 1px)" (u/em 0.5)]]
                     :font-family "inherit"
                     :font-size (u/em 1)
                     :border [[(u/px 1) "solid" "#aaaaaa"]]
                     :border-radius (u/px 2)
                     :background "#f2f2f2"
                     :color "black"
                     :display "inline-block"
                     :width "auto"
                     :cursor "pointer"}]
   [:button:focus {:border [[(u/px 1) "solid" "black"]]}]
   [:button:hover {:border [[(u/px 1) "solid" "black"]]}]
   [:button:active {:background-color "#aaaaaa"}]
   [:select {:padding-right (u/em 1.2)
             :background-position [["top" (u/percent 55) "right" (u/em 0.35)]]
             :background-size (u/em 0.5)
             :-webkit-appearance "button"
             :-moz-appearance "button"
             :appearance "button"}]
   [:select:focus {:border [[(u/px 1) "solid" "black"]]}]
   [:select:hover {:border [[(u/px 1) "solid" "black"]]}]
   [:select:active {:background-color "#aaaaaa"}]
   [:textarea {:outline "none"
               :box-sizing "border-box"
               :margin 0
               :padding [["calc(0.25em - 1px)" (u/em 0.5)]]
               :font-family "inherit"
               :font-size (u/em 1)
               :border [[(u/px 1) "solid" "#aaaaaa"]]
               :border-radius (u/px 2)
               :background "white"
               :color "black"
               :display "block"
               :width (u/percent 100)
               :line-height "calc(2em - 1px * 2 - (0.25em - 1px) * 2)"
               :-webkit-appearance "none"
               :-moz-appearance "none"
               :appearance "none"
               :height (u/em 4.5)
               :resize "vertical"
               :padding-top (u/em 0.5)
               :padding-bottom (u/em 0.5)}]
   [:textarea:focus {:border [[(u/px 1) "solid" "black"]]}]
   ["textarea::-moz-placeholder"
    "textarea::-webkit-input-placeholder"
    "textarea::-ms-input-placeholder" {:color "#aaaaaa"}]
   [:output {:display "block"}]])


(def inputs
  [["input[type=text]"
    "input[type=password]"
    "input[type^=date]"
    "input[type=email]"
    "input[type=number]"
    "input[type=search]"
    "input[type=tel]"
    "input[type=time]"
    "input[type=month]"
    "input[type=week]"
    "input[type=url]"
    "input[type=color]"
    {:outline "none"
     :box-sizing "border-box"
     :height (u/em 2)
     :margin 0
     :padding [["calc(0.25em - 1px)" (u/em 0.5)]]
     :font-family "inherit"
     :font-size (u/em 1)
     :border [[(u/px 1) "solid" "#aaaaaa"]]
     :border-radius (u/px 2)
     :background "white"
     :color "black"
     :display "block"
     :width (u/percent 100)
     :line-height "calc(2em - 1px * 2 - (0.25em - 1px) * 2)"
     :-webkit-appearance "none"
     :-moz-appearance "none"
     :appearance "none"}]
   ["input[type=text]:focus"
    "input[type=password]:focus"
    "input[type^=date]:focus"
    "input[type=email]:focus"
    "input[type=number]:focus"
    "input[type=search]:focus"
    "input[type=tel]:focus"
    "input[type=time]:focus"
    "input[type=month]:focus"
    "input[type=week]:focus"
    "input[type=url]:focus"
    "input[type=color]:focus"
    "input[type=submit]:focus"
    "input[type=button]:focus"
    "input[type=reset]:focus"
    "input[type=file]:focus"
    "input[type=color]:hover"
    "input[type=submit]:hover"
    "input[type=button]:hover"
    "input[type=reset]:hover"
    "input[type=file]:hover"
    {:border [[(u/px 1) "solid" "black"]]}]
   ["input[type=text]::-moz-placeholder"
    "input[type=password]::-moz-placeholder"
    "input[type^=date]::-moz-placeholder"
    "input[type=email]::-moz-placeholder"
    "input[type=number]::-moz-placeholder"
    "input[type=search]::-moz-placeholder"
    "input[type=tel]::-moz-placeholder"
    "input[type=time]::-moz-placeholder"
    "input[type=month]::-moz-placeholder"
    "input[type=week]::-moz-placeholder"
    "input[type=url]::-moz-placeholder"
    {:color "#aaaaaa"}]
   ["input[type=text]::-webkit-input-placeholder"
    "input[type=password]::-webkit-input-placeholder"
    "input[type^=date]::-webkit-input-placeholder"
    "input[type=email]::-webkit-input-placeholder"
    "input[type=number]::-webkit-input-placeholder"
    "input[type=search]::-webkit-input-placeholder"
    "input[type=tel]::-webkit-input-placeholder"
    "input[type=time]::-webkit-input-placeholder"
    "input[type=month]::-webkit-input-placeholder"
    "input[type=week]::-webkit-input-placeholder"
    "input[type=url]::-webkit-input-placeholder"
    "input[type=color]::-webkit-input-placeholder"
    {:color "#aaaaaa"}]
   ["input[type=text]::-ms-input-placeholder"
    "input[type=password]::-ms-input-placeholder"
    "input[type^=date]::-ms-input-placeholder"
    "input[type=email]::-ms-input-placeholder"
    "input[type=number]::-ms-input-placeholder"
    "input[type=search]::-ms-input-placeholder"
    "input[type=tel]::-ms-input-placeholder"
    "input[type=time]::-ms-input-placeholder"
    "input[type=month]::-ms-input-placeholder"
    "input[type=week]::-ms-input-placeholder"
    "input[type=url]::-ms-input-placeholder"
    "input[type=color]::-ms-input-placeholder"
    {:color "#aaaaaa"}]
   ["input[type=submit]"
    "input[type=button]"
    "input[type=reset]"
    "input[type=file]"
    {:outline "none"
     :box-sizing "border-box"
     :height (u/em 2)
     :margin 0
     :padding [["calc(0.25em - 1px)" (u/em 0.5)]]
     :font-family "inherit"
     :font-size (u/em 1)
     :border [[(u/px 1) "solid" "#aaaaaa"]]
     :border-radius (u/px 2)
     :background "f2f2f2"
     :color "black"
     :display "inline-block"
     :width "auto"
     :cursor "pointer"
     :-webkit-appearance "none"
     :-moz-appearance "none"
     :appearance "none"}]
   ["input[type=submit]:active"
    "input[type=button]:active"
    "input[type=reset]:active"
    "input[type=file]:active"
    {:background-color "#aaaaaa"}]
   ["input[type=file]" {:width (u/percent 100)
                        :height "auto"
                        :padding [[(u/em 0.75) (u/em 0.5)]]
                        :font-size (u/px 12)
                        :line-height 1}]])

(def forms
  (concat form-elements
          inputs))
(def style-str
  (str
   license-str
   attrib-str
   (css (concat
         fonts
         page-setup
         blocks
         tables
         forms))))
