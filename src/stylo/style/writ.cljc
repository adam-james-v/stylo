(ns stylo.style.writ
  (:require [garden.core :refer [css]]
            [garden.stylesheet :refer [at-media]]
            [garden.color :refer [rgba]]
            [garden.selectors :as s]
            [garden.units :as u]))

(def license-str ""
"
/*!
ISC License

Copyright © 2015, Curtis McEnroe <curtis@cmcenroe.me>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED \"AS IS\" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

")

(def attrib-str "" 
"
/*!
 * Writ v1.0.4
 *
 * Copyright © 2015, Curtis McEnroe <curtis@cmcenroe.me>
 *
 * https://cmcenroe.me/writ/LICENSE (ISC)
 */

")

(def fonts-and-sizes
  [[:html {:font-family "Palatino, Georgia, Lucida Bright, Book Antiqua, serif"
           :font-size (u/px 16)
           :line-height (u/rem 1.5)}]
   [:code :pre :samp :kbd {:font-family "Consolas, Liberation Mono, Menlo, Courier, monospace"
                           :font-size (u/rem 0.833)}]
   [:kbd {:font-weight "bold"}]
   [:h1 :h2 :h3 :h4 :h5 :h6 :th {:font-weight "normal"}]
   [:h1 {:font-size (u/em 2.488)}]
   [:h2 {:font-size (u/em 2.074)}]
   [:h3 {:font-size (u/em 1.728)}]
   [:h4 {:font-size (u/em 1.44)}]
   [:h5 {:font-size (u/em 1.2)}]
   [:h6 {:font-size (u/em 1)}]
   [:small {:font-size (u/em 0.833)}]])

(def heights-and-margins
  [[:h1 :h2 :h3 {:line-height (u/rem 3)}]
   [:p :ul :ol :dl :table :blockquote :pre :h1 :h2 :h3 :h4 :h5 :h6 {:margin [[(u/rem 1.5) 0 0]]}]
   [[:ul :ul] [:ol :ol] [:ul :ol] [:ol :ul] {:margin 0}]
   [:hr {:margin 0
         :border "none"
         :padding [[(u/rem 1.5) 0 0]]}]
   [:table {:line-height "calc(1.5rem - 1px)"
            :margin-bottom (u/px (- 1))}]
   [:pre {:margin-top "calc(1.5rem - 1px)"
          :margin-bottom (u/px (- 1))}]])

(def fonts (concat fonts-and-sizes heights-and-margins))

(def colors
  [[:body {:color "#222"}]
   [:code :pre :samp :kbd {:color "#111"}]
   [:a [:header :nav :a:visited] [:a :code] {:color "#00e"}]
   [:a:visited [:a:visited :code] {:color "#60b"}]
   [:mark {:color "inherit"}]

   [:code :pre :samp :thead :tfoot {:background-color (rgba 0 0 0 0.05)}]
   [:mark {:background-color "#fe0"}]

   [[:main :aside] :blockquote :ins {:border [["solid" (rgba 0 0 0 0.05)]]}]
   [:pre :code :samp {:border [["solid" (rgba 0 0 0 0.1)]]}]
   [:th :td {:border [["solid" "#dbdbdb"]]}]])

(def page-setup
  [[:body {:margin [[(u/rem 1.5) (u/ch 1)]]}]
   [(s/> :body :header) {:text-align "center"}]
   [:main (s/> :body :footer) {:display "block"
                               :max-width (u/ch 78)
                               :margin "auto"}]
   [[:main :figure] [:main :aside] {:float "right"
                                    :margin [[(u/rem 1.5) 0 0 (u/ch 1)]]}]
   [[:main :aside] {:max-width (u/ch 26)
                    :border-width [[0 0 0 (u/ch 0.5)]]
                    :padding [[0 0 0 (u/ch 0.5)]]}]])

(def blocks
  [[:blockquote {:margin-right (u/ch 3)
                 :margin-left (u/ch 1.5)
                 :border-width [[0 0 0 (u/ch 0.5)]]
                 :padding [[0 0 0 (u/ch 1)]]}]
   [:pre {:border-width (u/px 1)
          :border-radius (u/px 2)
          :padding [[0 (u/ch 0.5)]]
          :overflow-x "auto"}]
   [[:pre :code] {:border "none"
                  :padding 0
                  :background-color "transparent"
                  :white-space "inherit"}]
   [:img {:max-width (u/percent 100)}]])

(def lists
  [[:ul :ol :dd {:padding [[0 0 0 (u/ch 3)]]}]
   [:dd {:margin 0}]

   [(s/> :ul :li) {:list-style-type "disc"}]
   [(s/> "li ul" :li) {:list-style-type "circle"}]
   [(s/> "li li ul" :li) {:list-style-type "square"}]

   [(s/> :ol :li) {:list-style-type "decimal"}]
   [(s/> "li ol" :li) {:list-style-type "lower-alpha"}]
   [(s/> "li li ol" :li) {:list-style-type "lower-roman"}]

   [[:nav :ul] {:padding 0
                :list-style-type "none"}]
   [[:nav :ul :li] {:display "inline"
                    :padding-left (u/ch 1)
                    :white-space "nowrap"}]
   [[:nav :ul :li:first-child] {:padding-left 0}]])

(def tables
  [[:table {:width (u/percent 100)
            :border-collapse "collapse"
            :overflow-x "auto"}]
   [:th :td {:border-width (u/px 1)
             :padding [[0 (u/ch 0.5)]]}]])

(def inline
  [[:a {:text-decoration "none"}]
   [:sup :sub {:font-size (u/em 0.75)
               :line-height (u/em 1)}]
   [:ins {:border-width (u/px 1)
          :padding (u/px 1)
          :text-decoration "none"}]
   [:mark {:padding (u/px 1)}]
   [:code :samp {:border-width (u/px 1)
                 :border-radius (u/px 2)
                 :padding [[(u/em 0.1) (u/em 0.2)]]
                 :white-space "nowrap"}]])
