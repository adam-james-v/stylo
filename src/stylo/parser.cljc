(ns stylo.parser
  (:require [clojure.string :as s]
            [instaparse.core :as insta]
            #?(:cljs 
               [cljs.reader :refer [read-string]])))

;; old kinda working unordered lists.
;; ul = ul-i+ <bl>
;; ul-i = <'- '> #'.+' <nl>?

(def -md ""
"<root> = (hd |
           ul |
           ol |
           code |
           anc |
           img |
           tb |
           ex |
           kl |
           kl-hidden |
           para)+

para = (i-code |
        anc |
        str |
        em |
        para-t)+ <nl> (<nl>+)?

<para-t> = #'[^`\\n*#{}\\[\\]]+'

hd = #'^#{1,} .+' <nl>? <bl>?

str = <'**'> str-t <'**'> 
<str-t> = #'[^\\*\\*]+'

em = <'*'> em-t <'*'>
<em-t> = #'[^\\*]+'

ul = ul-i+ <nl>
ul-i = <nl> <'- '> para-t <nl>?

ol = ol-i+ <bl>
ol-i = <ol-i-token> #'.*' <nl>?
ol-i-token = #'[0-9]+\\. '

i-code = <'`'> #'[^`]+' <'`'>
code = <'~~~'> lang? <nl> code-t <'\n~~~'> <bl>
lang = <' '> #'[a-zA-Z]+'
code-t = #'[^\\n~~~]+'

anc = a-anc | t-anc
<a-anc> = <'<'> url <'>'>
<t-anc> = <'['> text <']'> <'('> url <')'>
<text> = #'[^]]+'
<url> = #'[^>)]+'

img = <nl>? <'!'>
      <'['> alt <']'>
      <'('> path title? <')'> <nl> (<nl>+)?

<alt> = #'[^]]+'
<path> = #'[^) ]+'
<title> = <spcs> #'[^)]+'

spc = ' '
spcs = spc+
bl = #'\n\n'
nl = #'\n' ")

;; doc extensions
(defn gen-ext-str
  [tag]
  (let [main (str tag " = <'{" tag "}'> <nl> " tag "-t <'{" tag "}'> <bl>\n") 
        inner (str "<" tag "-t> = #'([\\s\\S]*?)(?=(\\{" tag "\\}))'")]
    (str main inner)))

(def -ex (gen-ext-str "ex"))
(def -tb (gen-ext-str "tb"))
(def -kl (gen-ext-str "kl"))
(def -kl-hidden (gen-ext-str "kl-hidden"))

;; fix this transform. It doesn't work without a fn being run

(defn transform-ex
  [text]
  (let [results (read-string (str "[" (s/replace text #"\n" "") "]"))
        vals (map eval results)]
    (for [val vals]
      (when (not (var? val))
        [:div val]))))

(defn transform-kl
  [text]
  [:div.cm-container [:code.clj.block text]])

(defn transform-kl-hidden
  [text]
  [:div.hidden [:code.clj.block text]])

(declare ->hiccup)
(declare doc-parse)
;; Transformers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transform-anchor
  ([url] [:a {:href url} url])
  ([text url] [:a {:href url} text]))

(defn transform-emphasis
  [text]
  [:em text])

(defn transform-strong
  [text]
  [:strong text])

(defn transform-pre-code
  ([text] [:pre [:code text]])
  ([lang text] [:pre [:code text]]))

(defn transform-inline-code
  [text]
  [:code text])

(defn transform-image
  ([alt path] [:img {:src path :alt alt}])
  ([alt path title] [:img {:src path :alt alt :title title}]))

(defn transform-unordered-item
  [item]
  [:li item])

(defn transform-unordered-list
  [& items]
  (into [:ul] items))

(defn transform-ordered-item
  [item]
  [:li item])

(defn transform-ordered-list
  [& items]
  (into [:ol] items))

(defn transform-paragraph
  [& items]
  (into [:p] items))

(defn transform-heading
  [text]
  (let [octothorpes (first (s/split text #" "))
        text (s/trim (s/replace text #"#" ""))
        level (count octothorpes)
        tag (keyword (str "h" level))]
    [tag text]))

(defn transform-table
  [text]
  (let [seq (map #(s/split % #"\|") (s/split text #"\|\n"))
        body [:tbody
              (for [row (rest seq)]
                (into [:tr] (mapv #(conj [:td] (->hiccup (doc-parse (str (s/trim %) "\n\n")))) (rest row))))]
        head [:thead
                 (into [:tr]
                       (mapv #(conj [:th] (->hiccup (doc-parse (str (s/trim %) "\n\n")))) (rest (first seq))))]]
    (conj [:table] head body)))

(def doc-parse (insta/parser (str -md -tb -ex -kl -kl-hidden)))

(defn ->hiccup
  [tree]
  (let [transformations {:anc transform-anchor
                         :em transform-emphasis
                         :str transform-strong
                         :img transform-image
                         :tb transform-table
                         :ex transform-ex
                         :kl transform-kl
                         :kl-hidden transform-kl-hidden
                         :code transform-pre-code
                         :i-code transform-inline-code
                         :ul-i transform-unordered-item
                         :ul transform-unordered-list
                         :ol-i transform-ordered-item
                         :ol transform-ordered-list
                         :hd transform-heading
                         :para transform-paragraph}]
    (insta/transform transformations tree)))
