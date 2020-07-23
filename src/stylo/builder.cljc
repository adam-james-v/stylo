(ns stylo.builder
  (:require [clojure.string :as s]
            [hiccup.core :refer [h html]]
            [hiccup.def :refer [defelem]]
            [hiccup.page :as page]
            [hiccup.form :as form]
            [hiccup.element :as elem]
            [stylo.style.mu :as mu]
            [instaparse.core :as insta]))

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
<para-t> = #'[^`\\n*#{}\\-\\!\\[\\]]+'

hd = #'^#{1,} .+' <nl>? <bl>?

str = <'**'> str-t <'**'> 
<str-t> = #'[^\\*\\*]+'

em = <'*'> em-t <'*'>
<em-t> = #'[^\\*]+'

ul = ul-i+ <bl>
ul-i = <'- '> #'.+' <nl>?

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

img = <'!'>
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

(defn md->html
  "Parses markup into HTML."
  [markup]
  (html (->hiccup (doc-parse markup))))

(defn discern-title
  [markup]
  (let [t (first (s/split-lines markup))]
    (-> t
        (s/replace #"#" "")
        (s/trim))))

(defn md->page
  "compiles markup into a valid HTML5 string."
  [markup]
  (s/replace
   (hiccup.page/html5
    (concat [[:head
              [:meta {:charset "utf-8"}]
              [:title (discern-title markup)]
              [:style mu/style-str]]]
            [[:body [:main (->hiccup (doc-parse markup))]]]))
   #"><" ">\n<"))

(def klipse-settings "
window.klipse_settings = {
  selector: '.clj',
  codemirror_options_in: {
    lineWrapping: true,
    theme: 'nord',
  },
  codemirror_options_out: {
    lineWrapping: true,
    theme: 'nord',
  }
}
")

(def codemirror-style "
.cm-container {
  box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.4), 0 4px 6px -2px rgba(0, 0, 0, 0.05);
  font-size: 10pt;
  margin: 0 auto;
  max-width: 520px;
}
.CodeMirror {
  border: none;
  padding: 8px;
}
")

(defn contains-klipse?
  [markup]
  (s/includes? markup "{kl}"))

(defn md->klipse
  [markup]
  (s/replace
   (hiccup.page/html5
    (concat [[:head
              [:meta {:charset "utf-8"}]
              [:title (discern-title markup)]
              [:style mu/style-str]]]
            [[:body
              [:main (->hiccup (doc-parse markup))]
              (when (contains-klipse? markup) (list
                [:link {:rel "stylesheet"
                        :type "text/css"
                        :href "https://unpkg.com/klipse@7.9.6/dist/codemirror.css"}]
                [:link {:rel "stylesheet"
                        :type "text/css"
                        :href "https://codemirror.net/theme/nord.css"}]
                [:style codemirror-style]
                [:script klipse-settings]
                [:script {:src "https://unpkg.com/klipse@7.9.6/dist/klipse_plugin.js"}]))]]))
   #"><" ">\n<"))

(defn get-name
  [fpath]
  (first (s/split (last (s/split fpath #"/")) #"\.")))

(defn get-path
  [fpath]
  (let [fname (last (s/split fpath #"/"))]
    (s/replace fpath fname "")))

(defn -main [fpath]
  (let [markup (slurp fpath)
        name (get-name fpath)
        opath (get-path fpath)
        fname (str name ".html")]
    (do 
      (spit (str opath fname) (md->klipse markup))
      (println (str "created: " fname)))))
