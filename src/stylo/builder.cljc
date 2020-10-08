(ns stylo.builder
  (:require [clojure.string :as s]
            [hiccup.core :refer [h html]]
            [hiccup.def :refer [defelem]]
            [hiccup.page :as page]
            [hiccup.form :as form]
            [hiccup.element :as elem]
            [stylo.parser :refer :all]
            [stylo.style.mu :as mu]))

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
