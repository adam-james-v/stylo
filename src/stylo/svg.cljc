(ns stylo.svg
  (:require [clojure.string :as s]
            #?(:cljs 
               [cljs.reader :refer [read-string]])))

(defn svg
  [[w h sc] & content]
  [:svg {:width w
         :height h
         :ViewBox (str "-1 -1 " w " " h)
         :xmlns "http://www.w3.org/2000/svg"}
   [:g {:transform (str "scale(" sc ")")} content]])

(defn pt->str
  [pt]
  (apply str (interpose "," pt)))

(defn str->pt
  [str]
  (mapv read-string (s/split str #",")))

(defn points->str
  [pts]
  (apply str (interpose " " (map pt->str pts))))

(defn str->points
  [str]
  (mapv str->pt (s/split str #" ")))

(defn closed-path-str
  [pts]
  (let [line-to #(str "L" (first %) " " (second %))
        move-to #(str "M" (first %) " " (second %))]
    (str 
     (move-to (first pts)) " "
     (apply str (interpose " " (map line-to (rest pts))))
     " Z")))

(defn closed-path->pts
  [str]
  (as-> str s
    (s/split s #"\s")
    (drop-last s)
    (mapcat #(s/split % #"\D") s)
    (filter #(not (= % "")) s)
    (mapv read-string s)
    (partition 2 s)))

(defn scale-str
  [sc]
  (str "scale(" sc ")"))

(defn translate-str
  [x y]
  (str "translate(" x " " y ")"))

(defn rotate-str
  ([r]
   (str "rotate(" r ")"))
  ([r [x y]]
   (str "rotate(" r " " x " " y ")")))

(defn hsl-str
  [h s l]
  (str "hsl(" h ", " s "%, " l "%)"))

;; list of svg shapes:
;;
;; circle
;; ellipse
;; line
;; path
;; polygon
;; polyline
;; rect
;; text

(defn circle
  [r]
  [:circle {:cx 0 :cy 0 :r r}])

(defn ellipse
  [rx ry]
  [:ellipse {:cx 0 :cy 0 :rx rx :ry ry}])

(defn line
  [[ax ay] [bx by]]
  [:line {:x1 ax :y1 ay :x2 bx :y2 by}])

(defn path
  [d]
  [:path {:d d}])
  
(defn closed-path
  [& paths]
  (let [paths (map closed-path-str paths)
        d (apply str (interpose "\n" paths))]
    (path d)))

(defn polygon
  [pts]
  [:polygon {:points (points->str pts)}])

(defn polyline
  [pts]
  [:polyline {:points (points->str pts)}])

(defn rect
  [w h]
  [:rect {:width w :height h :x 0 :y 0}])

(defmulti translate-element 
  (fn [_ element]
    (first element)))

(defmethod translate-element :circle
  [[x y] [k props]]
  (let [new-props (-> props
                      (update :cx + x)
                      (update :cy + y))]
    [k new-props]))

(defmethod translate-element :ellipse
  [[x y] [k props]]
  (let [new-props (-> props
                      (update :cx + x)
                      (update :cy + y))]
    [k new-props]))

(defmethod translate-element :line
  [[x y] [k props]]
  (let [new-props (-> props
                      (update :x1 + x)
                      (update :y1 + y)
                      (update :x2 + x)
                      (update :y2 + y))]
    [k new-props]))

;; path translate doesn't work with multiple paths yet.
;; easy to fix, but I have to make the change still.

(defmethod translate-element :path
  [[x y] [k props]]
  (let [paths (map closed-path->pts (s/split-lines (:d props)))
        new-paths (for [path paths] 
                    (closed-path-str (map #(map + [x y] %) path)))
        new-props (assoc props :d (apply str (interpose "\n" new-paths)))]
    [k new-props]))

(defmethod translate-element :polygon
  [[x y] [k props]]
  (let [points (str->points (:points props))
        new-points (points->str (map #(map + [x y] %) points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod translate-element :polyline
  [[x y] [k props]]
  (let [points (str->points (:points props))
        new-points (points->str (map #(map + [x y] %) points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod translate-element :rect
  [[x y] elem]
  (let [props (second elem)
        new-props (-> props
                      (update :x + x)
                      (update :y + y))]
    [:rect new-props]))
  
(defn translate
  [[x y] & elems]
  (into [:g {:transform (translate-str x y)}] elems))

(defn scale
  [sc & elems]
  (into [:g {:transform (scale-str sc)}] elems))

(defn rotate
  [r [x y] & elems]
  (into [:g {:transform (rotate-str r [x y])}] elems))

(defn label
  [text]
  [:text {:fill "black"
          :x -4
          :y 4
          :font-family "Verdana"
          :font-size 12
          :transform "translate(0,0) scale(0.05)"} text])

(defn arrow
  [a b]
  [:g {}
   [:marker {:id "head"
             :orient "auto"
             :markerWidth "0.5"
             :markerHeight "1"
             :refX "0.025"
             :refY "0.25"}
    [:path {:d "M0,0 V0.5 L0.25,0.25 Z"
            :fill "black"}]]
   [:polyline {:marker-end "url(#head)"
               :stroke "black"
               :stroke-width "2"
               :fill "rgba(0,0,0,0)"
               :points (points->str [a b])}]])
