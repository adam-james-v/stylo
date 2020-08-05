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
  [s]
  (mapv read-string (s/split s #",")))

(defn points->str
  [pts]
  (apply str (interpose " " (map pt->str pts))))

(defn str->points
  [s]
  (mapv str->pt (s/split s #" ")))

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

(defn closed-path-str
  [pts]
  (let [line-to #(str "L" (first %) " " (second %))
        move-to #(str "M" (first %) " " (second %))]
    (str 
     (move-to (first pts)) " "
     (apply str (interpose " " (map line-to (rest pts))))
     " Z")))

(defn path->pts
  [s]
  (as-> s s
    (s/replace s #"Z" "") ;; removes Z at end of path
    (s/split s #"\s") ;; split string at spaces
    (mapcat #(s/split % #"[A-Z]") s) ;;splits on alpha chars
    (filter #(not (= % "")) s)
    (mapv read-string s)
    (partition 2 s)))

(defn xf-kv->str
  [[k v]]
  (let [k (symbol k)
        v (apply list v)]
    (str k v)))

(defn str->xf-kv
  [s]
  (let [split (s/split s #"\(")
        key (keyword (first split))
        val (vec (read-string (str "(" (second split))))]
    [key val]))

(defn xf-map->str
  [m]
  (apply str (interpose "\n" (map xf-kv->str m))))

(defn str->xf-map
  [s]
  (if-let [s s]
    (into {} (map str->xf-kv (s/split-lines s)))
    {}))

(def svg-elements 
  #{:circle
    :ellipse
    :line
    :path
    :polygon
    :polyline
    :rect
    :text
    :g})

(defn check-svg-impl
  [multimethod]
  (println "Missing Implementation for:"
           (clojure.set/difference 
            svg-elements
            (into #{} (keys (methods multimethod))))))

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

(defn text
  [text]
  [:text {:x 0 :y 0} text])

(defn g
  [& content]
  [:g {:transform "translate(0 0)"} content])

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

(defmethod translate-element :path
  [[x y] [k props]]
  (let [paths (map path->pts (s/split-lines (:d props)))
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
  [[x y] [k props]]
  (let [new-props (-> props
                      (update :x + x)
                      (update :y + y))]
    [k new-props]))

(defmethod translate-element :text
  [[x y] [k props text]]
  (let [new-props (-> props
                      (update :x + x)
                      (update :y + y))]
    [k new-props text]))

(defmethod translate-element :g
  [[x y] [k props content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :translate #(map + [x y] %)))
        new-props (assoc props :transform (xf-map->str new-xf))]
    [k new-props content]))

(defn element? [item]
  (svg-elements (first item)))

(defn translate
  [[x y] & elems]
  (let [elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (element? elem) (= 0 (count elems)))
        (translate-element [x y] elem)
        
        (and (element? elem) (< 0 (count elems)))
        (concat
         [(translate-element [x y] elem)]
         [(translate [x y] elems)])
      
        :else
        (recur [x y] (concat elem elems))))))

;; this is the 'old' way.
(defn translate-g
  [[x y] & elems]
  (into [:g {:transform (translate-str x y)}] elems))

(defn scale-element
  [[sx sy] [k props content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (xf-map->str new-xf))]
    [k new-props content]))

;; this is the old method
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
