(ns stylo.svg
  (:require [clojure.string :as s]
            [stylo.geom :as g]
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
    (map read-string s)
    (vec (map vec (partition 2 s)))))

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

#?(:clj
   (defn check-svg-impl
     [multimethod]
     (println "Missing Implementation for:"
              (clojure.set/difference 
               svg-elements
               (into #{} (keys (methods multimethod)))))))

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
  [:rect {:width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn text
  [text]
  (let [font-w 9.625
        font-h 10
        n-chars (count text)]
    [:text {:x (/ (* n-chars font-w) -2.0)
            :y (/ font-h 2.0)
            :style {:font-family "monospace"
                    :font-size 16}} text]))

(defn g
  [& content]
  (into [:g {}] content))

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
  [[x y] [k props & content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :translate (fnil #(map + [x y] %) [0 0])))
        new-props (assoc props :transform (xf-map->str new-xf))]
    (into [k new-props] content)))

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

(defn scale-element-by-transform
  [[sx sy] [k props & content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (xf-map->str new-xf))]
    [k new-props] content))

(defmulti scale-element 
  (fn [_ element]
    (first element)))

;; transforms are applied directly to the properties of shapes.
;; I have scale working the same way. One issue is that scaling a circle
;; turns it into an ellipse. This impl WILL change the shape to ellipse if non-uniform scaling is applied.

(defmethod scale-element :circle
  [[sx sy] [k props]]
  (let [circle? (= sx sy)
        r (:r props)
        new-props (if circle?
                    (assoc props :r (* r sx))
                    (-> props
                        (dissoc :r)
                        (assoc :rx (* sx r))
                        (assoc :ry (* sy r))))
        k (if circle? :circle :ellipse)]
    [k new-props]))

(defmethod scale-element :ellipse
  [[sx sy] [k props]]
  (let [new-props (-> props
                      (update :rx #(* sx %))
                      (update :ry #(* sy %)))]
    [k new-props]))

;; find bounding box center
;; translate bb-center to 0 0
;; scale all x y values by * [sx sy]
;; translate back to original bb-center

(defmethod scale-element :line
  [[sx sy] [k props]]
  (let [[cx cy] (g/bb-center [[(:x1 props) (:y1 props)]
                              [(:x2 props) (:y2 props)]])
        new-props (-> props
                      (update :x1 #(+ (* (- % cx) sx) cx))
                      (update :y1 #(+ (* (- % cy) sy) cy))
                      (update :x2 #(+ (* (- % cx) sx) cx))
                      (update :y2 #(+ (* (- % cy) sy) cy)))]
    [k new-props]))

(defn scale-pt-from-center
  [[cx cy] [sx sy] [x y]]
  [(+ (* (- x cx) sx) cx)
   (+ (* (- y cy) sy) cy)])

(defmethod scale-element :path
  [[sx sy] [k props]]
  (let [paths (map path->pts (s/split-lines (:d props)))
        center (g/bb-center (apply concat paths))
        new-paths (for [path paths] 
                    (closed-path-str 
                     (map 
                      (partial scale-pt-from-center center [sx sy])
                      path)))
        new-props (assoc props :d (apply str (interpose "\n" new-paths)))]
    [k new-props]))

(defmethod scale-element :polygon
  [[sx sy] [k props]]
  (let [points (str->points (:points props))
        center (g/bb-center points)
        new-points (points->str
                    (map 
                     (partial scale-pt-from-center center [sx sy])
                     points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod scale-element :polyline
  [[sx sy] [k props]]
  (let [points (str->points (:points props))
        center (g/bb-center points)
        new-points (points->str
                    (map 
                     (partial scale-pt-from-center center [sx sy])
                     points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod scale-element :rect
  [[sx sy] [k props]]
  (let [cx (+ (:x props) (/ (:width props) 2.0))
        cy (+ (:y props) (/ (:height props) 2.0))
        w (* sx (:width props))
        h (* sy (:height props))
        new-props (-> props
                      (assoc :width w)
                      (assoc :height h)
                      (update :x #(+ (* (- % cx) sx) cx))
                      (update :y #(+ (* (- % cy) sy) cy)))]
    [k new-props]))

(defmethod scale-element :text
  [[sx sy] [k props text]]
  (let [font-w 9.625
        font-h 10
        n-chars (count text)
        cx (+ (:x props) (/ (* font-w n-chars) 2.0))
        cy (+ (:y props) (/ font-h -2.0))
        new-props (-> props
                      (update-in [:style :font-size] #(* % sx))
                      (update :x #(+ (* (- % cx) sx) cx))
                      (update :y #(+ (* (- % cy) sy) cy)))]
    [k new-props text]))

(defmethod scale-element :g
  [[sx sy] [k props & content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (xf-map->str new-xf))]
    (into [k new-props] content)))

(defn scale
  [sc & elems]
  (let [[sx sy] (if (coll? sc) sc [sc sc])
        elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (element? elem) (= 0 (count elems)))
        (scale-element [sx sy] elem)
        
        (and (element? elem) (< 0 (count elems)))
        (concat
         [(scale-element [sx sy] elem)]
         [(scale [sx sy] elems)])
        
        :else
        (recur [sx sy] (concat elem elems))))))

;; this is the old method
(defn scale-g
  [sc & elems]
  (into [:g {:transform (scale-str sc)}] elems))

(defn rotate-element-by-transform
  ([deg [k props content]]
   (rotate-element-by-transform deg [0 0] [k props content]))
  
  ([deg center [k props content]]
   (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
         new-xf (-> xf
                    (update-in [:rotate 0] + deg)
                    (assoc-in [:rotate 1] (first center))
                    (assoc-in [:rotate 2] (second center)))
         new-props (assoc props :transform (xf-map->str new-xf))]
     [k new-props content])))

(defn rotate-pt
  [deg [x y]]
  (let [c (Math/cos (g/to-rad deg))
        s (Math/sin (g/to-rad deg))]
    [(- (* x c) (* y s))
     (+ (* x s) (* y c))]))

(defmulti rotate-element
  (fn [_ element]
    (first element)))

(defmethod rotate-element :circle
  [deg [k props]]
  (rotate-element-by-transform deg [k props]))

(defmethod rotate-element :ellipse
  [deg [k props]]
  (rotate-element-by-transform deg [k props]))

(defn move-pt
  [mv pt]
  (mapv + pt mv))

(defn rotate-pt-around-center
  [deg center pt]
  (->> pt
       (move-pt (map - center))
       (rotate-pt deg)
       (move-pt center)))

(defmethod rotate-element :line
  [deg [k props]] 
  (let [pts [[(:x1 props) (:y1 props)] [(:x2 props) (:y2 props)]]
        center (g/bb-center pts)
        [[x1 y1] [x2 y2]]  (map (partial rotate-pt-around-center deg center) pts)
        new-props (-> props
                      (assoc :x1 x1)
                      (assoc :y1 y1)
                      (assoc :x2 x2)
                      (assoc :y2 y2))]
    [k new-props]))

(defmethod rotate-element :path
  [deg [k props]]
  (let [paths (map path->pts (s/split-lines (:d props)))
        center (g/bb-center (apply concat paths))
        new-paths (for [path paths] 
                    (closed-path-str 
                     (map 
                      (partial rotate-pt-around-center deg center) 
                      path)))
        new-props (assoc props :d (apply str (interpose "\n" new-paths)))]
    [k new-props]))

(defmethod rotate-element :polygon
  [deg [k props]]
  (let [points (str->points (:points props))
        center (g/bb-center points)
        new-points (points->str
                    (map 
                     (partial rotate-pt-around-center deg center)
                     points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod rotate-element :polyline
  [deg [k props]]
  (let [points (str->points (:points props))
        center (g/bb-center points)
        new-points (points->str
                    (map 
                     (partial rotate-pt-around-center deg center)
                     points))
        new-props (assoc props :points new-points)]
    [k new-props]))

(defmethod rotate-element :rect
  [deg [k props]]
  (let [cx (+ (:x props) (/ (:width props) 2.0))
        cy (+ (:y props) (/ (:height props) 2.0))]
    (rotate-element-by-transform deg [cx cy] [k props])))

;; old approach
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
