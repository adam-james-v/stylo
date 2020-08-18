(ns stylo.geom)

(defn round
  [num places]
  (let [d (Math/pow 10 places)]
    (/ (Math/round (* num d)) d)))

(defn to-deg
  [rad]
  (* rad (/ 180 Math/PI)))

(defn to-rad
  [deg]
  (* deg (/ Math/PI 180)))

(defn sq
  [x]
  (* x x))

(defn slope
  [a b]
  (let [[x1 y1] a
        [x2 y2] b]
    (/ (- y2 y1) (- x2 x1))))

(defn clamp
  "clamps a value between lower bound and upper bound"
  [x lb ub]
  (cond
    (< x lb) lb
    (> x ub) ub
    :else x))

(defn bb-center
  [pts]
  (let [xs (map first pts)
        ys (map last pts)
        xmax (apply max xs)
        ymax (apply max ys)
        xmin (apply min xs)
        ymin (apply min ys)]
    [(+ (/ (- xmax xmin) 2.0) xmin)
     (+ (/ (- ymax ymin) 2.0) ymin)]))

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (let [m (Math/sqrt (reduce + (map * v v)))]
    (mapv / v (repeat m))))

(defn det
  [a b]
  (- (* (first a) (second b)) 
     (* (second a) (first b))))

(defn *d
  "calculates the dot product of two vectors"
  [a b]
  (reduce + (map * a b)))

(defn *c3d
  "calculates cross product of two 3d-vectors"
  [a b]
  (let [[a1 a2 a3] a
        [b1 b2 b3] b
        i (- (* a2 b3) (* a3 b2))
        j (- (* a3 b1) (* a1 b3))
        k (- (* a1 b2) (* a2 b1))]
    [i j k]))

(defn distance
  "Computes the distance between two points."
  [a b]
  (let [v (map - b a)
        v2 (apply + (map * v v))]
    (Math/sqrt v2)))

(defn perpendicular
  [[x y]]
  [(- y) x])

;; this is broken. the comparison of = with the cross product
;; will basically never be true... need to use a nearly? kind of fn

(defn pt-on-line?
  "determine if a point is on an infinitely extending line"
  [pt line]
  (let [[a b] line
        ap (mapv - a pt)
        bp (mapv - b pt)]
    (= (*c3d ap bp) [0 0 0])))

(defn radius-from-pts
  "compute the radius of an arc defined by 3 points"
  [p1 p2 p3]
  (when-not (pt-on-line? p1 [p2 p3])
    (let [a (distance p3 p2)
          b (distance p3 p1)
          c (distance p2 p1)
          s (/ (+ a b c) 2)
          sa ( - s a)
          sb ( - s b)
          sc ( - s c)
          rt (Math/sqrt (* s sa sb sc))
          R (/ (/ (* a b c) 4) rt)]
      R)))

;; https://math.stackexchange.com/a/1743505
(defn center-from-pts
  "compute the center point of an arc through 3 points"
  [p1 p2 p3]
  (when-not (pt-on-line? p1 [p2 p3])
    (let [u1 (mapv - p2 p1)
          u2 (mapv - p3 p1)
          w1 (*c3d (mapv - p3 p1) u1)
          u (normalize u1)
          w (normalize w1)
          v (*c3d w u)
          [bx by] [(*d u1 u) 0]
          [cx cy] [(*d u2 u) (*d u2 v)]
          h (/ (+ (sq (- cx (/ bx 2))) (sq cy) (- 0 (sq (/ bx 2)))) 
               (* 2 cy))]
      (mapv + p1 
            (mapv * (repeat (/ bx 2)) u) 
            (mapv * (repeat h) v)))))

;; https://math.stackexchange.com/questions/361412/finding-the-angle-between-three-points

(defn angle-from-pts-2d
  [p1 p2 p3]
  (let [v1 (map - p1 p2)
        v2 (map - p3 p2)
        l1 (distance p1 p2)
        l2 (distance p3 p2)
        n (*d v1 v2)
        d (Math/abs (* l1 l2))]
    (to-deg (Math/acos (/ n d)))))
