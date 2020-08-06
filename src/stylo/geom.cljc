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

(defn distance
  "Computes the distance between two points."
  [a b]
  (let [v (map - b a)
        v2 (apply + (map * v v))]
    (Math/sqrt v2)))

(defn perpendicular
  [[x y]]
  [(- y) x])

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (let [m (Math/sqrt (reduce + (map * v v)))]
    (mapv / v (repeat m))))

(defn det
  [a b]
  (- (* (first a) (second b)) 
     (* (second a) (first b))))

(defn *c3d
  "calculates cross product of two 3d-vectors"
  [a b]
  (let [[a1 a2 a3] a
        [b1 b2 b3] b
        i (- (* a2 b3) (* a3 b2))
        j (- (* a3 b1) (* a1 b3))
        k (- (* a1 b2) (* a2 b1))]
    [i j k]))
