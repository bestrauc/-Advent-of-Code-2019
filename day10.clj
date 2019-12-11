(require '[clojure.string :as string])

(defn unravel-index [index height width]
  "Borrowed from numpy. Convert flat index into a coordinate pair."
  [(rem index width) (quot index height)])

(defn read-asteroids [file-name]
  "Load the asteroids (\\# signs) and their coordinates from the input."
  (let [contents (slurp file-name)
        flattened (string/replace contents #"\n" "")
        height (count (string/split-lines contents))
        width (/ (count flattened) height)
        indexed (map-indexed #(vector (unravel-index %1 height width) %2) flattened)
        asteroid-map (keys (into {} (filter (fn [v] (= (second v) \#)) indexed)))]
    asteroid-map))

; For each asteroid, we get the direct line to each other asteroid.
; Each such line can be parametrized by x' = p' + s*u', where we use
; ' to indicate vectors (s is a scalar). For a given point p' is fixed
; and u' gives the direction of the line. s shits u' around to reach
; the different points on the line. 
;
; The trick is now to keep track of all different u_1', u_2', etc. we
; "shoot" from an asteroid to the other asteroids. If two asteroids
; have the same u_i', they are on the same line. The closer one (that
; we can see), is the one with the smaller scaling factor s.

(defn round [f] (-> f (* 10000) (Math/round) (/ 10000)))

(defn get-line-params [[p1_x p1_y] [p2_x p2_y]]
  "Return the orientation and distance of point 2 to point 1."
  (let [[u_x u_y] [(- p2_x p1_x) (- p2_y p1_y)]
        s (Math/sqrt (+ (* u_x u_x) (* u_y u_y)))
        u_norm (map #(-> % (/ s) round) [u_x u_y])]
    [u_norm s]))

; I was later informed that this exists in the clojure core as
; "(group-by first coll)", but I guess I learned something..
(defn collect-into-lists [coll]
  (reduce (fn [m [k v]] (assoc m k (conj (or (m k) []) v))) {} coll))

(defn sort-map-values [in-map]
  (reduce-kv (fn [m k v] (assoc m k (into [] (sort v)))) {} in-map))

(defn point-to-points [p pcoll]
  (let [lines (mapv (fn [pt] (get-line-params p pt)) pcoll)]
    (sort-map-values (collect-into-lists lines))))

(def problem-input (read-asteroids "inputs/input10_0.txt"))

(defn get-max-station [asteroids]
  (apply max
         (map #(-> %1 (point-to-points asteroids) count dec) asteroids)))

(defn compare-angles [[v1_x v1_y] [v2_x v2_y]]
  (let [v1_angle (-> (Math/atan2 v1_y v1_x) (+ (/ Math/PI 2)) (mod Math/PI))
        ;_ (println v1_angle)
        v2_angle (-> (Math/atan2 v2_y v2_x) (+ (/ Math/PI 2)) (mod Math/PI))]
        ;_ (println v2_angle)]
    (compare v1_angle v2_angle)))

(defn sort-by-angle [asteroid-map]
  (into (sorted-map-by compare-angles) asteroid-map))
