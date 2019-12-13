(require '[clojure.string :as string])

(defn print-state [moon-vecs moon-velocities]
  (do 
    (doseq [[[px py pz] [vx vy vz]] 
            (map vector moon-vecs moon-velocities)]
      (printf "pos=<x=%3d, y=%3d, z=%3d>    vel=<x=%3d, y=%3d, z=%3d>\n" px py pz vx vy vz))
    (printf "Sum of total energy: %d\n" (total-energy moon-vecs moon-velocities))))

(defn get-diff-vec [vec1 vec2] (into [] (map compare vec1 vec2)))

(def add-vectors #(apply map + %))

(defn velocity-updates [moon-vecs]
  (for [moon-1 moon-vecs]
    (add-vectors 
           (for [moon-2 moon-vecs]
            (get-diff-vec moon-2 moon-1)))))


(defn add-vector-lists [veclist1 veclist2]
  (map add-vectors (map vector veclist1 veclist2)))

(defn abs-vector [v]
  (map #(Math/abs %) v))

(defn move-planets [moon-vecs moon-velocities]
  (let [velocity-diffs (velocity-updates moon-vecs)
        new-velocities (add-vector-lists moon-velocities velocity-diffs)
        new-positions (add-vector-lists moon-vecs new-velocities)]
    [new-positions new-velocities]))

(defn total-energy [moon-vec moon-velocities]
  (let [potential-energies (map #(apply + %) (map abs-vector moon-vec))
        kinetic-energies (map #(apply + %) (map abs-vector moon-velocities))
        total-energies (map * potential-energies kinetic-energies)]
    (apply + total-energies)))

; Part 1.

(def positions 
  (map (fn [line] 
         (-> line 
             (string/replace #"[<>xyz =]" "") 
             (string/split #",")
             ((fn [nums] (map #(Integer/parseInt %) nums)))))
       (string/split-lines (slurp "inputs/input12_0.txt"))))

(def velocities (repeat (count problem-input) [0 0 0]))

(doseq [[positions velocities] (take 1001 (iterate #(apply move-planets %) [positions velocities]))]
  (do 
    (println)
    (print-state positions velocities)))
