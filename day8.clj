(require '[clojure.string :as string])
(def input-pixels
  (mapv #(Character/digit % 10) (-> "inputs/input8.txt" slurp string/trim-newline)))

(println "Total input pixels:" (count input-pixels))

(def image-dim (* 25 6))

(defn layer-iter [input-pixels image-dim]
  (map #(into [] %) (partition image-dim input-pixels)))

; Part 1 -------------------
(def min-zeroes
  (apply min-key #(get % 0)
    (for [layer (layer-iter input-pixels image-dim)]
      (frequencies layer))))

(println min-zeroes (* (min-zeroes 1) (min-zeroes 2)))

; Part 2 -------------------
(defn add-pixel [p1 p2] 
  "Return p2 if p1 is transparent, else keep p1."
  (if (== 2 p1) p2 p1))

(def layersum 
  (reduce #(map add-pixel %1 %2) 
          (layer-iter input-pixels image-dim)))

; Print the layers nicely.
(doseq [row (partition 25 layersum)]
  (let [rowstr (-> (string/join "" row) 
                   (string/replace #"0" "░") 
                   (string/replace #"1" "▓"))]
    (println rowstr)))
