(require '[clojure.java.io :as io])

(defn read-lines [filename]
  (with-open [rdr (io/reader filename)]
    (into [] (line-seq rdr))))

; Part 1 -----------------------

(defn calculate-fuel 
  [mass]
  (- (int (/ mass 3)) 2))

; Part 2 -----------------------

(defn calculate-recursive-fuel 
  (
   [mass]
   (calculate-recursive-fuel mass 0))
  (
   [mass fuel-sum]
   (
   let [fuel (calculate-fuel mass)
        pos-fuel (max 0 fuel)]
   (cond
     (== pos-fuel 0) fuel-sum
     :else (recur pos-fuel (+ fuel-sum pos-fuel))))))

; Sum the fuel -----------------------

(defn calc-total-fuel [] (
  let [input-lines (read-lines "input1.txt")
       module-weights (map #(Integer/parseInt %) input-lines)
       module-fuels (map calculate-recursive-fuel module-weights)]
  (apply + module-fuels)))

