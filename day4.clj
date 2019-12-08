(require '[clojure.string :as string])

; Part 1 ------------------------------

; Sort and compare to original - not very elegant but works.
(defn is-sorted? [xs] (= (seq xs) (sort xs)))

; When a number is sorted (condition 1) then it has pairs of 
; duplicate numbers when the set of digits in the number is 
; smaller than its length.
(defn is-valid-number[num]
  (let [numstr (str num)
    num-sorted? (is-sorted? numstr)
    has-doubles? (> (count numstr) (count (set numstr)))]
    (and num-sorted? has-doubles?)))

; Try the examples to test if the solution works.
(def example-inputs [111111 223450 123789])
(println (map is-valid-number example-inputs))

; Brute-force solution 1. Nicer would be to loop over
; the digits such that digit_i <= digit_{i+1} always.
(println
  (count (for [num (range 145852 616942)
               :when (is-valid-number num)]
           num)))

; Part 2 ------------------------------

; `contains` doesn't seem to work for lists, so this workaround.
; This doesn't return true/false but true/nil, but nil is falsy.
(defn in? [v coll] (some #(= v %) coll))

; Now we actually just make a histogram of numbers.
; Googling revealed the nice `frequencies` function.
(defn is-valid-number-2[num]
  (let [numstr (str num)
    num-sorted? (is-sorted? numstr)
    has-doubles? (in? 2 (vals (frequencies numstr)))]
    (and num-sorted? has-doubles?)))

; Try new examples.
(def example-inputs-2 [112233 123444 111122])
(println (map is-valid-number-2 example-inputs-2))

; Brute force again!
(println
  (count (for [num (range 145852 616942)
               :when (is-valid-number-2 num)]
           num)))
