(require '[clojure.string :as str])

(defn line-to-ints [line]
  (mapv #(Integer/parseInt %) (str/split line #",")))

; `->` and `slurp` borrowed from Daniel.
(def problem-input (-> "input5.txt" slurp str/trim-newline line-to-ints))
(println problem-input)

(defn get-digit [n i] 
  "Get the i-th digit of number n. 
  (The least significant digit is digit 0.)"
  (let [shift (->> i (Math/pow 10) int)]
    (mod (quot n shift) 10)))

(defn parse-instruction [mem]
 (let [instruction (first mem)
       opcode (mod instruction 100)
       pmodes (mapv #(get-digit instruction %) (range 2 5))]
   (case opcode
     1 {:f + :in-addr 2 :out-addr 1 :pmodes pmodes}
     2 {:f * :in-addr 2 :out-addr 1 :pmodes pmodes}
     3 {:f #(Integer/parseInt (read-line)) :in-addr 0 :out-addr 1 :pmodes pmodes}
     4 {:f println :in-addr 1 :out-addr 0 :pmodes pmodes}
     99 :halt
   )))

(defn load-addr [mem mode ip]
  (case mode
    0 (nth mem (nth mem ip))
    1 (nth mem ip)))

(defn store-addr [mem mode ip value]
  (case mode
    0 (assoc mem (nth mem ip) value)
    1 (assoc mem ip value)))

(defn addr-getter [mode]
  (fn [mem offset] 
    (case mode
      0 (nth mem offset)
      1 (offset))))

(defn eval-program
  ([mem] (eval-program mem 0))
  ([mem ip]
  (let [opcode (parse-instruction (subvec mem ip))
        pmodes (:pmodes opcode)
        addr-loader #(load-addr mem (nth pmodes %) (+ ip % 1))
        val-storer #(store-addr mem (nth pmodes %1) (+ ip %1 1) %2)]
    (when (not= opcode :halt)
      (let [in-args (:in-addr opcode)
            in-values (mapv addr-loader (range in-args))
            result (apply (:f opcode) in-values)
            out-args (:out-addr opcode)
            total-args (+ in-args out-args)
            no-output? (zero? out-args)]
        (if no-output?
          (eval-program mem (+ ip total-args 1))
          (eval-program (val-storer (- total-args 1) result) 
                        (+ ip total-args 1))))))))

(def test-program (vector 3 0 4 0 99))
(eval-program problem-input)
