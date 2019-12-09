(ns intcode-computer)

(defn get-digit [n i] 
  "Get the i-th digit of number n. 
  (The least significant digit is digit 0.)"
  (let [shift (->> i (Math/pow 10) int)]
    (mod (quot n shift) 10)))

(defn parse-instruction [mem]
  "Parse the ABCDE instruction at the start of this mem region.
  
  ABC contain the parameter modes for parameters 3/2/1 and DE
  stores the actual opcode. We return the function that this
  opcode computes and data about its arguments and param modes."
 (let [instruction (first mem)
       opcode (mod instruction 100)
       pmodes (mapv #(get-digit instruction %) (range 2 5))]
   (case opcode
     1 {:f + :in-args 2 :out-pos 2 :pmodes pmodes}
     2 {:f * :in-args 2 :out-pos 2 :pmodes pmodes}
     3 {:f #(Integer/parseInt (read-line)) :in-args 0 :out-pos 0 :pmodes pmodes}
     4 {:f #(println %) :in-args 1 :out-pos nil :pmodes pmodes}
     5 {:f #(if (zero? %1) nil %2) :in-args 2 :out-pos nil :pmodes pmodes}
     6 {:f #(if (zero? %1) %2 nil) :in-args 2 :out-pos nil :pmodes pmodes}
     7 {:f #(if (< %1 %2) 1 0) :in-args 2 :out-pos 2 :pmodes pmodes}
     8 {:f #(if (= %1 %2) 1 0) :in-args 2 :out-pos 2 :pmodes pmodes}
     99 :halt
   )))

(defn get-next-ip-addr [ip opcode result] 
  "Get the next IP after the opcode has run and returned the result.
  - for instructions with output, simply go to the pos after the output.
  - for instructions without output (print or jumps), there are two options:
      - if they return `nil`, the IP just goes past their params
      - if they return a value, it's interpreted as an address and the IP set to it"
  (if-let [out-pos (:out-pos opcode)] 
    (+ ip out-pos 2) 
    (if-let [target-addr result] 
      target-addr
      (+ ip (inc (:in-args opcode))))))

(defn addr-getter [mem ip pmodes]
  "Return a function that loads the address at ip+offset. Depending
  on the parameter mode, it returns mem[ip+offset+1] or ip+offset+1."
  (fn [offset] 
    (let [offset-mode (nth pmodes offset)
          mem-offset (+ ip offset 1)]
    (case offset-mode
      0 (nth mem mem-offset)
      1 mem-offset))))

(defn eval-program
  ([mem] (eval-program mem 0))
  ([mem ip]
  (let [opcode (parse-instruction (subvec mem ip))
        load-addr (addr-getter mem ip (:pmodes opcode))]
    (when (not= opcode :halt)
      (let [in-addrs (mapv load-addr (range (:in-args opcode)))
            in-vals (mapv #(nth mem %) in-addrs)
            result (apply (:f opcode) in-vals)
            next-ip (get-next-ip-addr ip opcode result)]
        (if (some? (:out-pos opcode))
          (eval-program (assoc mem (load-addr (:out-pos opcode)) result) next-ip)
          (eval-program mem next-ip)))))))
