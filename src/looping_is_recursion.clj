(ns looping-is-recursion)

(defn power [base exp]
  (letfn [(iter [i acc]
                (if (= i exp)
                  acc
                  (recur (inc i) (* base acc))))]
    (iter 0 1)))

(defn last-element [a-seq]
  (cond (empty? a-seq) nil
        (empty? (rest a-seq)) (first a-seq)
        :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (not= (first seq1) (first seq2)) false
        :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         [f & r :as b-seq] a-seq]
    (cond (empty? b-seq) nil
          (pred f) i
          :else (recur (inc i) r))))

(defn avg [a-seq]
  (loop [sum 0
         cnt 0
         [f & r :as b-seq] a-seq]
    (if (empty? b-seq) (/ sum (if (zero? cnt) 1 cnt))
      (recur (+ f sum) (inc cnt) r))))

(defn parity [a-seq]
  (loop [odd-things #{}
         [f & r :as b-seq] a-seq]
    (cond (empty? b-seq) odd-things
          (contains? odd-things f) (recur (disj odd-things f) r)
          :else (recur (conj odd-things f) r))))

(defn fast-fibo [n]
  (loop [a -1
         b 1
         i 0]
    (if (= i n)
      (+ a b)
      (recur b (+ a b) (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         prefix []
         [f & r :as b-seq] a-seq]
    (cond (empty? b-seq) prefix
          (seen f) prefix
          :else (recur (conj seen f)
                       (conj prefix f)
                       r))))
