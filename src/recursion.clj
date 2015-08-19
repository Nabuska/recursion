(ns recursion)

;IN REPL -->
;(use 'recursion)
;(require 'recursion)



(defn product [coll]
  (if(empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (empty? (rest coll)))

(defn my-last [coll]
  (if(singleton? coll)
    (first coll)
    (my-last(rest coll))))

(defn max-element [a-seq]
  (if(empty? a-seq)
    nil
    (if(singleton? a-seq)
      (first a-seq)
      (let [cur-max (max (first a-seq) (first(rest a-seq)))]
        (max-element (cons cur-max (rest (rest a-seq))))))))

(defn seq-max [seq-1 seq-2]
  (if(> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if(empty? a-seq)
    nil
    (if(singleton? a-seq)
      (first a-seq)
      (let [cur-max (seq-max (first a-seq) (first(rest a-seq)))]
        (longest-sequence (cons cur-max (rest (rest a-seq))))))))

(defn my-filter [pred? a-seq]
  (if(empty? a-seq)
    a-seq
    (if(pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

;(defn my-map [f a-seq]
;  (if (empty? a-seq)
;    a-seq
;    (cons (f (first a-seq))
;      (my-map f (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if(pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if(pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if(and (empty? a-seq) (empty? b-seq))
    true
    (if(= (first a-seq)(first b-seq))
      (seq= (rest a-seq)(rest b-seq))
      false)))

(defn my-map [f seq-1 seq-2]
  (if(or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f seq-1 seq-2) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if(zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if(< n 2)
    n
    (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if(< how-many-times 1)
    '()
    (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)))

(defn my-range [up-to]
  (let [value (dec up-to)]
    (if (< value 0)
      '()
      (conj (my-range value) value ))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (conj '() '())
    (conj (tails (rest a-seq)) (seq a-seq))))

;(defn inits [a-seq]
;  (if (empty? a-seq)
;    (conj '() '())
;    (conj (tails (drop-last a-seq)) (seq a-seq))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (drop-last (map concat (reverse (tails a-seq)) (inits a-seq))))


;use (not(contains (first a-seq)))
;use (assoc {:x 1} :x ?))
(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
      (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq) 0)));0 is applyed when no (first a-seq)
      (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [cKey (first (keys a-map)) cValue (last (first a-map))]
      (concat (repeat cValue cKey) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (< n 1))
    '()
    (conj (my-take (dec n) (rest coll)) (first coll))
    )
  )

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (< n 1)
      (conj (my-drop n (rest coll)) (first coll))
      (my-drop (dec n) (rest coll))
      )
    )
  )

(defn halve [a-seq]
  (let [middleIndex (int (/ (count a-seq) 2))]
    (conj (conj [] (my-take middleIndex a-seq)) (my-drop middleIndex a-seq))
    )
  )

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? b-seq) a-seq
    (empty? a-seq) b-seq
    :else
      (if (< (first a-seq) (first b-seq))
        (conj (seq-merge (rest a-seq) b-seq) (first a-seq))
        (conj (seq-merge a-seq (rest b-seq)) (first b-seq)))
    )
  )

(defn seq-merge2 [a-seq b-seq]
  (cond
    (empty? a-seq)  b-seq
    (empty? b-seq)  a-seq
    :else
      (if (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        )
    )
  )

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
      (seq-merge
        (merge-sort (first (halve a-seq)))
        (merge-sort (last (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (if(< (first a-seq) (first (rest a-seq))
      (cons (first a-seq) (split-into-monotonics (rest a-seq)))
      ((cons (split-into-monotonics (rest a-seq))))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])


