(defn sort
  ([coll])
  ([^java.util.Comparator comp coll]
   (if (seq coll)
     (let [a (to-array coll)]
       (. java.util.Arrays (sort a comp))
       (seq a))
())))

