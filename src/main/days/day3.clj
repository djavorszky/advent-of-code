(ns main.days.day3
   (:require [clojure.string :as str]
             [clojure.set :refer [union]]))



 (def input (slurp "input/3.txt"))
 (def lines (str/split-lines input))

 (defn neighbour-idxs [row col]
   (let [above (dec row)
         below (inc row)
         left (dec col)
         right (inc col)]
     #{[above left] [above col] [above right]
       [row left] [row right]
       [below left] [below col] [below right]}))


 (def symbols
   (->> (seq input)
        (filter #(not (or
                       (= \. %)
                       (= \newline %)
                       (Character/isDigit %))))
        (set)))

 (defn row-adjacents [rownum row]
   (->> row
        (map-indexed (fn [idx item] [idx item]))
        (filter #(contains? symbols (second %)))
        (map first)
        (map #(neighbour-idxs rownum %))
        (apply union)))


 (defn input->gear-idx [rownum line]
   (->> line
        (seq)
        (map-indexed vector)
        (filter #(= (second %) \*))
        (map #(hash-map :row rownum, :col (first %)))))


 (defn sets->map [data]
   (reduce (fn [result-map [coord map-set]]
             (reduce (fn [inner-map c]
                       (update inner-map c (fnil conj []) coord))
                     result-map
                     map-set))
           {}
           data))




 (defn set->map [set-data]
   (reduce (fn [result-map [key value-set]]
             (reduce (fn [inner-map coord]
                       (assoc inner-map coord key))
                     result-map
                     value-set))
           {}
           set-data))

 (def stuff (->> '({:col 130, :row 15})
                 (map #(vector % (neighbour-idxs (:row %) (:col %))))))

 (set->map stuff)

 (defn collect-symbol-neighbours [lines]
   (->> lines
        (map-indexed (fn [idx row] (row-adjacents idx row)))
        (apply union)))


 (collect-symbol-neighbours lines)

 (defn extract-numbers [linenum line]
   (loop [matches (re-seq #"\d+" line)
          parsed ()
          parsed-line line]
     (if (empty? matches)
       parsed
       (let [num (first matches)]
         (recur (rest matches) (conj parsed {:number (Integer/valueOf num)
                                             :col (str/index-of parsed-line num)
                                             :row linenum})
                (str/replace-first parsed-line num (apply str (repeat (count num) \.))))))))
 (str/replace-first "123...512" "123" (apply str (repeat (count "123") \.)))



 (defn digits [n] (-> n Math/log10 Math/floor long inc))


 (defn neighbour? [numbers syms]
   (let [n (digits (:number numbers))
         row (:row numbers)
         start-idx (:col numbers)]
     (loop [idx 0
            max n]
       (cond
         (>= idx n) false
         (contains? syms [row (+ start-idx idx)]) true
         :else (recur (inc idx) max)))))


 (defn positions [number]
   (let [n (digits (:number number))
         row (:row number)
         col (:col number)]
     (loop [idx 0
            positions []]
       (cond
         (>= idx n) positions
         :else (recur (inc idx) (conj positions [row (+ col idx)]))))))



 (defn task1 [input]
   (let [syms (collect-symbol-neighbours lines)]
     (->> (str/split-lines input)
          (map-indexed extract-numbers)
          flatten
          (filter #(neighbour? % syms))
          (map :number)
          (reduce +))))

 (task1 input)

 (defn get-gears [nums gear-coords]
   (->> (:pos nums)
        (reduce #(conj %1 (get gear-coords %2)) #{})))

 
 (defn gear-adjacent-coords [lines]
   (->> lines
        (map-indexed (fn [rownum row] (input->gear-idx rownum row)))
        (flatten)
        (map #(vector % (neighbour-idxs (:row %) (:col %))))
        (sets->map)))



 (defn gear-neighbour? [number coords]
   (some true? (map #(contains? coords %) (:pos number))))

  (defn gear->numbers [stuff]
   (->> (:gears stuff)
        (filter some?)
        flatten
        (reduce #(merge-with conj %1 (hash-map %2 (:number stuff))) {})))


 (defn merge-maps [m1 m2]
   (merge-with #(if (seq? %) (conj %2 %) (list % %2)) m1 m2))


 (defn task2 [input]
   (let [test-lines (str/split-lines input)
         gear-coords (gear-adjacent-coords test-lines)]
     (->> test-lines
          (map-indexed extract-numbers)
          flatten
          (map #(assoc % :pos (positions %)))
          (filter #(gear-neighbour? % gear-coords))
          (map #(assoc % :gears (get-gears % gear-coords)))
          (map #(gear->numbers  %))
          (reduce merge-maps {})
          vals
          (filter seq?)
          (filter #(= 2 (count %)))
          (map #(reduce * %))
          (reduce +))))

 (task2 input)


 (comment
   (def test-input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")


   (def test-lines (str/split-lines test-input))

   (let [gear-coords (gear-adjacent-coords test-lines)]
     (->> test-lines
          (map-indexed extract-numbers)
          flatten
          (map #(assoc % :pos (positions %)))
          (filter #(gear-neighbour? % gear-coords))
          (map #(assoc % :gears (get-gears % gear-coords)))
          (map #(gear->numbers  %))
          (reduce merge-maps {})
          vals
          (filter seq?)
          (filter #(= 2 (count %)))
          (map #(reduce * %))
          (reduce +))))


 (merge-with #(if (seq? %) (conj %2 %) (list % %2)) {{:r 1 :c 2} 10} {{:r 1 :c 2} 20})
 (update {} {:r 1 :c 2} conj 10)

 (def coords (gear-adjacent-coords test-lines))
 coords



 (let [number  {:number 467, :col 0, :row 0, :pos [[0 0] [0 1] [0 2]]}]
   (some true? (map #(contains? coords %) (:pos number))))


 (gear-adjacent-coords test-lines)



 (let [stuff {:number 755, :col 6, :row 7, :pos [[7 6] [7 7] [7 8]], :gears #{nil [{:col 5, :row 8}]}}]
   (->> (:gears stuff)
        (filter some?)
        flatten
        (reduce #(merge-with conj %1 (hash-map %2 (:number stuff))) {})))

 (let [gear-coords (gear-adjacent-coords test-lines)
       nums {:number 35, :col 2, :row 2, :pos [[2 2] [2 3]]}]
   (->> (:pos nums)
        (reduce #(conj %1 (get gear-coords %2 nil)) #{})))

 (let [coords (gear-adjacent-coords test-lines)]
   (->> (str/split-lines test-input)
        (map-indexed (fn [rownum row] (input->gear-idx rownum row)))
        flatten
        (filter #(neighbour? % coords))
        (map :number)
        (reduce +)))


