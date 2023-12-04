(ns days.day4
  (:require [clojure.string :as str]))
  
(defn str->int [x] (Integer/valueOf x))

(defn line->card [line]
  (let [[game winners cards] (str/split line #"(: )|( \| )")]
    (hash-map
     :game (str->int (re-find #"\d+" game))
     :winners (set (map str->int (re-seq #"\d+" winners)))
     :cards (map str->int (re-seq #"\d+" cards)))))

(defn winning-numbers [card]
  (->> (:cards card)
       (filter #(contains? (:winners card) %))))

(defn card-value [card]
  (let [winning-count (count (winning-numbers card))]
    (int (Math/pow 2 (dec winning-count)))))

(def input (slurp "input/4.txt"))
(def lines (str/split-lines input))


(defn task1 [lines]
  (->> lines
       (map line->card)
       (map card-value)
       (reduce +)))

(task1 lines)

(comment

  (def test-input (str/split-lines "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

  (def card (line->card (first test-input)))

  (->> (:cards card)
       (filter #(contains? (:winners card) %))
       (count))

  (let [card (line->card (first test-input))
        winning-count (count (winning-numbers card))]
    (int (Math/pow 2 (dec winning-count))))

  card
  (filter #(contains? (:winners card) %) (:cards card))


  (->> test-input
       (map line->card)
       (map card-value)
       (reduce +))


  )



