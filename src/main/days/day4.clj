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

(defn winning-count [card] (count (winning-numbers card)))

(defn card-value [card]
  (int (Math/pow 2 (dec (winning-count card)))))

(def input (slurp "input/4.txt"))
(def lines (str/split-lines input))


(defn task1 [lines]
  (->> lines
       (map line->card)
       (map card-value)
       (reduce +)))



(task1 lines)


;; ---- part 2 -----

(defn scratch-game [cards]
  (->> cards
       (map winning-count)
       (map #(hash-map :wins % :has 1))
       (into [])))

(defn amount-won [card-num cards]
  (:wins (nth cards (dec card-num))))


(defn update-counts [card-num cards]
  (let [amount (:has (nth cards (dec card-num)))]
    (loop [idx card-num
           max (+ (dec idx) (amount-won card-num cards))
           cards cards]
      (if (<= idx max)
        (recur
         (inc idx)
         max
         (update-in cards [idx :has] + amount))
        cards))))

(defn play-cards [cards]
  (loop [cards (scratch-game cards)
         idx 0
         max (count cards)]
    (if (< idx max)
      (recur (update-counts (inc idx) cards) (inc idx) max)
      cards)))



(defn task2 [lines]
  (->> lines
       (map line->card)
       (play-cards)
       (map :has)
       (reduce +)))

(task2 lines)

(comment

  (def test-input (str/split-lines "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

  (def card (line->card (first test-input)))

  card

  (->> (:cards card)
       (filter #(contains? (:winners card) %))
       (count))

  (let [card (line->card (first test-input))
        winning-count (count (winning-numbers card))]
    (int (Math/pow 2 (dec winning-count))))


  (filter #(contains? (:winners card) %) (:cards card))


  (def test-cards (map line->card test-input))

  (def scratchies (scratch-game test-cards))


  (defn play-cards [cards]
    (loop [cards (scratch-game cards)
           idx 0
           max (count cards)]
      (if (< idx max)
        (recur (update-counts (inc idx) cards) (inc idx) max)
        cards)))

  (->> (play-cards test-cards)
       (map :has)
       (reduce +))


  (def scratchies (scratch-game test-cards))

  (update-in scratchies [0 :wins] inc))

