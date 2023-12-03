(ns main.days.day1
  (:require [clojure.string :as str]))

(defn first-number [line]
  (->> (seq line)
       (filter #(Character/isDigit %))
       first))

(defn task1 [input]
  (let [lines (str/split-lines input)
        firsts (map first-number lines)
        lasts (map first-number (map reverse lines))]
    (->> (map vector firsts lasts)
         (map #(apply str %))
         (map #(Integer/parseInt % 10))
         (reduce +))))

(def input (slurp "input/1.txt"))

(task1 input)

(def words
  {"one" "1", "two" "2", "three" "3", "four" "4", "five" "5", "six" "6", "seven" "7", "eight" "8", "nine" "9",
   "oneight" "18", "twone" "21", "threeight" "38", "fiveight" "58", "sevenine" "79", "eightwo" "82", "eighthree" "83", "nineight" "98"})

(def pattern #"(one|two|three|four|five|six|seven|eight|nine|oneight|twone|threeight|fiveight|sevenine|eightwo|eighthree|nineight)")


(defn task2 [input]
  (-> input
      (str/replace pattern (fn [[_ found]] (get words found)))
      task1))

(task2 input)

(comment

  (def test-input "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")


  (let [lines (str/split-lines test-input)
        firsts (map first-number lines)
        lasts (map first-number (map reverse lines))]
    (->> (map vector firsts lasts)
         (map #(apply str %))
         (map #(Integer/parseInt % 10))
         (reduce +)))
  (->> (str/split-lines test-input)
       (map first-number))


  (->> (seq "pqr3stu8vwx")
       (filter #(Character/isDigit %))
       first)

  (def task2-test-input "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

  (str/replace task2-test-input pattern (fn [[_ found]] (get words found)))

  )