(ns main.days.day2
  (:require [clojure.string :as str]))


(defn parse-cubes [input]
  (->> (str/split input #" ")
       (reverse)))

(defn parse-set [input]
  (->> (str/split input #", ")
       (map parse-cubes)))

(defn get-id [game] (second (str/split game #" ")))

(defn list->map [l]
  (into {} (mapv (fn [[k v]] [k (Integer. v)]) l)))

(defn count-cubes [input]
  (let [input (str/split input #": ")
        games (second input)
        id (get-id (first input))
        sets (str/split games #"; ")]
    [(Integer. id) (->> sets
                        (map parse-set)
                        (mapv list->map))]))


(defn enough? [cubes]
  (and
   (>= 12 (get cubes "red" 0))
   (>= 13 (get cubes "green" 0))
   (>= 14 (get cubes "blue" 0))))

(defn all-possible? [cubes] (every? true? (map enough? (second cubes))))

(defn task1 [input]
  (->> (str/split-lines input)
       (map count-cubes)
       (filter all-possible?)
       (map first)
       (reduce +)))

(defn task2 [input]
  (->> (str/split-lines input)
       (map count-cubes)
       (map second)
       (map #(apply merge-with max %))
       (map #(reduce * (vals %)))
       (reduce +)))



(def input (slurp "input/2.txt"))

(task1 input)
(task2 input)

(comment
  (def test-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  (count-cubes test-game-imp-2)

  (def test-game-imp-1 "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
  (def test-game-imp-2 "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")

  (def res (count-cubes test-game-imp-1))

  (reduce + (vals res))

  (->> (str/split "3 blue, 4 red" #", ")
       (map parse-cubes))

  (second cubes)

  (->> (str/split-lines test-input)
       (map count-cubes)
       (map second)
       (map #(apply merge-with max %))
       (map #(reduce * (vals %)))
       (reduce +))
  


  (let [input (str/split test-game #": ")
        games (second input)
        id (get-id (first input))
        sets (str/split games #"; ")]
    id)


  (def test-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

  (defn enough? [cubes]
    (and
     (>= 12 (get cubes "red" 0))
     (>= 13 (get cubes "green" 0))
     (>= 14 (get cubes "blue" 0))))


  (->> (second cubes)
       (apply merge-with max))

  (def cubes (count-cubes test-game))

  (every? true? (map enough? (second cubes)))

  (let [input (str/split test-game-imp-2 #": ")
        games (second input)
        id (get-id (first input))
        sets (str/split games #"; ")]
    [(Integer. id) (->> sets
                        (map parse-set)
                        (mapv list->map))])


  )



