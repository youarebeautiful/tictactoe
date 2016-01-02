(ns tictactoe.core
  (require [clojure.string :as s])
  (:gen-class))

(def first-board
  "Provides first board to operate on"
  (sorted-map
   :1A "-" :1B "-" :1C "-"
   :2A "-" :2B "-" :2C "-"
   :3A "-" :3B "-" :3C "-"))

(defn pprint-board
  "Pretty prints a board"
  [board]
  (let [make-row  #(s/join "   " (vals (select-keys board %)))
        first-row  (make-row [:1A :1B :1C])
        second-row (make-row [:2A :2B :2C])
        third-row  (make-row [:3A :3B :3C])]
    (println "  A   B   C")
    (println (str "1 " first-row))
    (newline)
    (println (str "2 " second-row))
    (newline)
    (println (str "3 " third-row))))

;; Extracting rows, columns and diagonals

(def vec-vals
  (comp vec vals))

(defn first-row
  [board]
  (vec-vals (take 3 board)))

(defn second-row
  [board]
  (vec-vals (take 3 (drop 3 board))))

(defn third-row
  [board]
  (vec-vals (drop 6 board)))

(defn first-column
  [board]
  (vec-vals (take-nth 3 board)))

(defn second-column
  [board]
  (vec-vals (take-nth 3 (dissoc board :1A))))

(defn third-column
  [board]
  (vec-vals (take-nth 3 (dissoc board :1A :1B))))

(defn first-diag
  [board]
  (vec-vals (select-keys board [:1A :2B :3C])))

(defn second-diag
  [board]
  (vec-vals (select-keys board [:1C :2B :3A])))

(def board-funs
  (juxt first-row    second-row    third-row
        first-column second-column third-column
        first-diag   second-diag))

;; Moving

(defn valid-move?
  "Checks if field is taken or not"
  [board move]
  (= ((keyword (s/upper-case move)) board) "-"))

(defn almost-valid?
  [board move]
  (valid-move? board (s/reverse move)))

(defn make-move-valid
  "Has to take a string" ;;TODO read about typed clojure
  [board move]
  (cond
    (valid-move? board move) (s/upper-case move)
    (almost-valid? board move) (s/reverse (s/upper-case move))
    :else false))

(defn valid-or-almost?
  [board move]
  (some true? ((juxt valid-move? almost-valid?) board move)))

(defn new-board
  "Makes a new board from given board and user input"
  [board player]
  (let [move (read-line)]
    (if (valid-or-almost? board move)
      (assoc board (keyword (make-move-valid board move)) player)
      (throw (Exception. "You done messed up.")))))

(defn x-moves
  [board]
  (new-board board "x"))

(defn o-moves
  [board]
  (new-board board "o"))

;; Checking for win/draw

(defn draw?
  [board]
  (not (some #(= "-" %) (vals board))))

(defn winning-row?
  [row]
  (apply = row))

(defn winner
  [row]
  (if (winning-row? row)
    (first row)
    false))

(defn someone-won?
  [board]
  (comment (let [board-funs (juxt first-row    second-row    third-row
                                  first-column second-column third-column
                                  first-diag   second-diag)]))
  (->> (board-funs board)
       (map winner)
       (map boolean)
       (some true?)))

(defn who-won?
  [wannabe-winners]
  (first (drop-while #(= false %) wannabe-winners)))

;; Actual game

(defn game
  []
  (println "Darn, move. *choo choo*")
  (loop [i 0
         board first-board]
    (pprint-board board)
    (newline)
    (let [winner (->> (board-funs board)
                      (map winner)
                      (who-won?))]
      (cond
        (and (someone-won? board)
             (not= winner "-"))   (println (str "That's the end! " winner " has wonnered!"))
        (draw? board)             (println "It's a draw!")
        (= i 0)                   (recur (inc i) (x-moves board))
        (odd? i)                  (recur (inc i) (o-moves board))
        (even? i)                 (recur (inc i) (x-moves board))))))

(defn -main
  "I don't do a whole lot"
  []
  (game))
