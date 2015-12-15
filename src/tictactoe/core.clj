(ns tictactoe.core
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
  (let [make-row  #(clojure.string/join "   " (vals (select-keys board %)))
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

(defn first-row
  [board]
  (vec (vals (take 3 board))))

(defn second-row
  [board]
  (vec (vals (take 3 (drop 3 board)))))

(defn third-row
  [board]
  (vec (vals (drop 6 board))))

(defn first-column
  [board]
  (vec (vals (take-nth 3 board))))

(defn second-column
  [board]
  (vec (vals (take-nth 3 (dissoc board :1A)))))

(defn third-column
  [board]
  (vec (vals (take-nth 3 (dissoc board :1A :1B)))))

(defn first-diag
  [board]
  (vec (vals (select-keys board [:1A :2B :3C]))))

(defn second-diag
  [board]
  (vec (vals (select-keys board [:1C :2B :3A]))))

(def board-funs
  (juxt first-row    second-row    third-row
        first-column second-column third-column
        first-diag   second-diag))

;; Moving

(defn valid-move?
  "Checks if field is taken or not"
  [board move]
  (= ((keyword move) board) "-"))

(defn new-board
  "Makes a new board from given board and user input"
  [board player]
  (let [move (keyword (read-line))]
    (if (valid-move? board move)
        (assoc board move player)
        (do (println "It's not a valid move, I'll crash now.")
            (board)))))

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
  (println "Darn, move. Valid moves are 1A, 2B etc.")
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
