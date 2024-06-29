(ns kwadris.core
  (:require [clojure.string :as str]))

(def width 10)

(def height 22)

(def empty-cells (vec (repeat (* width height) \.)))

(def init-state
  {:cells empty-cells,
   :score 0,
   :lines-cleared 0,
   :input-pointer 0,
   :input-buffer []})

(defn read-input-line [] (read-line))

(defn matrix-as-str
  [state]
  (->> state
       (:cells)
       (partition width)
       (map #(str/join \space %))
       (str/join \newline)))

(defn print-state [state] (println (matrix-as-str state)) state)

(defn given-state
  [state matrix-repr]
  (assoc state
    :cells (->> matrix-repr
                (str/join)
                (remove #(Character/isWhitespace %))
                (vec))))

(defn print-score [state] (printf "%d%n" (:score state)) (flush) state)

(defn clear-state [state] (assoc state :cells empty-cells))

(defn print-lines-cleared
  [state]
  (printf "%d%n" (:lines-cleared state))
  (flush)
  state)

(defn complete?
  [cells line-index]
  (not-any? #{\.}
            (subvec cells (* width line-index) (* width (inc line-index)))))

(defn splice-vec
  [v new-elements start-index]
  (reduce (fn [v0 [i e]] (assoc v0 (+ i start-index) e))
    v
    (map-indexed vector new-elements)))

(defn clear-line
  [cells line-index]
  (splice-vec cells (repeat width \.) (* width line-index)))

(defn set-active-tetranimo [state type] (assoc state :active-tetramino type))

(def tetramino-repr
  {:I [". . . ." "c c c c" ". . . ." ". . . ."],
   :I2 [". . c ." ". . c ." ". . c ." ". . c ."],
   :I3 [". . . ." ". . . ." "c c c c" ". . . ."],
   :I4 [". c . ." ". c . ." ". c . ." ". c . ."],
   :O ["y y" "y y"],
   :Z ["r r ." ". r r" ". . ."],
   :Z2 [". . r" ". r r" ". r ."],
   :Z3 [". . ." "r r ." ". r r"],
   :Z4 [". r ." "r r ." "r . ."],
   :S [". g g" "g g ." ". . ."],
   :S2 [". g ." ". g g" ". . g"],
   :S3 [". . ." ". g g" "g g ."],
   :S4 ["g . ." "g g ." ". g ."],
   :J ["b . ." "b b b" ". . ."],
   :J2 [". b b" ". b ." ". b ."],
   :J3 [". . ." "b b b" ". . b"],
   :J4 [". b ." ". b ." "b b ."],
   :L [". . o" "o o o" ". . ."],
   :L2 [". o ." ". o ." ". o o"],
   :L3 [". . ." "o o o" "o . ."],
   :L4 ["o o ." ". o ." ". o ."],
   :T [". m ." "m m m" ". . ."],
   :T2 [". m ." ". m m" ". m ."],
   :T3 [". . ." "m m m" ". m ."],
   :T4 [". m ." "m m ." ". m ."]})

(defn tetramino-as-str [id] (str/join \newline (tetramino-repr id)))

(defn print-active-tetramino
  [state]
  (println (tetramino-as-str (:active-tetramino state)))
  state)

(def tetranamo-rotations
  {:I :I2,
   :I2 :I3,
   :I3 :I4,
   :I4 :I,
   :O :O,
   :Z :Z2,
   :Z2 :Z3,
   :Z3 :Z4,
   :Z4 :Z,
   :S :S2,
   :S2 :S3,
   :S3 :S4,
   :S4 :S,
   :J :J2,
   :J2 :J3,
   :J3 :J4,
   :J4 :J,
   :L :L2,
   :L2 :L3,
   :L3 :L4,
   :L4 :L,
   :T :T2,
   :T2 :T3,
   :T3 :T4,
   :T4 :T})

(defn rotate-active-tetranimo-clockwise
  [state]
  (update state :active-tetramino tetranamo-rotations))

(defn print-newline [state] (println) state)

(defn step
  [state]
  (let [complete-line-indexes (filter #(complete? (:cells state) %)
                                (range height))]
    (-> state
        (update :cells #(reduce clear-line % complete-line-indexes))
        (update :lines-cleared + (count complete-line-indexes))
        (update :score + (* 100 (count complete-line-indexes))))))

(defn quit [state] (assoc state :quit true))

(defn execute-command
  [state command]
  (case command
    "q" (quit state)
    "p" (print-state state)
    "g" (given-state state (repeatedly height read-input-line))
    "c" (clear-state state)
    "?s" (print-score state)
    "?n" (print-lines-cleared state)
    "s" (step state)
    ";" (print-newline state)
    ("I" "O" "Z" "S" "J" "L" "T") (set-active-tetranimo state (keyword command))
    ")" (rotate-active-tetranimo-clockwise state)
    "t" (print-active-tetramino state)
    (do (printf "[Error] Unknown command: %s%n" command) (flush) state)))

(defn next-command
  [state]
  (if (< (:input-pointer state) (count (:input-buffer state)))
    [(nth (:input-buffer state) (:input-pointer state))
     (update state :input-pointer inc)]
    (when-let [input (read-input-line)]
      (recur (-> state
                 (assoc :input-buffer (str/split input #"\s+"))
                 (assoc :input-pointer 0))))))

(defn game-loop
  []
  (loop [state init-state]
    (when-not (:quit state)
      (when-let [[command state] (next-command state)]
        (recur (execute-command state command))))))

(defn run [_opts] (game-loop))
