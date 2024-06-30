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

(defn matrix-repr
  [cells]
  (->> cells
       (partition width)
       (map #(str/join \space %))
       vec))

(defn print-state
  [state]
  (println (str/join \newline (matrix-repr (:cells state)))))

(defn given-state
  [state given-matrix]
  (assoc state
    :cells (->> given-matrix
                (str/join)
                (remove #(Character/isWhitespace %))
                (vec))))

(defn print-score [state] (printf "%d%n" (:score state)) (flush))

(defn clear-state [state] (assoc state :cells empty-cells))

(defn print-lines-cleared
  [state]
  (printf "%d%n" (:lines-cleared state))
  (flush))

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

(defn tetramino-spawn-col [id] (if (= id :O) 4 3))

#_(def tetramino-spawn-col {:O 4, :L 3, :J 3, :Z 3, :S 3, :I 3, :T 3})

(defn set-active-tetranimo
  [state id]
  (-> state
      (assoc :active-tetramino id)
      (assoc :active-tetramino-row 0)
      (assoc :active-tetramino-col (tetramino-spawn-col id))))

(def tetramino-repr
  {:L4 [[\o \o \.] [\. \o \.] [\. \o \.]],
   :S4 [[\g \. \.] [\g \g \.] [\. \g \.]],
   :Z2 [[\. \. \r] [\. \r \r] [\. \r \.]],
   :L [[\. \. \o] [\o \o \o] [\. \. \.]],
   :I [[\. \. \. \.] [\c \c \c \c] [\. \. \. \.] [\. \. \. \.]],
   :I4 [[\. \c \. \.] [\. \c \. \.] [\. \c \. \.] [\. \c \. \.]],
   :O [[\y \y] [\y \y]],
   :S2 [[\. \g \.] [\. \g \g] [\. \. \g]],
   :Z3 [[\. \. \.] [\r \r \.] [\. \r \r]],
   :J [[\b \. \.] [\b \b \b] [\. \. \.]],
   :Z [[\r \r \.] [\. \r \r] [\. \. \.]],
   :J2 [[\. \b \b] [\. \b \.] [\. \b \.]],
   :I3 [[\. \. \. \.] [\. \. \. \.] [\c \c \c \c] [\. \. \. \.]],
   :T [[\. \m \.] [\m \m \m] [\. \. \.]],
   :L3 [[\. \. \.] [\o \o \o] [\o \. \.]],
   :T4 [[\. \m \.] [\m \m \.] [\. \m \.]],
   :J3 [[\. \. \.] [\b \b \b] [\. \. \b]],
   :T2 [[\. \m \.] [\. \m \m] [\. \m \.]],
   :L2 [[\. \o \.] [\. \o \.] [\. \o \o]],
   :J4 [[\. \b \.] [\. \b \.] [\b \b \.]],
   :T3 [[\. \. \.] [\m \m \m] [\. \m \.]],
   :I2 [[\. \. \c \.] [\. \. \c \.] [\. \. \c \.] [\. \. \c \.]],
   :S3 [[\. \. \.] [\. \g \g] [\g \g \.]],
   :S [[\. \g \g] [\g \g \.] [\. \. \.]],
   :Z4 [[\. \r \.] [\r \r \.] [\r \. \.]]})


(defn tetramino-as-str
  [id]
  (->> (tetramino-repr id)
       (map #(str/join \space %))
       (str/join \newline)))

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

(defn print-active-tetramino
  [state]
  (println (tetramino-as-str (:active-tetramino state))))

(defn cells-with-active-tetramino
  [state]
  (let [cells (:cells state)
        tetr (-> state
                 :active-tetramino
                 tetramino-repr)]
    (reduce (fn [cs tetr-row]
              (splice-vec cs
                          (map #(Character/toUpperCase %) (nth tetr tetr-row))
                          (+ (:active-tetramino-col state)
                             (* width
                                (+ tetr-row (:active-tetramino-row state))))))
      cells
      (range (count tetr)))))

(defn print-state-with-active-tetramino
  [state]
  (println (str/join \newline
                     (matrix-repr (cells-with-active-tetramino state)))))


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
    "p" (do (print-state state) state)
    "g" (given-state state (repeatedly height read-input-line))
    "c" (clear-state state)
    "?s" (do (print-score state) state)
    "?n" (do (print-lines-cleared state) state)
    "s" (step state)
    ";" (do (println) state)
    ("I" "O" "Z" "S" "J" "L" "T") (set-active-tetranimo state (keyword command))
    ")" (rotate-active-tetranimo-clockwise state)
    "t" (do (print-active-tetramino state) state)
    "P" (do (print-state-with-active-tetramino state) state)
    (do (printf "[Error] Unknown command: %s%n" command) (flush) state)))


(defn parse-command-list
  [s]
  (loop [s s
         commands []]
    (if (seq s)
      (cond (Character/isWhitespace (first s)) (recur (next s) commands)
            (= \? (first s)) (recur (drop 2 s)
                                    (conj commands (str/join (take 2 s))))
            :else (recur (next s) (conj commands (str (first s)))))
      commands)))

(defn next-command
  [state]
  (if (< (:input-pointer state) (count (:input-buffer state)))
    [(nth (:input-buffer state) (:input-pointer state))
     (update state :input-pointer inc)]
    (when-let [input (read-input-line)]
      (recur (-> state
                 (assoc :input-buffer (parse-command-list input))
                 (assoc :input-pointer 0))))))

(defn game-loop
  []
  (loop [state init-state]
    (when-not (:quit state)
      (when-let [[command state] (next-command state)]
        (recur (execute-command state command))))))

(defn run [_opts] (game-loop))
