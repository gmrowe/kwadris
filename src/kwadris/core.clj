(ns kwadris.core
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

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

(defn cell-matrix-as-str
  [cells]
  (->> cells
       (partition width)
       (map #(str/join \space %))
       vec
       (str/join \newline)))

(defn print-state [state] (println (cell-matrix-as-str (:cells state))))

(defn set-cell-matrix
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

(defn reduce-indexed
  [f val coll]
  (second (reduce (fn [[idx val] e] [(inc idx) (f idx val e)]) [0 val] coll)))

(defn conditional-merge-vec-elements
  [v1 v2 p start-index]
  (reduce-indexed
    (fn [i v e]
      (if (and (p e) (<= 0 (+ i start-index)) (< (+ i start-index) (count v)))
        (assoc v (+ i start-index) e)
        v))
    v1
    v2))

(defn splice-vec
  [v new-elements start-index]
  (reduce (fn [v0 [i e]]
            (if (and (<= 0 (+ i start-index)) (< (+ i start-index) (count v0)))
              (assoc v0 (+ i start-index) e)
              v0))
    v
    (map-indexed vector new-elements)))

(defn clear-line
  [cells line-index]
  (splice-vec cells (repeat width \.) (* width line-index)))

(defn tetramino-spawn-col [id] (if (= id :O) 4 3))

(defn set-active-tetranimo
  [state id]
  (-> state
      (assoc :active-tetramino id)
      (assoc :active-tetramino-row 0)
      (assoc :active-tetramino-col (tetramino-spawn-col id))))

(defn last-index-of
  [p xs]
  (->> xs
       (keep-indexed (fn [i e] (when (p e) i)))
       last))

(defn index-of
  [p xs]
  (->> xs
       (keep-indexed (fn [i e] (when (p e) i)))
       first))

(defn collision-bounds
  [tetramino-repr]
  (let [occupied? #(not= % \.)]
    {:bottom-bounds (mapv #(last-index-of occupied? %)
                      (apply map vector tetramino-repr)),
     :left-bounds (mapv #(index-of occupied? %) tetramino-repr),
     :right-bounds (mapv #(last-index-of occupied? %) tetramino-repr)}))

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
   :T2 [[\. \m \.] [\. \m \m] [\. \m \.]],
   :T3 [[\. \. \.] [\m \m \m] [\. \m \.]],
   :T4 [[\. \m \.] [\m \m \.] [\. \m \.]],
   :L3 [[\. \. \.] [\o \o \o] [\o \. \.]],
   :J3 [[\. \. \.] [\b \b \b] [\. \. \b]],
   :L2 [[\. \o \.] [\. \o \.] [\. \o \o]],
   :J4 [[\. \b \.] [\. \b \.] [\b \b \.]],
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

(defn rotate-active-tetranimo-counterclockwise
  [state]
  (nth (iterate rotate-active-tetranimo-clockwise state) 3))

(defn print-active-tetramino
  [state]
  (println (tetramino-as-str (:active-tetramino state))))

(defn splice-active-tetramino
  [cells tetramino row col]
  (reduce-indexed (fn [idx cells tetr-row]
                    (conditional-merge-vec-elements cells
                                                    tetr-row
                                                    #(not= % \.)
                                                    (+ (* width (+ row idx))
                                                       col)))
                  cells
                  tetramino))

(defn as-active-tetramino-repr
  [tetramino]
  (map (fn [row] (map #(Character/toUpperCase %) row)) tetramino))

(defn print-state-with-active-tetramino
  [state]
  (println (cell-matrix-as-str (splice-active-tetramino
                                 (:cells state)
                                 (-> state
                                     :active-tetramino
                                     tetramino-repr
                                     as-active-tetramino-repr)
                                 (:active-tetramino-row state)
                                 (:active-tetramino-col state)))))

(defn tetramino-in-bounds?
  [state]
  (let [{:keys [active-tetramino active-tetramino-col active-tetramino-row]}
          state
        {:keys [bottom-bounds left-bounds right-bounds]}
          (collision-bounds (tetramino-repr active-tetramino))]
    (and (<= 0 (+ active-tetramino-col (apply min (filter some? left-bounds))))
         (< (+ active-tetramino-col (apply max (filter some? right-bounds)))
            width)
         (<= 0 active-tetramino-row)
         (<= (+ active-tetramino-row (apply max (filter some? bottom-bounds)))
             height))))

(defn move-active-tetramino-left
  [state]
  (let [new-loc (update state :active-tetramino-col - 1)]
    (if (tetramino-in-bounds? new-loc) new-loc state)))

(defn move-active-tetramino-right
  [state]
  (let [new-loc (update state :active-tetramino-col + 1)]
    (if (tetramino-in-bounds? new-loc) new-loc state)))

(defn tetramino-at-bottom?
  [state]
  (let [{:keys [active-tetramino active-tetramino-row]} state
        {:keys [bottom-bounds]} (->> active-tetramino
                                     tetramino-repr
                                     collision-bounds)]
    (= (+ active-tetramino-row (apply max (filter some? bottom-bounds)))
       height)))

(defn tetramino-blocked?
  [state]
  (let [{:keys [active-tetramino active-tetramino-row active-tetramino-col
                cells]}
          state
        {:keys [bottom-bounds]} (->> active-tetramino
                                     tetramino-repr
                                     collision-bounds)]
    (some (fn [[i b]]
            (when b
              (not= \.
                    (nth cells
                         (+ (* width (+ active-tetramino-row b))
                            (+ i active-tetramino-col))))))
          (map-indexed vector bottom-bounds))))

(defn active-tetramino-at-rest?
  [state]
  (or (tetramino-at-bottom? state) (tetramino-blocked? state)))

(defn add-active-tetramino-to-matrix
  [state]
  (assoc state
    :cells (splice-active-tetramino (:cells state)
                                    (tetramino-repr (:active-tetramino state))
                                    (:active-tetramino-row state)
                                    (:active-tetramino-col state))))

(defn move-active-tetramino-down
  [state]
  (let [new-loc (update state :active-tetramino-row + 1)]
    (cond (not (tetramino-in-bounds? new-loc)) state
          (active-tetramino-at-rest? new-loc) (add-active-tetramino-to-matrix
                                                state)
          :else new-loc)))

(defn find-equal-adjacent-elements
  [xs]
  (some (fn [[a b]] (when (= a b) a)) (partition 2 1 xs)))

(defn hard-drop-active-tetramino
  [state]
  (find-equal-adjacent-elements (iterate move-active-tetramino-down state)))

(defn complete-line?
  [cells line-index]
  (not-any? #{\.}
            (subvec cells (* width line-index) (* width (inc line-index)))))

(defn step
  [state]
  (let [complete-line-indexes (filter #(complete-line? (:cells state) %)
                                (range height))]
    (-> state
        (update :cells #(reduce clear-line % complete-line-indexes))
        (update :lines-cleared + (count complete-line-indexes))
        (update :score + (* 100 (count complete-line-indexes))))))

(defn quit [state] (assoc state :quit true))

(defn dump-internal-state [state] (pp/pprint (dissoc state :cells)))

(defn execute-command
  [state command]
  (case command
    "q" (quit state)
    "p" (do (print-state state) state)
    "g" (set-cell-matrix state (repeatedly height read-input-line))
    "c" (clear-state state)
    "?s" (do (print-score state) state)
    "?n" (do (print-lines-cleared state) state)
    "s" (step state)
    ";" (do (println) state)
    ("I" "O" "Z" "S" "J" "L" "T") (set-active-tetranimo state (keyword command))
    ")" (rotate-active-tetranimo-clockwise state)
    "(" (rotate-active-tetranimo-counterclockwise state)
    "t" (do (print-active-tetramino state) state)
    "P" (do (print-state-with-active-tetramino state) state)
    "<" (move-active-tetramino-left state)
    ">" (move-active-tetramino-right state)
    "v" (move-active-tetramino-down state)
    "V" (hard-drop-active-tetramino state)
    "D" (do (dump-internal-state state) state)
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
