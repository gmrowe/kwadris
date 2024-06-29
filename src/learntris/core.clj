(ns learntris.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def width 10)

(def height 22)

(def empty-cells (into [] (repeat (* width height) \.)))

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
                (into []))))

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

(def active-tetramino-as-str
  {"I" (str/join \newline [". . . ." "c c c c" ". . . ." ". . . ."]),
   "O" (str/join \newline ["y y" "y y"])})

(defn print-active-tetramino
  [state]
  (println (active-tetramino-as-str (:active-tetramino state)))
  state)

(defn step
  [state]
  (let [complete-line-indexes (filter #(complete? (:cells state) %)
                                (range height))]
    (-> state
        (update :cells #(reduce clear-line % complete-line-indexes))
        (update :lines-cleared + (count complete-line-indexes))
        (update :score + (* 100 (count complete-line-indexes))))))

(defn execute-command
  [state command]
  (case command
    "p" (print-state state)
    "g" (given-state state (repeatedly height read-input-line))
    "c" (clear-state state)
    "?s" (print-score state)
    "?n" (print-lines-cleared state)
    "s" (step state)
    ("I" "O") (set-active-tetranimo state command)
    "t" (print-active-tetramino state)
    (do (binding [*out* *err*] (printf "[Error] Unknown command: %s%n" command))
        (flush)
        state)))

(defn game-loop
  []
  (loop [state init-state]
    (if (< (:input-pointer state) (count (:input-buffer state)))
      (let [command (nth (:input-buffer state) (:input-pointer state))]
        (recur (-> state
                   (update :input-pointer inc)
                   (execute-command command))))
      (when-let [input (read-input-line)]
        (recur (-> state
                   (assoc :input-buffer (str/split input #"\s+"))
                   (assoc :input-pointer 0)))))))

(comment
  (let [phony "1 2 3 ?4\n"] (str/split phony #"\s+")))

(defn run [_opts] (game-loop))
