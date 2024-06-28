(ns learntris.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def width 10)

(def height 22)

(def empty-cells (into [] (repeat (* width height) \.)))

(def init-state {:cells empty-cells, :score 0, :lines-cleared 0})

(def input (atom (line-seq (io/reader *in*))))

(defn consume-line!
  []
  (let [line (first @input)]
    (swap! input next)
    line))

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

(defn print-score [state] (printf "%d%n" (:score state)) state)

(defn clear-state [state] (assoc state :cells empty-cells))

(defn print-lines-cleared [state] (printf "%d%n" (:lines-cleared state)) state)

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
    "g" (given-state state (repeatedly height consume-line!))
    "c" (clear-state state)
    "?s" (print-score state)
    "?n" (print-lines-cleared state)
    "s" (step state)
    (binding [*out* *err*] (printf "[Error] Unknown command: %s" command))))

(defn game-loop
  []
  (loop [state init-state]
    (when-let [line (consume-line!)]
      (recur (execute-command state (str/trim line))))))

(defn run [_opts] (game-loop))

