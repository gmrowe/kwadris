(ns learntris.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def width 10)

(def height 22)

(def init-state
  {:cells (into [] (repeat (* width height) \.)), :score 0, :lines-cleared 0})

(defn output-matrix-as-str
  [state]
  (with-out-str (doseq [row (partition width (:cells state))]
                  (println (str/join \space row)))))

(def input (atom (line-seq (io/reader *in*))))

(defn consume-line!
  []
  (let [line (first @input)]
    (swap! input next)
    line))

(defn execute-command
  [state command]
  (cond (= command "p") (do (print (output-matrix-as-str state)) state)
        (= command "g") (assoc state
                          :cells (->> (repeatedly height consume-line!)
                                      (str/join)
                                      (remove #(Character/isWhitespace %))
                                      (into [])))
        (= command "c") (assoc state
                          :cells (into [] (repeat (* width height) \.)))
        (= command "?s") (do (printf "%d" (:score state)) state)
        (= command "?n") (do (printf "%d" (:lines-cleared state)))
        :else (binding [*out* *err*]
                (printf "[Error] Unknown command: %s" command))))

(defn game-loop
  []
  (loop [state init-state]
    (when-let [line (consume-line!)]
      (recur (execute-command state (str/trim line))))))

(defn run [_opts] (game-loop))
