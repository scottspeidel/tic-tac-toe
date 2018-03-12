(ns tic-tac-toe.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn- is-x-or-o
  [square]
  square
  (cond
    (= square \X) true
    (= square \O) true
    :else false))

(defn winner?
  ;; [0 1 2
  ;;  3 4 5
  ;;  6 7 8]
  [board]
  (cond
    (and (is-x-or-o (nth board 0)) (= (nth board 0) (nth board 1) (nth board 2))) (nth board 0)
    (and (is-x-or-o (nth board 3)) (= (nth board 3) (nth board 4) (nth board 5))) (nth board 3)
    (and (is-x-or-o (nth board 6)) (= (nth board 6) (nth board 7) (nth board 8))) (nth board 6)
    (and (is-x-or-o (nth board 0)) (= (nth board 0) (nth board 3) (nth board 6))) (nth board 0)
    (and (is-x-or-o (nth board 1)) (= (nth board 1) (nth board 4) (nth board 7))) (nth board 1)
    (and (is-x-or-o (nth board 2)) (= (nth board 2) (nth board 5) (nth board 8))) (nth board 2)
    (and (is-x-or-o (nth board 0)) (= (nth board 0) (nth board 4) (nth board 8))) (nth board 0)
    (and (is-x-or-o (nth board 6)) (= (nth board 6) (nth board 2) (nth board 4))) (nth board 6)
    :else false))

(defn possible-moves
  [board]
  (vec (remove nil? (map-indexed (fn [idx x] (cond (= \- x) idx)) board))))

(defn add-move
  [board player move]
  (assoc (vec board) move player))

(defn possible-boards
  [board player possible-moves]
  (map (partial add-move board player) possible-moves))

(defn g
  [board possible]
  (loop [possible possible
         paths  []]
    (if (empty? possible)
      paths
      (recur (pop possible) (cons (last possible) paths)))))

(defn paths
  [board]
  (g board (vec (combo/permutations (possible-moves board)))))

(defn swap-player
  [player]
  (cond
    (= player \X) \O
    (= player \O) \X))

(defn score
  [board player path]
  (let [original-player player]
    (loop [board board
           current-player player
           traversal path
           count 0]
      (cond
        (= (winner? board) original-player) {:score 10 :path path :player original-player :count count}
        (= (winner? board) (swap-player original-player)) {:score -10 :path path :player (swap-player original-player) :count count}
        (and (empty? traversal) (not= (winner? board) player)) {:score 0 :path path :board board}
        :else
        (recur  (add-move board current-player (first traversal)) (swap-player current-player) (rest traversal) (+ count 1))))))

;; https://groups.google.com/forum/#!msg/clojure/oQU30CSsIQA/ybzSI2287K8J
(defn compare-many [comps]
  (fn [xs ys]
    (if-let [result (first (drop-while zero? (map (fn [f x y] (. f (compare x y))) comps xs ys)))]
      result
      0)))

(defn sort-by-many [keyfns comps coll] (sort-by (apply juxt keyfns) (compare-many comps) coll))

(defn sort-by-map [m coll]
  (sort-by-many (keys m)
                (map #(case % 1 compare -1 (comp - compare) (throw (Exception. "1 or -1"))) (vals m))
                coll))

(defn play
  [board player]
  (first (sort-by-map (array-map :score -1 :count 1) (map (partial score board player) (paths board)))))

(let [board '(\O \X \O
              \X \X \-
              \O \- \-)
      player \X
      result-map (play board player)]
  (let [{path :path} result-map ]
    (first path)))

;; (defn -main(de)
;;   "I don't do a whole lot ... yet."
;;   [& args]
;;   (println "Hello, World!"))

