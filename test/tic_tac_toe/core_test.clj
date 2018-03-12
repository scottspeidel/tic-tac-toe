(ns tic-tac-toe.core-test
  (:require [clojure.test :refer :all]
            [tic-tac-toe.core :refer :all]))

(deftest find-next-move-full-board
  (testing "find-next-move-full-board"
    (let [
          full-board [\X \X \O
                      \X \O \O
                      \O \X \X]
          expected-boards nil
          ]
      (is (= (find-next-move full-board) expected-boards))
      )
    )
  )
(deftest find-next-move-not-full-board
  (testing "find-next-move-full-board"
    (let [
          not-full-board [\- \X \O
                          \X \O \O
                          \O \X \X]
          expected-board [\X \X \O
                          \X \O \O
                          \O \X \X]
          ]
      (is (= (find-next-move full-board) expected-boards))
      )
    )
  )
