(ns snake.core
  (:require [cljs.core.async :as async :refer [<! >! chan]]
            [clojure.string :as string]
            [goog.dom :as dom]
            [goog.events :as events])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn- log! [& msg]
  (.log js/console (string/join (map str msg))))

(defn- set-html! [el html]
  (set! (.-innerHTML el) html))

; The state is maintained in a hash, representing the grid, of pairs in the
; format [x y] val.  A pair will represent only a non-empty square in the
; grid; we know that a square is empty by the absence of a cell in the map
; with these coordinates.
;
; The val in a [x y], val pair of a cell is one of the following keywords:
;   :snake -- a square of the body of the snake
;   :apple -- an apple
;   :block -- an obstacle
;
; The format of [x y] coordinates is [width height]
;
; The edges of the grid are in the map, outlined as :block elements, as well
; as any obstable the grid may contain.
;
; The value of the grid at any point in time refers to a valid game state;
; the transition function mapping one grid to another either generates a valid
; grid, or returns nil to indicate end of game.

(defn- new-apple [grid]
  "Generates a random new location for an apple in the grid; makes sure it
  doesn't generate it on top of an already-existing apple."
  [1 1])

(defn- snake [grid head dir]

  "The transition function takes a valid grid, the location [x y] of the snake's
  head, and a direction for the next move of the snake (one of :up, :down, :left,
  or :right) and returns a new grid representing the next game state, or nil if
  the game is over.  The new grid will be updated with a consumed apple, if any,
  and a new apple, if an apple has already been consumed.

  Note that we have to take in the location of the snake's head because the
  grid does not make that distinction.  For that reason, we return a pair
  consisting of a the updated grid and the new snake's head."

  (let [delta   (case dir :up [0 -1] :down [0  1] :left [-1 0] :right [1  0])
        loc     (vec (map + head delta))
        content (grid loc)]
    (if content
      (cond (= content :apple)
              (let [newgrid (assoc
                              (assoc
                                (dissoc grid loc)
                                loc :snake)
                              (new-apple grid) :apple)]
                #_(log! "apple!")
                [newgrid loc])
            (or (= content :block)
                (= content :snake))
              (do
                #_(log! "block or snake!")
                [nil loc])) ; apple of obstacle
      (do
        #_(log! "safe to move")
        [(assoc grid loc :snake) loc]))))

(let [[g h] (snake {[50 49] :apple} [50 50] :up)]
  (let [[g h] (snake g h :up)]
    (log! g)
    (log! h)))


