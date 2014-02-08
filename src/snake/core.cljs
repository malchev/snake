(ns snake.core
  (:require [cljs.core.async :as async :refer [<! >! chan]]
            [clojure.string :as string]
            [clojure.data :as data]
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
; The val in a [x y], val pair of a cell is one of the following:
;   <num>  -- a square of the body of the snake; the value of num increases
;             monotonically, such that the highest value belongs to the head,
;             and the lower one belongs to the tail
;   :apple -- an apple
;   :block -- an obstacle
;
; The format of [x y] coordinates is [width height]
;
; The edges of the grid are in the map, outlined as :block elements, as well
; as any obstable the grid may contain.

(defn- get-snake [grid]
  "Return a list of blocks belonging to the snake, sorted by descending order
  (i.e., head is first, tail is last)"
  (sort (fn [l r] (> (l 1) (r 1)))
                      (filter (fn [v]
                                (number? (v 1)))
                              grid)))

(declare new-apple)

(defn- snake-move [grid dir]

  "Takes a grid and a direction, and returns a vector of two hashes; the first
  hash contains the additions (empty if end game), and the second hash contains
  removals (eaten apple, tail moved)"

  (let [delta   (case dir :up [-1 0] :down [1  0] :left [0 -1] :right [0  1])
        snake   (get-snake grid)
        head    (first snake) ; e.g. [[10 20] 5]
        tail    (last snake)
        loc     (vec (map + (head 0) delta))
        content (grid loc)]
    (if content
      (cond (= content :apple)
                ; add the new head, add a new apple, and remove the eaten one;
                ; the tail stays the same, as the snake becomes one block
                ; longer.
                [{loc (inc (head 1)) (new-apple grid) :apple}
                 {loc :apple}]
            (or (= content :block) (number? content))
                ; game over
                [nil loc])
      ; advance the head, remove the tail
      [{loc (inc (head 1))} (merge {} tail)])))

(defn- update-grid [grid add del]
  (merge (first (data/diff grid del)) add))

;

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


