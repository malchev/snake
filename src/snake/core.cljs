(ns snake.core
  (:require [cljs.core.async :as async :refer [<! >! put! chan timeout]]
            [clojure.string :as string]
            [clojure.data :as data]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]
            [goog.events :as events]
            [goog.events.KeyHandler :as key-handler]
            [goog.events.KeyCodes :as key-codes])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn- log! [& msg]
  (.log js/console (string/join (map str msg))))

(defn- set-html! [el html]
  (set! (.-innerHTML el) html))

(defn- by-id [id]
  (.getElementById js/document id))

(defn- listen [el t]
  (let [in (chan)]
    (.addEventListener el t
                   (fn [e] (put! in e)))
    in))

(defn set-class! [el name]
  (classes/set el name))

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

; Rendering Logic

(def width 20)
(def height 50)

(defn- new-apple [grid]
  "Generates a random new location for an apple in the grid; makes sure it
  doesn't generate it on top of an already-existing apple."
  (loop [x (rand-int width) y (rand-int height)]
    (if (grid [x y])
      (recur (rand-int width) (rand-int height))
      [x y])))

(def ^:constant APPLE "@")
(def ^:constant BLOCK "x")
(def ^:constant SNAKE "#")
(def ^:constant BLANK "-")

(defn- token-to-representation [v]
  "maps :block, :apple, and <num> to 'x', '@', and '#' respectively"
  (cond
   (= v :block) [ BLOCK "block" ]
   (= v :apple) [ APPLE "apple" ]
   (number? v)  [ SNAKE "snake" ]
   true         [ BLANK "table" ]))

(defn- render-full! [grid]
  (let [el (by-id "snakegrid")
        arr (array)]
    (loop [x 0]
      (when (< x width)
        (.push arr "<tr>")
        (loop [y 0]
          (when (< y height)
            (let [[v g] (token-to-representation (grid [x y]))]
              (.push arr (str "<td id=block-" (+ (* x height) y) " class=" g ">" v "</td>")))
            (recur (inc y))))
        (.push arr "</tr>")
        (recur (inc x))))
    (set-html! el (.join arr ""))))

(defn- render-updates! [add del]
  "Go over the deletions, and replace them with '_' elements; then go over the
  additions, and replace them with a '#' element"
  (doseq [[[x y] _] del]
    (let [el (by-id (str "block-" (+ (* x height) y)))]
      (set-html!  el BLANK)
      (set-class! el "table")))
  (doseq [[[x y] v] add]
    (let [[r g] (token-to-representation v)
          el (by-id (str "block-" (+ (* x height) y)))]
      (set-html!  el r)
      (set-class! el g))))

(def starting-grid
  (let [top+bot    (reduce (fn [m v]
                             (merge m {[v 0] :block,
                                       [v (dec height)] :block}))
                           {}
                           (range width))
        sides      (reduce (fn [m v]
                             (merge m {[0 v] :block,
                                       [(dec width) v] :block}))
                           {}
                           (range height))
        borders    (merge top+bot sides)
        borders+apple (merge borders {(new-apple borders) :apple})
        [cx cy]    [(/ width 2) (/ height 2)]
        snake      {[cx (dec cy)] 2 [cx cy] 1 [cx (inc cy)] 0}
        board      (merge borders+apple snake)]
    board))

(render-full! starting-grid)

; Game loop and control

(defn- key-to-direction [v]
  ({38 :up, 40 :down,  37 :left,  39 :right} (.-keyCode v)))

(defn- valid-key? [v]
  (let [k (.-keyCode v)]
    (not (nil? (some #{k} (range 37 41))))))

(defn- opposite? [a b]
  (= ({:up :down, :down :up, :left :right, :right :left} a) b))

(defn- step-loop [rate start]
  "Returns a channel that returns a direction at most every @rate milliseconds.
  If an arrow key is pressed within @rate ms, that arrow key is returned, else
  the previous one is returned.  The starting direction is in start."
  (let [out (chan)
        k   (listen js/window "keydown")]
    (go (loop [tick (timeout @rate)
               dir  start]
          (let [[v c] (alts! [k tick])]
            (condp = c
              tick (do
                     (>! out dir)
                     (<! (timeout 0))
                     (recur (timeout @rate) dir))
              k    (cond
                     (not (valid-key? v))
                        (recur tick dir)
                     (opposite? dir (key-to-direction v))
                        (recur tick dir)
                     true
                        (recur tick (key-to-direction v)))))))
    out))

(def rate (atom 750))

(defn- speed-up! []
  (let [s @rate
        n (- s 25)]
    (if (>= n 20)
      (swap! rate (fn [_] n)))))

(defn- has-apple [del]
  (not (empty? (filter (fn [v] (= (v 1) :apple)) del))))

(let [ch (step-loop rate :left)]
  (go (loop [grid starting-grid]
        (let [dir (<! ch)
              [add del] (snake-move grid dir)]
          (if (nil? add)
            (js/alert "Game over.")
            (do
              (when (has-apple del)
                (speed-up!))
              (render-updates! add del)
              (recur (update-grid grid add del))))))))
