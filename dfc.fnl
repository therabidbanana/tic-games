;; title:   Disastrous Flying Critters
;; author:  @therabidbanana@notvery.social
;; desc:    Help the Rainbow Princess Witch make World Peace
;; site:    website link
;; license: MIT License
;; version: 0.2
;; script:  fennel
;; strict:  true

(macro -- [val]
  `(set ,val (- ,val 1)))
(macro ++ [val]
  `(set ,val (+ ,val 1)))

(fn first [coll] (?. coll 1))
(fn last [coll] (?. coll (length coll)))
(fn nil? [x] (= x nil))

(fn ^in [x ...] (doto x (table.insert ...)))
(fn table? [x] (= (type x) :table))
(fn arr? [x] (and (table? x) (?. x 1)))

(fn cons [head rest]
  "Inserts head into rest, if array. Turns into array if not."
  (let [t? (table? rest)]
    (if
     (and (nil? head) t?)    rest
     (and (nil? rest) head)  [head]
     t?                      (^in rest 1 head)
     :else                   [head rest])))

(macro *args [val]
  `(if (arr? ,val) ,val (cons ,val [...])))

(macro hargs [val]
  `(if (and (not (arr? ,val)) (table? ,val))
       ,val
       (let [list# (cons ,val [...])
             acc# {}]
         (for [i# 1 (count list#) 2]
           (tset acc# (?. list# i#) (?. list# (+ i# 1))))
         acc#)))

(fn count [coll]
  (if (arr? coll)
      (length coll)
      (table? coll)
      (accumulate [count 0 i v (pairs coll)] (+ count 1))
      coll (length coll)
      nil))
(fn empty? [coll]
  (if (nil? coll) true (= 0 (count coll))))

(fn into [arr val ...]
  "Insert a list of values at the end of an existing list. "
  (assert (table? arr) "first arg must be collection")
  (let [coll (*args val)]
    (each [_ v (ipairs coll)]
      (^in arr v))
    arr))

(fn merge [h1 h2 ...]
  (let [new-hash
        (accumulate [new {}
                     k v (pairs h1)]
          (doto new (tset k v)))]
    (accumulate [new new-hash k v (pairs (hargs h2))] (doto new (tset k v)))))

(fn update [hash k func]
  (doto hash (tset k (func (?. hash k)))))

(fn min [a ...]
  "Find min of a collection (or list of arguments)"
  (let [coll (*args a)]
    (accumulate [min (first coll) _ v (ipairs coll)]
      (if (< v min) v min))))

(fn max [a ...]
  "Find max of a collection (or list of arguments)"
  (let [coll (*args a)]
    (accumulate [max (first coll) _ v (ipairs coll)]
      (if (< v max) max v))))

(fn sum [a ...]
  "Find sum of a collection (or list of arguments)"
  (let [coll (*args a)]
    (accumulate [acc 0 _ v (ipairs coll)]
      (if v (+ v acc) acc))))

(fn clamp [val min max]
  (if (> val max) max
      (< val min) min
      val))

(fn between? [val min max]
  (if (> val max) false
      (< val min) false
      true))

(fn mapv [func coll]
  (assert (table? coll) "last arg must be collection")
  (icollect [_ v (ipairs coll)] (func v)))

(fn filterv [func coll]
  (assert (table? coll) "last arg must be collection")
  (icollect [_ v (ipairs coll)]
    (if (func v) v)))

(fn mapiv [func coll]
  (assert (table? coll) "last arg must be collection")
  (icollect [i v (ipairs coll)] (func i v)))

(fn take [n coll]
  (assert (table? coll) "last arg must be collection")
  (accumulate [acc []
               i v (ipairs coll) :until (<= n (length acc))]
    (into acc v)))

(fn partition [arr max-cnt]
  "Takes an single dimension table and splits into pages of size count"
  (assert (table? arr) "first arg must be collection")
  (accumulate [acc [[]]
               index val (ipairs arr)]
    (if
     ;; If max <= current last size, new last
     (<= max-cnt (count (last acc)))
     (do (into acc [[val]]) acc)
     ;; Else, append to last
     (do (into (last acc) val) acc))))

(fn chars [str]
  (local acc [])
  (for [i 1 (count str)]
    (^in acc (string.sub str i i)))
  acc)

(fn words [str]
  (local acc [])
  (each [v (string.gmatch (string.gsub str "\n" " _NEWLINE_ ") "[^ \t]+")]
    (^in acc v))
  acc)

(fn fit-lines [{: text : w : chr-size}]
  (let [chr-size (or chr-size 5) ;; too small to handle non-mono
        max-len (// w chr-size)]
    (var l 1)
    (local acc [""])
    (each [i v (ipairs (words text))]
      (if (= :_NEWLINE_ v) (do
                             (++ l)
                             (tset acc l ""))
          ;; Else
          (do
            (if (= nil (?. acc l))
                (tset acc l ""))
            ;; Space + fudge every seventh letter because char-size too small
            (if (>= (+ 1 (// (length (. acc l)) 7) (length (. acc l)) (length v)) max-len)
                (do
                  (++ l)
                  (tset acc l v))
                ;; else
                (if (= "" (or (?. acc l) ""))
                    (tset acc l v)
                    (tset acc l (.. (. acc l) " " v)))))))
    acc))

(fn text-box-lines [{: text : w : chr-size}]
  "Takes text and splits into lines that will fit in the specified width"
  (let [w (min w 240)
        chr-size (or chr-size 5)
        len (count text)]
    (fit-lines {: text : w})))

(fn pages-for-lines [{: lines : max-h : h : chr-size}]
  (let [chr-size (or chr-size 7)
        max-h (or max-h 68)
        h (or h (min (* chr-size (count lines)) max-h))
        line-count (max (// h chr-size) 1)]
    (partition lines line-count)))

(fn inside? [{: x : y &as box} {:x x1 :y y1 &as point}]
  (and (>= x1 x) (<= x1 (+ x box.w))
       (>= y1 y) (<= y1 (+ y box.h))))

(fn touches? [{&as ent1} {&as ent2}]
  (and
   (< (+ ent1.x 0) (+ ent2.x ent2.w))
   (> (+ ent1.x ent1.w) (+ ent2.x 0))
   (< (+ ent1.y 0) (+ ent2.y ent2.h))
   (> (+ ent1.y ent1.h) (+ ent2.y 0))))

(fn collision-sides [{&as ent1} {&as ent2}]
  {:top (and (> ent1.y ent2.y)
             (< (+ ent1.y 0) (+ ent2.y ent2.h))
             (> (+ ent1.y ent1.h) (+ ent2.y 0)))
   :bottom (and (< ent1.y ent2.y)
                (< (+ ent1.y 0) (+ ent2.y ent2.h))
                (> (+ ent1.y ent1.h) (+ ent2.y 0)))
   :right (and (< ent1.x ent2.x)
                (< (+ ent1.x 0) (+ ent2.x ent2.w))
                (> (+ ent1.x ent1.w) (+ ent2.x 0)))
   :left (and (> ent1.x ent2.x)
              (< (+ ent1.x 0) (+ ent2.x ent2.w))
              (> (+ ent1.x ent1.w) (+ ent2.x 0)))})

;; --------------------
;; End Base Functions
;; --------------------


;; Testing functionality
(let [lines (text-box-lines {:text "This is a big page" :w 48 :max-h 12})]
  (assert (= 2 (count (pages-for-lines {:lines lines :max-h 14}))) "two pages of lines")
  (pages-for-lines {:lines lines :max-h 14}))

;; --------------------
;; UI Functions
;; --------------------



(global $ui {:components [] :t 0
             :defaults #(merge {:x 4 :y 80 :padding 5 :char-size 6
                                :bg-color 15 :border-color 12 :text-color 13}
                               (or $2 {}))
             :pop #(table.remove $.components)
             :clear-all! (fn [self tag]
                           (tset self :components (icollect [_ v (ipairs self.components)]
                                                    (if (and tag (not= tag v.tag)) v))))
             :push #(into $1.components $2)})

(fn box-styles [styles]
  (let [styles ($ui:defaults styles)
        pad2   (* styles.padding 2)
        w (- 236 styles.x)
        chars (if styles.chars
                  (+ (* styles.char-size styles.chars) pad2)
                  w)
        h (min 68 (- 135 styles.y))]
    (if (nil? styles.w) (tset styles :w (min w chars)))
    (if (nil? styles.h) (tset styles :h h))
    (merge styles {:inner-w (- styles.w pad2)
                   :inner-h (- styles.h pad2)})))

(fn pick-animated-sprite [{ : w : animate : ticks : sprite}]
  (if (nil? animate)
      sprite
      (let [{ : period : steps } animate
            new-steps  (mapiv #(if (table? $2)
                                    (merge {:index $1} $2)
                                    {:index $1 :t $2})
                              steps)
            time-spot   (% ticks period)
            sheet-index (or (last (mapv #(if (>= time-spot $.t) $.index) new-steps)) 1)
            sprite-num  (* (or w 1) (- sheet-index 1))]
        (+ sprite-num sprite))))

(fn draw-box! [{: w : h : x : y : bg-color : border-color}]
  (if bg-color (rect x y w h bg-color))
  (if border-color (rectb x y w h border-color)))

(fn draw-sprite! [{: sprite : w : h : scale : trans : x : y
                   : animate : ticks
                   : flip : rotate
                   : anchor-x : anchor-y : shift-x : shift-y
                   : box}]
  (let [sprite (pick-animated-sprite { : ticks : w : sprite : animate})
        scale (or scale 1)
        w (or w 1)
        full-w (* 8 w scale)
        h (or h 1)
        full-h (* 8 h scale)
        x (if (= anchor-x :center) (- x (// full-w 2))
              (= anchor-x :right) (- x full-w)
              x)
        y (if (= anchor-y :center) (- y (// full-h 2))
              (= anchor-y :bottom) (- y full-h)
              y)
        x (+ x (or shift-x 0))
        y (+ y (or shift-y 0))]
    (if box (draw-box! (merge {:x x :y y :w full-w :h full-h} box)))
    (spr sprite x y (or trans -1) scale (or flip 0) (or rotate 0) w h)))

(fn draw-map! [{: w : h : x : y : sx : sy : trans : on-draw : scale}]
  (map (or x 0) (or y 0) (or w 30) (or h 17) (or sx 0) (or sy 0) (or trans -1) (or scale 1) on-draw))

(fn draw-right-arrow! [{: x : y : ticks}]
  (let [wobble (if (> (% (or ticks 0) 70) 40)
                   1
                   0)
        x (+ x wobble)]
    (tri x y
         (- x 4) (+ y 3)
         (- x 4) (- y 3)
         13)))

(fn draw-down-arrow! [{: x : y : ticks}]
  (let [wobble (if (> (% (or ticks 0) 70) 40)
                   1
                   0)
        y (- y wobble)]
    (tri x y
         (+ x 3) (- y 4)
         (- x 3) (- y 4)
         13)))

(macro defui [ui comp fns]
  (let [comp! (.. comp "!")]
    `(doto ,ui
       (tset ,comp (doto ,fns (tset :component ,comp)))
       (tset ,comp! (fn [self# params#] ((. (. ,ui ,comp) :open)
                                         (. ,ui ,comp)
                                         params#))))))

(defui $ui
  :textbox
  {:open
   (fn [self {: box : text : tag : character : action}]
     (let [box (box-styles (or box {}))
           lines (text-box-lines {: text :w box.inner-w})
           pages (pages-for-lines {:max-h (max box.inner-h 12) : lines})
           page 1]
       ($ui:push (merge self {: action : character : tag : box : pages : page :ticks 0}))))
   :render
   (fn [{: box : page : pages : ticks : character}]
     (let [lines (?. pages page)
           more? (> (count pages) page)]
       (if lines
           (let [ticks (or ticks 1000)
                 u (+ box.x box.w -5)
                 v (+ box.y box.h -1)
                 letter-cnt (// ticks 2)
                 mid-x (+ box.x (// box.w 2))
                 mid-y (+ box.y (// box.h 2))
                 ]
             (if (and character (= character.position :left))
                 (draw-sprite! (merge character {:x box.x :y mid-y :anchor-x :right :anchor-y :center : ticks}))
                 character
                 (draw-sprite! (merge character {:x mid-x :y box.y :anchor-x :center :anchor-y :bottom : ticks})))
             (draw-box! box)
             (accumulate [prev-letters 0
                          ln line-text (ipairs lines)]
               (let [visible (- letter-cnt prev-letters)
                     tx (table.concat (take visible (chars line-text)) "")]
                 (print tx
                        (+ box.padding box.x)
                        (+ box.y box.padding -1 (* 8 (- ln 1)))
                        box.text-color)
                 (+ prev-letters (count line-text))))
             (if more? (draw-down-arrow! {:x u :y v : ticks }))))))
   :react
   (fn [self]
     (let [page (or self.page 1)
           lines (?. self.pages page)
           ticks (or self.ticks 0)]
       (if (btnp 4)
           (let [total-chars (sum (mapv count lines))
                 no-more (> (// ticks 2) total-chars)]
             (if no-more ;; unless typewriter effect still going
                 (do
                   (tset self :page (+ 1 page))
                   (tset self :ticks 0))
                 (do
                   (tset self :ticks (* total-chars 2))))))
       (tset self :ticks (+ (or self.ticks ticks) 1)))
     (if (> (or self.page 1) (or (count self.pages) 1))
         ;; Pop self
         (do
           ($ui:pop)
           (if self.action (self.action)))
         ))})

(defui $ui
  :menu
  {:open
   (fn [self {: box : options : tag}]
     (let [box (box-styles (merge (or box {}) {:chars 40}))
           selected 1]
       ($ui:push
        (merge self {: tag : box : options : selected :ticks 0}))))
   :render
   (fn [{: component : box : options : ticks : selected}]
     (let [lines (mapv #$.label options)]
       (let [ticks (or ticks 0)]
         (draw-box! box)
         (each [ln option (ipairs options)]
           (let [tx (?. option :label)
                 y-spot (+ box.y box.padding -1 (* 10 (- ln 1)))]
             (print tx (+ box.padding 4 box.x) y-spot box.text-color)
             (let [u (+ box.x 7)
                   v (+ y-spot 3)]
               (if (= selected ln)
                   (draw-right-arrow! {:x u :y v : ticks})))
             )))))
   :react
   (fn [self]
     (let [{ : selected : options} self
           dec-select  (max (- selected 1) 1)
           opt-count     (count options)
           inc-select  (min (+ 1 selected) opt-count)
           curr-option (?. options selected)
           action      (or curr-option.action #:no-action)
           keep-open?  curr-option.keep-open?
           ticks       (or self.ticks 0)]
       (if (btnp 0)
           (doto self
             (tset :ticks 0)
             (tset :selected dec-select))
           (btnp 1)
           (doto self
             (tset :ticks 0)
             (tset :selected inc-select))
           (btnp 4)
           (if keep-open?
               (action)
               (do ($ui:pop) (action))))
       (tset self :ticks (+ (or (?. self :ticks) ticks) 1))))})

(defui $ui
  :sprite-selector
  {:open
   (fn [self {: box : map : sprites : tag}]
     ;; TODO - visit box styles?
     (let [box (box-styles (merge (or box {}) {:chars 40}))
           arrow :down
           selected 1]
       ($ui:push
        (merge self {: tag : box : map : sprites : selected :ticks 0 : arrow}))))
   :render
   (fn [{: component : box : map : sprites : ticks : selected : arrow}]
     (let [arrow-fn (if (= arrow :right) draw-right-arrow! draw-down-arrow!)]
       (let [ticks (or ticks 0)]
         ;; (draw-box! box)
         (if map (draw-map! map))
         (each [idx ent (ipairs sprites)]
           ;; (print tx (+ box.padding 4 box.x) y-spot box.text-color)
           (draw-sprite! ent)
           ;; TODO - handle arrow placement
           (let [u (+ ent.x (// (* (or ent.w 1) 8) 2))
                 v (- ent.y 4)]
             (if (= selected idx)
                 (arrow-fn {:x u :y v : ticks})))))))
   :react
   (fn [self]
     (let [{ : selected : sprites} self
           dec-select  (max (- selected 1) 1)
           opt-count     (count sprites)
           inc-select  (min (+ 1 selected) opt-count)
           curr-option (?. sprites selected)
           action      (or curr-option.action #:no-action)
           keep-open?  curr-option.keep-open?
           ticks       (or self.ticks 0)]
       (if (or (btnp 0) (btnp 2))
           (doto self
             (tset :ticks 0)
             (tset :selected dec-select))
           (or (btnp 1) (btnp 3))
           (doto self
             (tset :ticks 0)
             (tset :selected inc-select))
           (btnp 4)
           (if keep-open?
               (action)
               (do ($ui:pop) (action))))
       (tset self :ticks (+ (or (?. self :ticks) ticks) 1))))})

;; Not clear if this should be true yet - render all components?
;; I think general idea is if a menu kicks a text box, menu still exists
(fn ui->display! []
  (each [i v (ipairs (or $ui.components []))]
    (v:render)))

(fn ui->active? [] (> (count $ui.components) 0))

(fn ui->react! []
  (let [v (last $ui.components)]
    (if (nil? v) v (v:react))))

;; ---------------------
;; Screen Management
;; ---------------------

;; Entity

(fn react-entities! [self screen-state]
  (if (not (ui->active?))
      (let [screen-state (or screen-state {})
            entities (or self.entities [])
            reacted (mapv #(let [up ($:react (merge (or $.state {}) screen-state) self)]
                             (if (= :die up)
                                 nil
                                 (table? up)
                                 (doto $ (tset :state up))
                                 $))
                          entities)]
        (tset self :entities reacted))))

(fn draw-entities! [self screen-state]
  (let [screen-state (or screen-state {})]
   (mapv #($:render (merge (or $.state {}) screen-state) self) self.entities)))

;; Handles swapping screens (title / game)
(global $screen
        {:tick! #(let [screen-tick (. (or $.active {:tick #:noop}) :tick)
                       new-state   (screen-tick $.active $.active.state)]
                   (tset $.active :state new-state)
                   (ui->react!)
                   (ui->display!))
         :draw! #(let [screen-draw (. (or $.active {:draw #:noop}) :draw)
                       new-state   (screen-draw $.active $.active.state)]
                   (ui->display!))
         :active nil
         :screens {}
         ;; Swap + prepare
         :select! (fn [self name ...] (let [screen (?. self.screens name)]
                                    (tset self :active screen)
                                    (screen:prepare ...)))
         ;; Switch without preparing (allows pause screens)
         :swap! (fn [self name] (let [screen (?. self.screens name)]
                                    (tset self :active screen)))
         :add! (fn [self screen] (let [name screen.screen]
                                   (tset self.screens name screen)))})

(macro defscreen [screen name fns]
  `(let [screen-comp# (merge {:screen ,name} ,fns)]
     (tset (. ,screen :screens) ,name screen-comp#)
     (: ,screen :add! screen-comp#)
     screen-comp#))

;; -------

(var t 0)
(var player-sprite 256)
(var enemy-portal-colors {:red 32 :orange 40 :yellow 80 :green 88 :blue 128 :purple 136 :white 176})
(var enemy-portal-tiles {32 :red 40 :orange 80 :yellow 88 :green 128 :blue 136 :purple 176 :white})

(fn draw-entity [{ : character &as ent} state {: bounds &as game}]
  (let [shifted-x (- state.x (or state.screen-x 0))
        shifted-y (- state.y (or state.screen-y 0))]
    (draw-sprite! (merge (merge character state) {:x shifted-x
                                                  :y shifted-y}))))

;; TODO: Start figuring out tile coloration
(fn tile-color [tile]
  (let [col (% tile 16)
        row (// tile 16)
        secondary? (>= col 8)]
    (if (>= row 12)
        :none
        (= tile 0)
        :none
        (< row 3)
        (if secondary? :orange :red)
        (< row 6)
        (if secondary? :green :yellow)
        (< row 9)
        (if secondary? :purple :blue)
        :else
        (if secondary? :grey :none)
        )))

(var tile-starts {:red 0     :orange 8
                  :yellow 48 :green 56
                  :blue 96   :purple 104
                  :grey 152})

(fn shift-tile-color [tile color]
  (let [col (% tile 16)
        row (// tile 16)
        current-color (tile-color tile)
        dist (- (?. tile-starts current-color) (?. tile-starts color))]
    (if (or (= current-color :none) (= color :none) (= tile 0))
        tile
        (< row 9)
        (- tile dist)
        tile)))

(fn bullet-react [{: color &as bullet} {: x : y : screen-x : screen-y &as state} {: entities &as game}]
  (let [collision   (bullet:collision-box)
        intersected (-?>> (filterv #(= :enemy $.tag) entities)
                          (filterv #(touches? collision ($:collision-box)))
                          (filterv #(not= $.color color))
                          first)
        x (+ x state.dx)
        y (+ y state.dy)
        {: would-paint?} (game:fetch-map-tile {: x : y : color})]
    (if intersected
        (do (intersected:take-damage! bullet) :die)
        (not (touches? {:x (+ -10 screen-x) :y (+ -10 screen-y) :w 260 :h 200} collision))
        :die
        would-paint?
        (do (game:paint-tile! {: x : y : color}) :die)
        (merge state {: x : y}))))

(var palette {:red 2 :orange 3 :yellow 4 :green 6 :blue 9 :purple 1})

(fn build-bullet [{: x : y : color : speed &as state}]
  (let [speed (or speed 2.5)
        dx    speed
        dy    0
        color (or color :red)]
    {:react bullet-react
     :state (merge state {: dx : dy : speed : color :h 2 :w 2})
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 2 :h 2})
     :color color
     :render (fn [{&as bullet} {: x : y : color : h : w : screen-x : screen-y} _e]
               (let [shifted-x (- x screen-x)
                     shifted-y (- y screen-y)]
                 (rect shifted-x shifted-y w h (?. palette color))))}))


(var color-cycle [:red :orange :yellow :green :blue :purple])
(var next-color {:red :orange :orange :yellow :yellow :green :green :blue :blue :purple :purple :red})
(var prev-color {:red :purple :orange :red :yellow :orange :green :yellow :blue :green :purple :blue})

(fn player-collides? [tile]
  (and tile.solid? (or tile.would-paint? (= tile.color :grey))))

(fn player-react [{: firstp : secondp &as self}
                  {: x : y : dx : dy : color : dir : hp : invuln &as state}
                  {: entities : bounds &as game}]
  (let [max-speed 1.5
        drag 0.05
        add-speed 0.5
        gravity 0.05
        dx (or dx 0)
        dy (or dy 0)
        dx (if (btn (if firstp 2 secondp 10)) (max (- dx add-speed) (* -1 max-speed))
               (btn (if firstp 3 secondp 11)) (min (+ dx add-speed) max-speed)
               (< dx 0) (min (+ dx drag) 0)
               (max (- dx drag) 0)
               )
        dy (if (btn (if firstp 0 secondp 8)) (min -1 dy)
               (btn (if firstp 1 secondp 9)) (min (+ dy add-speed) max-speed)
               (< dy -2) (+ dy (* 4 gravity))
               (< dy 0.15) (+ dy gravity)
               (min (+ dy gravity) 0.15))
        x (+ x dx)
        y (+ y dy)
        dir (if (< dx -0.1) -1 (> dx 0.1) 1 (or dir 1))
        left? (= dir -1)
        x (clamp x (* bounds.x 8) (- (* 8 (+ bounds.x bounds.w)) 8)) ;; Limit to edges
        y (clamp y (- (* bounds.y 8) 8) (+ (* 8 (+ bounds.y bounds.h)) -12))
        collision   (self:collision-box)
        intersected (-?>> (filterv #(= :enemy $.tag) entities)
                          (filterv #(touches? collision ($:collision-box)))
                          first)
        invuln (or invuln 0)
        collisions (if intersected
                       (collision-sides (intersected:collision-box) collision))
        damaged? (and intersected (<= invuln 0) (not collisions.top))
        bounced? (and intersected collisions.top)
        hp (or hp 3)
        hp (if damaged? (- hp 1) hp)
        color (if (btnp (if firstp 6 secondp 14)) (?. prev-color color)
                  (btnp (if firstp 5 secondp 13)) (?. next-color color)
                  damaged? intersected.color
                  color)
        new-invuln (if damaged? 200 (max (- (or invuln 1) 1) 0))
        dy (if (and bounced? (btn (if firstp 1 secondp 9))) -4 bounced? -2.5 dy)
        y (if bounced? (+ y dy) y)
        ;; Handle bouncing
        foot-tile (game:fetch-map-tile {:x (+ x 7) :y (+ y 14) : color})
        dy (if (player-collides? foot-tile) (min dy 0) dy)
        y  (if (player-collides? foot-tile) (- foot-tile.y 14) y)
        head-tile (game:fetch-map-tile {:x (+ x 7) :y (+ y 2) : color})
        dy (if (player-collides? head-tile) (max dy 0.15) dy)
        y  (if (player-collides? head-tile) (+ head-tile.y 6) y)
        right-tile (game:fetch-map-tile {:x (+ x 14) :y (+ y 6) : color})
        dx (if (player-collides? right-tile) (min dx 0) dx)
        x  (if (player-collides? right-tile) (- right-tile.x 14) x)
        left-tile (game:fetch-map-tile {:x x :y (+ y 6) : color})
        dx (if (player-collides? left-tile) (max dx 0) dx)
        x  (if (player-collides? left-tile) (+ left-tile.x 8) x)
        ]
    (if bounced?
        (do
          (sfx 16 "E-5" 8 0 4)
          (intersected:take-damage! {: color : x : y})))
    (if damaged?
        (do
          (sfx 17 "G#7" 16 1 7))
        )
    (if (btnp (if firstp 4 secondp 12))
        ;; TODO: Is there a less sneaky way to add entity?
        (do 
          (sfx 18 "D-6" 8 0 4)
          (^in entities (build-bullet {:x (+ x (if left? 0 15)) :y (+ y 10) : color :speed (if left? -2 2)}))))
    (if (<= hp 0)
        :die
        (merge state {: x : y : dx : dy : color : dir :invuln new-invuln : hp}))))

(defscreen $screen :pause
  {:tick
   (fn []
     (cls 14)
     (print "Paused..." 84 24 13))
   :prepare
   (fn []
     (poke 0x03FF8 8)
     ($ui:clear-all!)
     ($ui:menu! {:box {:x 50 :w 140}
                 :options [{:label "Play" :action #($screen:swap! :game)}
                           {:label "Quit" :action #($screen:select! :title)}]}))})

(var portraits {
                :princess {:position :left :sprite 201 :w 4 :h 4 :trans 0 :box {:bg-color 0 :border-color 13}}
                :advisor {:position :left :sprite 161 :w 4 :h 4 :trans 0 :box {:bg-color 0 :border-color 13}}
                })
(fn dialog-chain [after-action dialog ...]
  (let [next-dialogs [...]]
    (if (empty? next-dialogs)
       ($ui:textbox! (merge dialog :action after-action))
       ($ui:textbox! (merge dialog :action #(dialog-chain after-action (table.unpack next-dialogs)))))))


(defscreen $screen :intro
  {:tick
   (fn []
     (cls 0)
     )
   :draw
   (fn []
     )
   :prepare
   (fn []
     (sync 0)
     (poke 0x03FF8 0)
     ($ui:clear-all!)
     (dialog-chain #(do
                      ;; NEW GAME logic
                      (tset $screen.screens.map :completions {})
                      ($screen:select! :map))
                   {:box {:x 34}
                    :character portraits.advisor
                    :text "Princess! Please help us! It's a disaster!"}
                   {:box {:x 34 :y 0}
                    :character portraits.princess
                    :text "What's happening?"}
                   {:box {:x 34}
                    :character portraits.advisor
                    :text "There's a bunch of critters, flying around the kingdoms, causing all sorts of destruction!"}
                   {:box {:x 34 :y 0}
                    :character portraits.princess
                    :text "Where did they come from?"}
                   {:box {:x 34}
                    :character portraits.advisor
                    :text "They appear to be crawling out of colorized portals!"}
                   {:box {:x 34 :y 0}
                    :character portraits.princess
                    :text "What!? I'll see what I can do with my Rainbow Witch powers!"}
                   {:box {:x 34}
                    :character portraits.advisor
                    :text "Be careful! They appear to be immune to weapons of their own color!"}
                   {:box {:x 34 :y 0}
                    :character portraits.princess
                    :text "Good thing I can change colors on demand!"}
                   )
     )})


(fn completion-rate [color current]
  (let [all-tiles (+ (sum (mapv #(or (?. current $) 0) color-cycle))
                     (or current.grey 0))
        all-tiles (max all-tiles 1) ;; Hack around possible div/0
        chosen    (or (?. current color) 0)]
    (// (* (/ chosen all-tiles) 100) 1)))

(defscreen $screen :outro
  {:tick
   (fn []
     (cls 0)
     )
   :draw
   (fn []
     )
   :prepare
   (fn [self completions]
     (let [total-completion (sum (mapv #(completion-rate $1
                                                         (or (?. completions $1) {}))
                                       color-cycle))
           total-completion-rate (// (* 100 (/ total-completion 600)) 1)]
       (poke 0x03FF8 0)
       ($ui:clear-all!)
       (dialog-chain #($screen:select! :title)
                     {:box {:x 34}
                      :character portraits.advisor
                      :text "Great work princess! Did all the monsters go away?"}
                     {:box {:x 34 :y 0}
                      :character portraits.princess
                      :text "Yes! All the portals have been closed. Peace is restored to the kingdom."}
                     {:box {}
                      :text (.. "Completion Rate: " total-completion-rate "%")}))
     )})

(defscreen $screen :map
  {:tick
   (fn []
     (cls 0)
     )
   :draw
   (fn []
     )
   :completions {}
   :prepare
   (fn [self level-name color-bar]
     (let [completions  self.completions
           completions  (if level-name
                            (merge self.completions {level-name color-bar})
                            completions)
           level-select (fn -level-select [color]
                          (if
                           (?. completions color)
                           (do
                             (dialog-chain #:noop
                                           {:box {:x 34 :y 0}
                                            :character portraits.princess
                                            :text (.. "Looks like I've already helped that world! - " (completion-rate color (?. completions color)) "%")
                                            }))
                           ;; else
                           (do
                             (set $screen.screens.game.level color)
                             ($screen:select! :game))))
           sprite-pick  (fn -sprite-pick [color] (if (?. completions color)
                                                     enemy-portal-colors.white
                                                     (?. enemy-portal-colors color)))
           map-details
           {
            :map {:x 210 :y 17 :trans 0}
            :sprites [{:h 1 :w 1 :sprite (sprite-pick :red) :trans 0 :x 32 :y 16
                       :keep-open? (?. completions :red)
                       :action #(level-select :red)}
                      {:h 1 :w 1 :sprite (sprite-pick :orange) :trans 0 :x 80 :y 40
                       :keep-open? (?. completions :orange)
                       :action #(level-select :orange)}
                      {:h 1 :w 1 :sprite (sprite-pick :yellow) :trans 0 :x 160 :y 16
                       :keep-open? (?. completions :yellow)
                       :action #(level-select :yellow)}
                      {:h 1 :w 1 :sprite (sprite-pick :green) :trans 0 :x 200 :y 72
                       :keep-open? (?. completions :green)
                       :action #(level-select :green)}
                      {:h 1 :w 1 :sprite (sprite-pick :blue) :trans 0 :x 104 :y 112
                       :keep-open? (?. completions :blue)
                       :action #(level-select :blue)}
                      {:h 1 :w 1 :sprite (sprite-pick :purple) :trans 0 :x 24 :y 72
                       :keep-open? (?. completions :purple)
                       :action #(level-select :purple)}
                      ]}
           completed-count (-> (filterv #(?. completions $) color-cycle) count)]
       (if (< completed-count 6)
           (do
             (tset self :completions completions)
             (poke 0x03FF8 0)
             ($ui:clear-all!)
             ($ui:sprite-selector! map-details)
             (dialog-chain #:noop
                           {:box {:x 34 :y 0}
                            :character portraits.princess
                            :text "Where should I go?"
                            }))
           ;; Game end
           (do
             ($screen:select! :outro completions))))
     )})


(defscreen $screen :title
  {:state {}
   :tick
   (fn [self {&as screen-state}]
     {:ticks (+ (or screen-state.ticks 0) 1)})
   :draw
   (fn [self {: ticks &as screen-state}]
     (cls 0)
     (draw-sprite! {:sprite 16 :x 10 :y 4 :w 12 :h 4 :trans 0})
     (draw-sprite! {:sprite 80 :x 36 :y (+ 25 (* 5 (math.sin (% (/ ticks 60) 60)))) :w 12 :h 4 :trans 0})
     (draw-sprite! {:sprite 161 :x 60 :y 48 :w 10 :h 3 :trans 0})
     (draw-sprite! {:sprite 460 :x 150 :y 48 :w 4 :h 4 :trans 0})
     ;; (print "Disastrous Flying" 22 22 15 false 2)
     ;; (print "Critters" 68 52 15 false 2)
     ;; (print "Disastrous Flying" 23 23 13 false 2)
     ;; (print "Critters" 69 53 13 false 2)
     )
   :prepare
   (fn []
     (sync 0 1)
     (poke 0x03FF8 15)
     ($ui:clear-all!)
     ($ui:menu! {:box {:x 50 :w 140}
                 :options [{:label "Play Game" :action #(do (set $screen.screens.game.two_mode false)
                                                            (set $screen.screens.intro.two_mode false)
                                                            (set $screen.screens.map.two_mode false)
                                                            ($screen:select! :intro))}
                           {:label "Play Two Player" :action #(do (set $screen.screens.game.two_mode true)
                                                                  (set $screen.screens.intro.two_mode true)
                                                                  (set $screen.screens.map.two_mode true)
                                                                  ($screen:select! :intro))}]}))})

(var sprite-colors {:red 256 :orange 264 :blue 320 :green 296 :purple 328 :yellow 288})

(fn build-player [base-state first-player]
  {:render (fn draw-player [{: character &as ent} {: dir : color : invuln &as state} _others]
             (let [sprite (if (> (% (or invuln 0) 33) 29)
                              (?. sprite-colors (?. next-color color))
                              (and (< (% (or invuln 0) 33) 9) (not= 0 invuln))
                              (?. sprite-colors (?. prev-color color))
                              (?. sprite-colors color))
                   flip (if (> (or dir 1) 0) 0 1)
                   shifted-x (- state.x state.screen-x)
                   shifted-y (- state.y state.screen-y)
                   ]
               (draw-sprite! (merge (merge character state)
                                    {:x shifted-x :y shifted-y : sprite : flip}))))
   :react player-react
   :collision-box (fn [{: state}] {:x (+ state.x 5) :y (+ state.y 4) :w 10 :h 10})
   :state (merge {:x 0 :y 0 :color :yellow}
                 base-state)
   :tag :player
   :firstp  first-player
   :secondp  (not first-player)
   :character
   {;; Test weird blink patterns
    :animate {:period 200 :steps [{:t 0 :index 1} {:t 100 :index 2} {:t 112 :index 1}
                                  {:t 115 :index 3} {:t 130 :index 1}]}
    :trans 0
    :w 2 :h 2}})

(fn enemy-react [{: color &as self}
                 {: hp : x : y : dx : dy : ticks : cycle &as state}
                 { : bounds &as game}]
  (let [left-bound (* (* bounds.x 8))
        right-bound (* (+ bounds.x bounds.w) 8)
        top-bound (+ (* bounds.y 8) 1)
        bottom-bound (* (+ bounds.y bounds.h) 8)
        x (clamp (if dx (+ x dx) x)
                 left-bound
                 (- right-bound 8))
        y (clamp (if dy (+ y dy) y)
                 top-bound
                 (- bottom-bound 8))
        dx (if (< x (+ left-bound 1))
               (max (- 0 dx) 0.2)
               (> x (- right-bound 16))
               (min (- 0 dx) -0.2)
               dx)
        dy (if (< y (+ top-bound 1))
               (max (- 0 (or dy 1)) 0.2)
               (> y (- bottom-bound 16))
               (min (- 0 (or dy 1)) -0.2)
               (math.sin (* (/ ticks cycle) 1)))
        {: would-paint? } (game:fetch-map-tile {:x (+ x 7) :y (+ y 7) : color})]
    (if (<= hp 0)
        :die
        (do
          (if would-paint? (game:paint-tile! {:x (+ x 7) :y (+ y 7) : color}))
          (merge state {: x : y : dx : dy})))))

(var enemy-sprite-colors {:red 384 :orange 392 :yellow 416 :green 424 :blue 448 :purple 456})
(fn build-enemy [{: color &as base-state}]
  (let [color (or color :red)
        sprite (?. enemy-sprite-colors color)
        cycle (+ 10 (* 50 (math.random)))]
    {:render draw-entity
     :react enemy-react
     :tag :enemy
     :critter true
     :color color
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 16 :h 16})
     :state (merge {: color : cycle} (or base-state {}))
     :take-damage! (fn [self bullet]
                     (tset self.state :hp (- (or self.state.hp 1) 1)))
     :character
     {:sprite sprite :trans 0 :w 2 :h 2}}))

(fn player-nearby? [{: x : y &as state} entities]
  (let [nearby-players
        (-?>> (filterv #(= :player $.tag) entities)
              (filterv #(< (math.abs (- $.state.x x)) 240))
              (filterv #(< (math.abs (- $.state.y y)) 144))
              count)]
    (> nearby-players 0)))

(fn portal-react [{: color &as self}
                  {: hp : cycle : x : y : dx : dy : ticks : stationary? &as state}
                  {: entities : bounds &as game}]
  (let [left-bound (* (* bounds.x 8))
        right-bound (* (+ bounds.x bounds.w) 8)
        top-bound (+ (* bounds.y 8) 1)
        bottom-bound (* (+ bounds.y bounds.h) 8)
        x (clamp (if dx (+ x dx) x)
                 left-bound
                 (- right-bound 8))
        y (clamp (if dy
                     (+ y dy)
                     y)
                 top-bound
                 (- bottom-bound 8))
        dx (if (< x (+ left-bound 1))
               (max (- 0 dx) 0.2)
               (> x (- right-bound 16))
               (min (- 0 dx) -0.2)
               dx)
        dy (if (< y (+ top-bound 1))
               (max (- 0 (or dy 1)) 0.2)
               (> y (- bottom-bound 16))
               (min (- 0 (or dy 1)) -0.2)
               stationary?
               (* -0.15 (math.sin (/ ticks 40)))
               (math.sin (/ ticks 40)))
        cycle (or cycle (+ 197 (// (* (math.random) 90) 1)))
        {: would-paint? } (game:fetch-map-tile {:x (+ x 7) :y (+ y 7) : color})
        player-is-nearby? (player-nearby? state entities)]
    (if (<= hp 0)
        :die
        (do
          (if would-paint? (game:paint-tile! {:x (+ x 7) :y (+ y 7) : color}))
          (if (and (= (% ticks cycle) 0) player-is-nearby?)
              (game:add-entity! (build-enemy {:color color
                                              :dx (if stationary?
                                                      (+ -0.5 (* -1 (+ (math.random) 0.05)))
                                                      (* -1 (+ (math.random) 0.05)))
                                              :x (+ x (- 10 (* 10 (math.random))))
                                              :y (+ y (- 10 (* 10 (math.random))))
                                              :hp 1})))
          (merge state {: x : y : dx : dy : cycle})))))

(fn build-portal [{: color : hp : stationary? &as base-state}]
  (let [color (or color :red)
        hp    (or hp 10)
        sprite (?. enemy-portal-colors color)]
    {:render (fn [self {: x : y : screen-x : screen-y : hp : max-hp &as state} {&as game}]
               (let [x (- x screen-x)
                     y (- y screen-y)]
                 (draw-box! {:x (+ x 2) :y (- y 4) :w 12 :h 3 :border-color (?. palette color)})
                 (draw-box! {:x (+ x 3) :y (- y 3) :w (* (/ hp max-hp) 10) :h 1 :bg-color (?. palette color)})
                 (draw-entity self state game)))
     :react portal-react
     :tag :enemy
     :portal true
     :color color
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 16 :h 16})
     :state (merge {: color :max-hp hp : hp} (or base-state {}))
     :take-damage! (fn [self bullet]
                     (tset self.state :hp (- (or self.state.hp 1) 1)))
     :character
     {:sprite sprite :trans 0 :w 1 :h 1 :scale 2}}))

(fn build-home-portal [{: color : hp &as base-state}]
  (let [color (or color :white)
        sprite (?. enemy-portal-colors color)]
    {:render draw-entity
     :react (fn [{: color &as self}
                 {: hp : timer-ticks : cycle : x : y : dx : dy :  ticks : color-bar &as state}
                 {: entities : level &as game}]
              (let [dy (* 0.15 (math.sin (/ ticks 40)))
                    player-ent (->> (filterv #(= :player $.tag) entities)
                                    (filterv #(= $.firstp true))
                                    first)]
                (if (touches? (self:collision-box) (player-ent:collision-box))
                    ;; ($ui:textbox! {:box {:x 34}
                    ;;                :character portraits.princess
                    ;;                :text "Time to head to the next problem!"
                    ;;                :action #($screen:select! :title)})
                    ($screen:select! :map level color-bar)
                    (> (or timer-ticks 0) (* 60 60))
                    ;; ($ui:textbox! {:box {:x 34}
                    ;;                :character portraits.princess
                    ;;                :text "Time to head to the next problem!"
                    ;;                :action #($screen:select! :title)})
                    ($screen:select! :map level color-bar)
                    :else
                    (merge state {:timer-ticks (+ (or timer-ticks 0) 1) :y (+ y dy) :dy dy}))))
     :tag :home
     :portal true
     :color :white
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 16 :h 16})
     :state (merge {: color :max-hp hp : hp} (or base-state {}))
     :take-damage! (fn [self bullet] :noop)
     :character
     {:sprite sprite :trans 0 :w 1 :h 1 :scale 2}}))

(fn draw-hud-colorbar [current]
  (let [all-tiles (+ (sum (mapv #(or (?. current $) 0) color-cycle))
                     (or current.grey 0))
        red-portion (* 234 (/ (or current.red 0) all-tiles))
        orange-portion (* 234 (/ (or current.orange 0) all-tiles))
        yellow-portion (* 234 (/ (or current.yellow 0) all-tiles))
        green-portion (* 234 (/ (or current.green 0) all-tiles))
        blue-portion (* 234 (/ (or current.blue 0) all-tiles))
        purple-portion (* 234 (/ (or current.purple 0) all-tiles))
        grey-portion (* 234 (/ (or current.grey 0) all-tiles))
        ]
    ;; (print (.. "All tiles: " all-tiles) 10 10 13)
    ;; (print (.. "Red tiles: " current.red) 10 30 13)
    (draw-box! {:x 3 :y 3 :w red-portion :h 2 :bg-color palette.red})
    (draw-box! {:x (sum 3 red-portion) :y 3 :w orange-portion :h 2 :bg-color palette.orange})
    (draw-box! {:x (sum 3 red-portion orange-portion) :y 3 :w yellow-portion :h 2 :bg-color palette.yellow})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion) :y 3 :w green-portion :h 2 :bg-color palette.green})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion green-portion) :y 3 :w blue-portion :h 2 :bg-color palette.blue})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion green-portion blue-portion) :y 3 :w purple-portion :h 2 :bg-color palette.purple})
    (draw-box! {:x (sum 3 red-portion orange-portion yellow-portion green-portion blue-portion purple-portion) :y 3 :w grey-portion :h 2 :bg-color 14})
    (draw-box! {:x 2 :y 2 :w 236 :h 4 :border-color 12})
    ))

(fn capitalize-word [str]
  (str:gsub "^%l" string.upper))

(fn draw-hud [{: level : entities &as game} { : screen-x : screen-y : color-bar : ticks}]
  (let [portals (filterv #(?. $ :portal) entities)
        critters (filterv #(?. $ :critter) entities)
        home    (-> (filterv #(= $.tag :home) entities) first)
        enemy-count (-> (filterv #(= :enemy $.tag) entities) count)]
    ;; (print (.. "screen-x: " screen-x) 10 20 13)
    (if (> enemy-count 0)
        (do
          (print (.. "Portals: " (count portals)) 4 10 13 false 1 true)
          (print (.. "Critters: " (count critters)) 180 10 13 false 1 true))
        home
        (do 
          (print (.. (capitalize-word level) " restored: " (completion-rate level color-bar) "%") 4 8 13 false 1 true)
          (print (.. "Time Remaining... " (- 60 (// (or home.state.timer-ticks 0) 60)) "s")
                 158 8 13 false 1 true))
        )
    )
  (draw-hud-colorbar color-bar))

(fn draw-sky! [{: ticks : screen-x}]
  )

(fn draw-stats [player first-player?]
  (if first-player?
      (do
        (print (.. "1P: " (or player.state.hp 3)) 11 121 15)
        (print (.. "1P: " (or player.state.hp 3)) 10 120 13)
        )
      (do
        (print (.. "2P: " (or player.state.hp 3)) 191 121 15)
        (print (.. "2P: " (or player.state.hp 3)) 190 120 13)
        )
      )
  ;; (print (.. "x:" (or first-player.state.x 0)) 10 110 13)
  ;; (print (.. "y:" (or first-player.state.y 0)) 10 100 13)
  )

(fn mark-grey-tiles [{: level : entities : bounds : two_mode &as self} percentage]
  (for [x bounds.x (- (+ bounds.x bounds.w) 1)]
    (for [y bounds.y (- (+ bounds.y bounds.h) 1)]
      (let [tile (mget x y)]
        (if (> (* 100 (math.random)) (- 100 percentage))
            (self:paint-tile! {:color :grey :x (* x 8) :y (* y 8)}))))))

(fn spawn-players! [{: level : entities : bounds : two_mode &as self} during-game]
  (let [first-player (->> (filterv #(= $.firstp true) self.entities) first)]
    (if first-player
        :noop
        (do
          (if during-game
              ($ui:textbox! {:box {:x 34} :text "Ouch!" :character portraits.princess}))
          (for [x bounds.x (- (+ bounds.x bounds.w) 1)]
            (for [y bounds.y (- (+ bounds.y bounds.h) 1)]
              (let [tile (mget x y)]
                (if
                 (= 240 tile)
                 (let [centered-x (- (* x 8) 120)]
                   (tset self.state :screen-x centered-x)
                   (tset self.state :screen-y (* bounds.y 8))
                   (self:add-entity! (build-player {:invuln (if during-game 200 0) :x (* x 8) :y (* y 8) :color level} true)))
                 (> (* 100 (math.random)) 95)
                 (let []
                   (if during-game (self:paint-tile! {:color :grey :x (* x 8) :y (* y 8)}))
                   ))))))))
  (let [first-player (->> (filterv #(= $.firstp true) self.entities) first)
        second-player (->> (filterv #(= $.secondp true) self.entities) first)]
    (if (or second-player (not two_mode))
        :noop
        (do
          (if during-game
              ($ui:textbox! {:box {:x 34} :text "oof!" :character portraits.princess}))
          (let [x (+ first-player.state.x 8)
                y (+ first-player.state.y 8)]
            (self:add-entity! (build-player {:invuln (if during-game 200 0) :x x :y y :color level} false)))
          (if during-game (mark-grey-tiles self 5))))))

(fn spawn-home-portal! [{: entities : bounds &as self} during-game]
  (let [home-ent    (->> (filterv #(= :home $.tag) self.entities) first)
        enemy-count (->> (filterv #(= :enemy $.tag) self.entities) count)]
    (if (or home-ent (> enemy-count 0))
        :noop
        (do
          ($ui:textbox! {:box {:x 34} :text "Looks like I can head home. Maybe I should clean up a bit first?" :character portraits.princess})
          ;; Look through tile bounds
          (for [x bounds.x (- (+ bounds.x bounds.w) 1)]
            (for [y bounds.y (- (+ bounds.y bounds.h) 1)]
              (let [tile (mget x y)]
                (if
                 (= 240 tile)
                 (let [shifted-y (- y 6)]
                   (self:add-entity! (build-home-portal {:x (* x 8) :y (* shifted-y 8)})))
                 ))))))))

(var test-level-bounds
     {:yellow {:x 0 :y 0
               :w (* 30 5) :h 17}
      :red {:x 0 :y 0
            :w (* 30 5) :h 17}
      :orange {:x 0 :y 0
               :w (* 30 5) :h 17}
      :blue {:x 0 :y 17
             :w (* 30 3) :h 20}
      :purple {:x 0 :y 17
               :w (* 30 3) :h 17}
      :green {:x 0 :y 17
              :w (* 30 3) :h 17}
      })

(var real-level-bounds
     {:yellow {:x (* 30 5) :y (* 17 4)
               :w (* 30 2) :h (* 17 4)}
      :red {:x 0 :y (* 17 4)
            :w (* 30 5) :h 17}
      :orange {:x 0 :y 0
               :w (* 30 5) :h 17}
      :blue {:x 0 :y (* 17 3)
             :w (* 30 3) :h 17}
      :purple {:x 0 :y (* 17 2)
               :w (* 30 6) :h 17}
      :green {:x 0 :y (* 17 5)
              :w (* 30 4) :h (* 17 2)}
      })


(defscreen $screen :game
  {:state {}
   :tick
   (fn [{: bounds &as self} {: ticks : color-bar : screen-x : screen-y &as screen-state}]
     ;; (if (btnp 7) ($screen:select! :pause))
     (react-entities! self screen-state)
     (spawn-players! self true)
     (spawn-home-portal! self true)
     ;; (draw-sky! screen-state)
     (let [player-ent (->> (filterv #(= $.firstp true) self.entities) first)
           player-offset-x (- player-ent.state.x screen-x)
           diffx (- (clamp player-offset-x 80 160) player-offset-x)
           shifted-x (- screen-state.screen-x diffx)
           new-screen-x (clamp shifted-x (* 8 bounds.x) (- (* 8 (+ bounds.x bounds.w)) 240))
           player-offset-y (- player-ent.state.y screen-y)
           diffy (- (clamp player-offset-y 34 94) player-offset-y)
           shifted-y (- screen-state.screen-y diffy)
           new-screen-y (clamp shifted-y (* 8 bounds.y) (* 8 (+ bounds.y bounds.h -17)))
           second-player (->> (filterv #(= $.secondp true) self.entities) first)]
       ;; second player dragging along logic
       (if (and second-player (or (< second-player.state.x (- new-screen-x 8))
                                  (> second-player.state.x (+ new-screen-x 220))))
           (tset second-player.state :x
                 (clamp (- second-player.state.x diffx)
                        (* 8 bounds.x)
                        (* 8 (+ bounds.x bounds.w)))
                 ))
       (if (and second-player (or (< second-player.state.y (- new-screen-y 8))
                                  (> second-player.state.y (+ new-screen-y 128))))
           (tset second-player.state :y
                 (clamp (- second-player.state.y diffy)
                        (* 8 bounds.y)
                        (* 8 (+ bounds.y bounds.h)))))
       (if (= (% ticks 60) 0)
           (self:recalculate-color-bar!))
       (if (empty? self.entities)
           ($screen:select! :title))
       ;; (icollect [_ v (ipairs self.entities)]
       ;;   (if (and (= :enemy v.tag) (> screen-state.ticks 60))
       ;;       (v:take-damage! {})))
       ;; (print (.. "Count " (count self.entities)) 20 20 13 )
       (cls 8) ;; Allow pretty sky
       {:ticks (+ screen-state.ticks 1) :screen-x new-screen-x : color-bar :screen-y new-screen-y}))
   :draw
   (fn [{: bounds &as self} {: screen-x : screen-y : color-bar &as screen-state}]
     (let [player-ent  (->> (filterv #(= $.firstp true) self.entities) first)
           player2-ent (->> (filterv #(= $.secondp true) self.entities) first)]
       (draw-sky! screen-state)
       (draw-map! {:x bounds.x :w bounds.w
                   :y bounds.y :h bounds.h
                   :sx (- 0 (- screen-x (* bounds.x 8))) :sy (- 0 (- screen-y (* bounds.y 8)))
                   :trans 0
                   :on-draw (fn [tile x y]
                              (if (between? tile 242 247)
                                  (do (self:add-entity!
                                       (build-portal {
                                                      :color (?. color-cycle (- tile 241))
                                                      :dx -0.5 :x (* x 8) :y (* y 8) :hp 10}))
                                      (mset x y 0)
                                      0)
                                  (?. enemy-portal-tiles tile)
                                  (do (self:add-entity!
                                       (build-portal {
                                                      :color (?. enemy-portal-tiles tile)
                                                      :dx 0 :x (* x 8) :y (* y 8) :hp 10
                                                      :stationary? true
                                                      :cycle 97
                                                      }))
                                      (mset x y 0)
                                      0)
                                  (= tile 240)
                                  (do (set self.state.home-x (* x 8))
                                      (set self.state.home-y (* y 8))
                                      tile)
                                  tile)
                              )
                   })
       (draw-entities! self screen-state)
       (if player-ent (draw-stats player-ent true))
       (if player2-ent (draw-stats player2-ent false))
       (draw-hud self screen-state))
     )
   :entities []
   :add-entity! (fn [self ent] (into self.entities [ent]))
   :fetch-map-tile (fn fetch-map-tile [{: state : bounds &as self} {: x : y : color}]
                     (let [tile-x (// (+ x 0) 8)
                           tile-y (// (+ y 0) 8)
                           tile (mget tile-x tile-y)
                           colorable? (between? tile 1 144)
                           tile-color (tile-color tile)
                           solid? (fget tile 0)
                           oob? (or (< tile-x bounds.x) (> tile-x (+ bounds.x bounds.w))
                                    (< tile-y bounds.y) (> tile-y (+ bounds.y bounds.h)))
                           would-paint? (and
                                         (not= tile-color :grey)
                                         (not= tile-color :none)
                                         (not= color tile-color)
                                         (not oob?))]
                       {:x (* 8 (- tile-x 0))
                        :y (* 8 (- tile-y 0))
                        : tile-x : tile-y : oob? : solid? : tile : colorable? :color tile-color : would-paint?}))
   :paint-tile! (fn [{: state &as self} {: x :  y : color &as input}]
                  (let [{: tile-x : tile-y
                         :color tile-color : would-paint?
                         : tile : colorable? } (self:fetch-map-tile input)
                        in-color (or (?. state.color-bar color) 0)
                        out-color (or (?. state.color-bar tile-color) 0)]
                    (if (or would-paint? (and colorable? (= color :grey)))
                        (do
                          (doto state.color-bar
                            (tset color (+ in-color 1))
                            (tset tile-color (max (- out-color 1) 0)))
                          (mset tile-x tile-y (shift-tile-color tile color))))))
   :recalculate-color-bar!
   (fn [{: state : bounds : level &as self}]
     (let [{: map-y} state
           color-bar {:grey 0}]
       (for [x bounds.x (- (+ bounds.x bounds.w) 1)]
         (for [y bounds.y (- (+ bounds.y bounds.h) 1)]
           (let [t-color (tile-color (mget x y))
                 curr-val (or (?. color-bar t-color) 0)]
             (if (not= t-color :none)
                 (tset color-bar
                       t-color
                       (+ curr-val 1))))))
       (tset state :color-bar color-bar)))
   :prepare
   (fn prepare-game [self]
     (poke 0x03FF8 0)
     (tset self :entities [])
     (tset self :bounds (?. real-level-bounds self.level))
     (tset self :state {:ticks 0 :screen-x (* self.bounds.x 8) :color-bar {} :screen-y (* self.bounds.y 8)})
     ($ui:clear-all!)
     (self:recalculate-color-bar!)
     (spawn-players! self false)
     ;; (self:add-entity! (build-enemy {:dx -0.5 :x 200 :y 40 :hp 1}))
     ;; (self:add-entity! (build-enemy {:dx -0.5 :dy 1 :x 240 :y 100 :hp 1}))
     )})

(fn _G.BDR [line]
  (let [scans 288
        PALETTE_ADDR 0x03FC0
        CHANGE_COL 8
        color (* (// (* 0xff (/ line scans)) 8) 8)]
    (poke (+ (+ (* CHANGE_COL 3) 0) PALETTE_ADDR) color)
    (poke (+ (+ (* CHANGE_COL 3) 1) PALETTE_ADDR) color)
    (poke (+ (+ (* CHANGE_COL 3) 2) PALETTE_ADDR) color)))

(fn _G.BOOT []
  ($screen:select! :title)
  )

(fn _G.TIC []
  ($screen:tick!))

(fn _G.OVR []
  ($screen:draw!))

;; <TILES>
;; 001:0111111111222222122233321222222212233222123342321232233212322222
;; 002:1111111122222222222332222222222233222332222222222222222222222222
;; 003:1111111022222211233322212222222122233221232433212332232122222321
;; 004:4112211413322331044224400042240000222200002222000200202020002002
;; 009:0222222222333333233333332333333323333333233323332333333323333333
;; 010:2222222233333333333333333333333333333333333333333333333333333333
;; 011:2222222033333322333333323333333233333332333233323333333233333332
;; 012:0223322000030000000330000033330003323230333333332333333302222220
;; 016:0000000000000000002222000024220000222200002222000024222000242220
;; 017:1222222212223222122232221232222212322222122222221222322212223222
;; 018:2223222222223332222232223232223323322232232223322222222222232222
;; 019:2222222122232221222322212222232122222321222222212223222122232221
;; 020:0020000000220200002222005002220005022000005500000005000000050000
;; 024:0000000000000000003333000033330003333330033333303334433333344333
;; 025:2333333323333333233333332333333323333333233333332333333323333333
;; 026:3333333332333333333333333332233333223333332333333333323333333333
;; 027:3333333233333332333333323333333233333332333333323333333233333332
;; 028:0000000000303000003330000033300006333660006656000006660000006000
;; 032:0022220002000000200222002020002020202020200220202000002002222200
;; 033:1222222212322332123222321232342212223322122223331122222201111111
;; 034:2222222222222222222222223322233222222222222332222222222211111111
;; 035:2222222123322321232223212243232122332221333222212222221111111110
;; 040:0033330003000000300333003030003030303030300330303000003003333300
;; 041:2333333323332223233333222333323223333333233333332233333302222222
;; 042:3333333333333333333333333333333333333333333333333333333322222222
;; 043:3333333233333332333333323323333233333332333333323333332222222220
;; 049:0111111111444444144444341444444414443444144444441444434414444444
;; 050:1111111144444444444344444444444444444434444444444444444444444444
;; 051:1111111044444411434444414444444144444441434443414444444144444341
;; 052:0141141004444440043343400433434004444440044444400000000000000000
;; 057:0777777777555555766666667665666676666566765667767666666776667666
;; 058:7777777755555555666666666665666666666665656666666766656676666666
;; 059:7777777055555577666666676665666766666667657766676666756766666767
;; 060:0775577000055000005555000555555065577555655775556557755506666660
;; 064:0000000000000000003333300034443003344433004444400044244000442440
;; 065:1444444414443444144444441444444414444444144444441444444414444444
;; 066:4444444444444444444444443444444344444444434443444444444444434444
;; 067:4444444144444441444344414444444144444341444444414443444144444441
;; 068:0444444044344444434443444434443404433344004444400006600000066000
;; 072:006666000665566006655660066ff66006666660066ff6600667766006677660
;; 073:7666766676666776766666677666666676666666766666667666677676666667
;; 074:6766666666766666676667766676775666676666667666667766666667666666
;; 075:6666766766666777766676676757666766776667666676676666676767666667
;; 076:0000066000006600066660000066006600660660000666000004600000044000
;; 080:0044440004000000400444004040004040404040400440404000004004444400
;; 081:1444444414444344143444341444444414444344144444431144444401111111
;; 082:4444444444444444444444444444344444444444444444444444444411111111
;; 083:4444444144444341434444414444434144434441334444414444441111111110
;; 088:0066660006000000600666006060006060606060600660606000006006666600
;; 089:7666666776666666766666667666666676666666766666667766666607777777
;; 090:7666666676666666676666666676666666766666666766666667666677777777
;; 091:7667666765766667676666676676666777666667667666676666667777777770
;; 097:0888888888aaaaaa8aa999998a9999998a9999998a9999998a9999998a999999
;; 098:88888888aaaaaaaa999999999999999999999999999999999999999999999999
;; 099:88888880aaaaaa8899999aa8999999a8999999a8999999a8999999a8999999a8
;; 100:88acca880000c0000000c0000000c0000c00c00c0ac0c0ca00acaca000000000
;; 105:0888888888221111811111118111111181111112811111118112211181111111
;; 106:8888888811111111111111221111111121111111111111111111111111111111
;; 107:8888888011111188111111181122111811111118111111181111122811111118
;; 108:0881188330111130032222003021120303111130011111100118811000811800
;; 112:0000000000000000008888800089998008899988009999900099b9900099b990
;; 113:8a9999998a9999998a9999998a9999998a9999998a9999998a9999998a999999
;; 114:9999999999999b99999999999999999999aa999999ba99999999999999999999
;; 115:999999a8999999a899a999a899b999a8999999a8999999a8999999a8999999a8
;; 116:000000000000000000aba0000baaa0007aa5aba007baa0000077000000070000
;; 120:0000000000000000001111000013110000111100000110000003100000031000
;; 121:8111111181111111811111118111111181111222811111118111111181111111
;; 122:1111111111111221111111111111111121111111111111111111111111111111
;; 123:1111111811111118111111182111111811111118111111181111122811111118
;; 124:1000000101000010001111001002200101122110000550000005500000055000
;; 128:0099990009000000900999009090009090909090900990909000009009999900
;; 129:8a9999998a9999998a9999998a999ab98a999aa98aa9999988aaaaaa08888888
;; 130:999999999999999999999999999999999999999999999999aaaaaaaa88888888
;; 131:999999a8999999a8999999a8999999a8999999a89999aaa8aaaaaa8888888880
;; 136:0011110001000000100111001010001010101010100110101000001001111100
;; 137:8111111181221111811111118111112281111111811111118811111108888888
;; 138:1111111111111111111111112111111111111111111112211111111188888888
;; 139:1111111822211118111111181111111811122118111111181111118888888880
;; 144:0000000c00000000000000000000000000000000000000000000000000000000
;; 153:0fffffffffddeeeefeefeeeefeeefeeffeeeeffffeeeeeeffeeddefefeeeefee
;; 154:ffffffffeeeeeeeeeeeeeeddfffffeeedeeeefffeeeffeeeeeefeeeeeeeeffff
;; 155:fffffff0eeeeeeffeeeeeeefeeddeeefeffeeeefeeffeeefeefeeddfffeeeeef
;; 156:0ffeeff0000ee0000000e000000ee00000ee000000ee0000000e0e00000ee000
;; 160:00000000000000000000000000d0000000000000000000000000000000000000
;; 161:0000000000000006000000660000006600000066000006660000066600000666
;; 162:6666666666666666666666666666666666777777677ccccc67cccccc67ccffcc
;; 163:6666666666666666666666666666666677777776cccccc77ccccccc7ccffccc7
;; 164:0000000060000000660000006660000066600000666000006660000066660000
;; 168:000000000000dd00000eedd00000eed000eeee000feeeff00ffdeff0ddfdeff0
;; 169:feeefeeefeeeffeefeeeeeeefeeeeeeefeeeedddfeeeeeeefeeeefeefeeffeff
;; 170:feeeeeefefeeeddfeefffefeefeefefefeeeefeefeeeeefefeeeeeeffeeeeeef
;; 171:feeeeeefefeeeeefeeffeeefdeeffeefeeeeeeefeeeeeeefeeeeeddfeeeeeeef
;; 172:00000000e00000000e00000e00ee00e0000efe00000eee00000ef000000ef000
;; 176:00cccc000c000000c00ccc00c0c000c0c0c0c0c0c00cc0c0c00000c00ccccc00
;; 177:00000666000006660000066600000777000007770000000c0000000c00000003
;; 178:67ccffcc77ccfccc77cccccc7ccccccc7ccccccccccccccccccc33333cc33333
;; 179:ccffccc7ccfcccccddccccccccdddcccccccdcccccccdccc33cc333333333333
;; 180:76660000766600007777000077770000cc000000cc000000cc00000033300000
;; 181:00000ccc00cccccc0ccccccc0ccccccccccccccccccccccccccccccc0ccccccc
;; 182:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;; 185:feeeeeeffeddeeeffeeeeeeefeeeeeddfeeeeeeefeeeeeeeffeeeeee0fffffff
;; 186:eeeeeeeeeffeeefefeffefeedeeeffeeeeeeefeeeeeeefffeeeeeeeeffffffff
;; 187:eeeeeeefdddeeeefeeefeeefeeffeeefefffdeeffeeffeefeeeeeefffffffff0
;; 192:00000000000000000000040000000000000000000d0000000000000000000000
;; 193:0000003300000033000000330000033300000333000003330000033300000333
;; 194:333333cc333ccccc33cccccc33ccccee333ccccc33333ccc333333cc33333333
;; 195:33333333cccccc33ccccc333eeecc333ccccc333ccccc333cc33333333333333
;; 196:3330000033300000333000003330000033300000333000003330000033300000
;; 197:cccccccccccccccccccccccc0ccccccc0ccccccc0ccccccc0cccccc000000000
;; 198:cccccccccccccccccccccccccccccccccccccccc00cccccc0000c00000000000
;; 199:cc000000cccccc00ccccccc0ccccccc0ccccccc0cccccc000000000000000000
;; 202:440000004444a0a0444440a404444aa4a4424444a9422444a9444444aa444444
;; 203:44400000444a00a44444aa044444aa4422444444224444424444444444444444
;; 204:0400000044a0000044aa000044aa000024aaa000244aaa00444aa9004444aa00
;; 208:00000000000000000d0000000000000000000000000000d00000000000000000
;; 209:0000033300011133001111130111111101111111011111110011111100111111
;; 210:3333333333333333333333331333333311111111111111111111111111111111
;; 211:33333333333333333313333311111ccc111cdcbb11ccbcbb1cccbbbb12233344
;; 212:3330000033440000c444d000cccee000cdce1000cbce1100bbce110066691100
;; 213:000000000ccccccccccccccccccccccccccccccccccccccccccccccc0cccccc0
;; 214:000000000ccccc00ccccccc0cccccccccccccccccccccccccccccccccccccccc
;; 217:0000000000000000000000000000000a0000000a000000aa00000aaa0000aaa9
;; 218:a94eeeeeaaeecccca98ccccca98cacac998ca55599ac58ff9a9cc8df999ccccc
;; 219:eeeee444cccdeeeecccccccccccccacacccc555a5cc5fff5cdccfdfcddcccccc
;; 220:4444aaa04444aaa0eee49aaaccc9aaaaccc999aaccca99aacccc9aaacccc99aa
;; 224:00cccccc0c000123c0000964c0000000c00000b0c00000bbc00000bbcbbb000b
;; 225:cccccc00100dddc0100000dc100000dcb0b0000cbbb0000cbbc0000cbb0c0bbc
;; 226:00cccccc0c000222c0000222c0000000c00000b0c00000bbc00000bbcbbb000b
;; 227:00cccccc0c000333c0000333c0000000c00000b0c00000bbc00000bbcbbb000b
;; 228:00cccccc0c000444c0000444c0000000c00000b0c00000bbc00000bbcbbb000b
;; 229:00cccccc0c000666c0000666c0000000c00000b0c00000bbc00000bbcbbb000b
;; 230:00cccccc0c000999c0000999c0000000c00000b0c00000bbc00000bbcbbb000b
;; 231:00cccccc0c000111c0000111c0000000c00000b0c00000bbc00000bbcbbb000b
;; 233:000aaaa900aa9a9900a99a990aa9aa990a99aa990a9aa9990a9aa99a0a9aaa99
;; 234:a99ccccca99ccccc999ccccca99ccccc9999ccc19a99cccc9999accc99999ccc
;; 235:dcccccccdcccccccdddccccccccccccc1111cccc111ccccccccccccccccccccc
;; 236:cccca9aacc3c99a0dcca99a0cd9a99a0ca99a9a0c99a99a0a9999aa0a99999a0
;; 240:cbbb000bcbfbb00bcbfbbbbbcbfbbbbbcbbbbbb2cbbbbd33cb888d22cb888d22
;; 241:bb00cbbcbb00bfbcbbbbbfbcbbbbbfbc2bbbbbbc33dbbbbc22d888bc22d888bc
;; 242:0000000000000000002020000002000000202000000000000000000000000000
;; 243:0000000000000000003030000003000000303000000000000000000000000000
;; 244:0000000000000000004040000004000000404000000000000000000000000000
;; 245:0000000000000000006060000006000000606000000000000000000000000000
;; 246:0000000000000000009090000009000000909000000000000000000000000000
;; 247:0000000000000000001010000001000000101000000000000000000000000000
;; 249:0a9a9999009a9aa9009a99990099a9a90000a9990000a99a00000a6600000666
;; 250:99a999ac999999669a9966669a66666696666666666666666666666666666666
;; 251:ccccccec3ceeeeec3ccccccc63ccccc3663cc336666336666666666666666666
;; 252:a9a9a9a036a9a9aa3669aaaa66666aaa6666666a666666666666666666666666
;; </TILES>

;; <TILES1>
;; 021:000000000000000000000ff0000000ff0000000f00000ff00d000f000de0ffff
;; 022:000000000000000000000000000000000ff0000000f0000000f00000ff000000
;; 025:000fee00000ff0000000fe000000fe00000efe0000eef00000efff000000eeff
;; 026:0e000000ee000000fe000000ff0000000fe00000ef000000f0000000fe000000
;; 032:0eeeeeeeededddddede00000fdef0ff0fdefff00fdeff000ffdff00ffddff0f0
;; 033:ee000000feee0000dddfee0000ddee00000ddeed0000deedff00def00ff00df0
;; 034:00000000000000000000000000000000dde00000dde00000dde0dddd0000d000
;; 035:0000000f000ff00f0ff0f0000000f0ff0000ffff00000000000ddd0000deed00
;; 036:ff000000f0000000fff00000fff00dddfffddfff0edd00000ed000000edde000
;; 037:0deff00f0def00000def0000dddddddddfffffddde00000dde00000dde00000d
;; 038:f00000000000000000000000e0ddd00eddddddeddeeeedddde0000ddde0000dd
;; 039:0000000000000000eee00000ddddd000dddddd000000ddd0ff000ed0f0fffed0
;; 040:000000000000000000000000d0000dd0d0000dd0de000dd0de000dd0de000dde
;; 041:000000ee0000eddd0000dfff000def0f000ddef00000ddde000000dd0000000d
;; 042:e0000000d0000000dd000000fd000000f000000000000000e0000000de000000
;; 048:fddf00f0fddf00ffffdff000ffdff000ffdfff0fffdfffffffdffffff0deeeee
;; 049:00f00df0f0f0fded0ffffdfd0fffedfdfffeedfdfffeddfdfeeddffdeedfffff
;; 050:f00dd0e0f000dddef00000ddf000000df00dd000fffdddddddfffeeeffffffff
;; 051:00df0d0000df0de0e0df0deed0df0ddddeddddfdee00d0ffe00ffffffffff0f0
;; 052:00dddde00000ddde00d000ddeed000dddeddddffff0ffffffff0ffffffffffff
;; 053:de00000dde00000ddd00000dfde00d00fddedd00fffddffffffffffffffffff0
;; 054:d00000d0d00000d0d00000d0d00000ddd00ffffdffffffffffffffff00ffffff
;; 055:f0f0fed0f00ffed0ffffedd00eeedd00dddd0f00ffffffffffffff00000000ff
;; 056:de000ddfde000ddfde000ddfde00eddfddfffddffdddddddffddddffffffffff
;; 057:000000000000000000000f00e000d0ffff00eddfddddeeddddddeeeefffffff0
;; 058:dde000000dde00000fdee000ffdee000fddeee00deee0000ee00000000000000
;; 064:00eeeedd00dddfff00dd0ff00000000000000000000000000000000000000000
;; 065:dffffffffffff000000000000000000000000000000000000000000000000000
;; 066:ffffffffffffff00000000000000000000000000000000000000000000000000
;; 067:ff0fffff00000000000000000000000000000000000000000000000000000000
;; 068:ffffffff0fffffff000fffff0000000000000000000000000000000000000000
;; 069:ffffff00fffff000000000000000000000000000000000000000000000000000
;; 072:ffffffff00000fff000000000000000000000000000000000000000000000000
;; 073:ffffff00ff000000000000000000000000000000000000000000000000000000
;; 081:0000000000000000000000000000000000000000000000220002222200022222
;; 082:0000000000000000000000000000002200222222222222222222222f22fffff0
;; 083:0000000000000000000000000000030020033300000333000003330000033300
;; 084:0000000000000000000000000000000000000000000000000000000000440000
;; 085:0000000600000006000000060000000f00000000000000000040000604400006
;; 086:660000006600000066000000ff000000000000006600f9906600ff9966000fe9
;; 087:00000000000000000000000000000000009999900999999999fff9999ff0fff9
;; 088:0000000000000000000000010000001100000011900000119000001199000011
;; 089:000000000111111111111111111111111100000f1000000f1000000f1000000f
;; 090:0000000011110000111110001110000011000000110000001100000011100000
;; 091:0010000000110000001100000011000001111000011110001111100011111100
;; 096:0000000000000000002200000022000000222000002222200022222200222222
;; 097:0002222200022222000200220000002200002222002222222222222222222222
;; 098:2ff00000ff000000000000000000000000000000000000000000000002222222
;; 099:000333000003330000033300000f3300000f3300000f3300000f3300200f3300
;; 100:4444000000440000004440000044400000444000004440000044400e00444444
;; 101:044000060444000004440000044440000444f0000444f0004444f00044440000
;; 102:66600ff9066000f9066000090660000906600009066000090660000906600009
;; 103:9f0000f9900000f9900000ff9000000f90000000900000009000000090000000
;; 104:9900001199000011990000019900000f99000000999000009990000099900000
;; 105:100000ff11000ff111111111f1111111fffffff1000000f1000000f1000000f1
;; 106:1100000111111111111111111111111111111111111111011fff110f100ff100
;; 107:1111110011111100111111001111111011111110111f1110110ff111f100ff11
;; 112:00222222022222220222222202222222022f2222022f2222022f022202ff022f
;; 113:2222f2222222f2222222f222222ff22222ff0222f2f00222f2f00222f0f00222
;; 114:222222ff2222fff02ffff0002f00000020000000200000002000000020000000
;; 115:f00f3300000f3300000f3330000f3330000f3330000f3330000fff330000ff33
;; 116:000ff444000000000000000000000000000000003300000033004000330f4000
;; 117:4f44000000440000004400000044000000440000004400000044000000440000
;; 118:06600009066000090000000000000d0000e11111001111110111111011000000
;; 119:9000000090000000000000000d00d00d11111111111111110000101100000000
;; 120:099000000000000000d000d01111111111111111111111111100000000000000
;; 121:000000f100001111011111111111111111111110111110000000000000000000
;; 122:1000f1001000f000100000001000000000000000000000000000000000000000
;; 123:ff000f110f1000110f0000f10000000100000001000000000000000000000000
;; 128:02f0022f02f0002f020000000000000000000000000000000000000000000000
;; 129:0000002200000022000000220000000200000000000000000000000000000000
;; 130:2000000020000000000000000000000000000000000000000000000000000000
;; 131:00000fff00000000000000000000000000000000000000000000000000000000
;; 132:ff0f4400000ff4440000f44400000fff00000000000000000000000000000000
;; 133:00440000444ff00044fff000fff0000000000000000000000000000000000000
;; 134:1100000010000010100001110100001101100001001100110001111100000000
;; 135:0000000000000000111000001110000011100000111000000110000000000000
;; 161:0000000000000000000000000000000000000000000000000000000d0000000d
;; 162:0000000000000ddd00dddddd0dddeeeeddde0000dde00000de000000de000000
;; 163:00000000d0000000ddd00000eee0000000000000000000000000000000000000
;; 164:0000000000000000000000000000000000000000000000dd000000dd000000dd
;; 165:0000000000000d0000000d0000000d000000dd000000dde00000ddee0000dddd
;; 166:000000d0000000d000000dd000000dd00000edd00eeedddd00ddddddd0dddd00
;; 167:00000000000000000000000000000000dde00ddddd00ddff0000dff00000de00
;; 168:00000000000000000000000000000000ddd00000ffdddd000eed0ddd0ddd00dd
;; 169:0000000000000000000000000000000000000000dddddd00ddd0ddd0d0000dd0
;; 170:00000000000000000000000000000000e00000000ddddd00dd00ddd0d0000dd0
;; 177:0000000d0000000d0000000d0000000d0000000d0000000d0000000d00000000
;; 178:de000000de000000de000000de000000de000000de000000dd000000dde00000
;; 179:00000000000000de000000dd000000dd00000edd000dd0dd000dd0dd000dd0dd
;; 180:ddddd000deeee0d0d0000dd000000dd000000dd000000de000000de000000dde
;; 181:0ddddddd0edddd000000dd000000dde00000dee00000dde00000dde00000ddde
;; 182:0000dde00000dde00000dde00000dde00000ddee00e00dde0ee00dddeed00ddd
;; 183:0000dddd0000de000000ddf00ee0ddf0eed0ddf0edd00dffdd000dddd00000dd
;; 184:dd0000dd000000dd0000000d000e000d0ede000dffdd000ddddd000dddd0000d
;; 185:d00000d0de000000de000000de000000de000000de00000dd000000ed000000e
;; 186:ddd000d000dd0000000dd000000dd0000000ddd0d0000dd0d00eddd0ddddddd0
;; 194:dddee00000ddeee0000ddddd0000000000000000000000000000000000000000
;; 195:00dd00dd0dd000dddd0000000000000000000000000000000000000000000000
;; 196:00000ddd000000dd000000000000000000000000000000000000000000000000
;; 197:d0000dddd0000ddd000000000000000000000000000000000000000000000000
;; 198:ddd00000dd000000000000000000000000000000000000000000000000000000
;; 200:0000000d00000000000000000000000000000000000000000000000000000000
;; 201:d000000e00000000000000000000000000000000000000000000000000000000
;; 202:eeeee00000000000000000000000000000000000000000000000000000000000
;; 208:00000000000000000d0000000000000000000000000000d00000000000000000
;; 209:0000033300011133001111130111111101111111011111110011111100111111
;; 210:3333333333333333333333331333333311111111111111111111111111111111
;; 211:33333333333333333313333311111ccc111cdcbb11ccbcbb1cccbbbb12233344
;; 212:3330000033440000c444d000cccee000cdce1000cbce1100bbce110066691100
;; 213:000000000ccccccccccccccccccccccccccccccccccccccccccccccc0cccccc0
;; 214:000000000ccccc00ccccccc0cccccccccccccccccccccccccccccccccccccccc
;; 224:00cccccc0c000123c0000964c0000000c00000b0c00000bbc00000bbcbbb000b
;; 225:cccccc00100dddc0100000dc100000dcb0b0000cbbb0000cbbc0000cbb0c0bbc
;; 226:00cccccc0c000222c0000222c0000000c00000b0c00000bbc00000bbcbbb000b
;; 227:00cccccc0c000333c0000333c0000000c00000b0c00000bbc00000bbcbbb000b
;; 228:00cccccc0c000444c0000444c0000000c00000b0c00000bbc00000bbcbbb000b
;; 229:00cccccc0c000666c0000666c0000000c00000b0c00000bbc00000bbcbbb000b
;; 230:00cccccc0c000999c0000999c0000000c00000b0c00000bbc00000bbcbbb000b
;; 231:00cccccc0c000111c0000111c0000000c00000b0c00000bbc00000bbcbbb000b
;; 240:cbbb000bcbfbb00bcbfbbbbbcbfbbbbbcbbbbbb2cbbbbd33cb888d22cb888d22
;; 241:bb00cbbcbb00bfbcbbbbbfbcbbbbbfbc2bbbbbbc33dbbbbc22d888bc22d888bc
;; 242:0000000000000000002020000002000000202000000000000000000000000000
;; 243:0000000000000000003030000003000000303000000000000000000000000000
;; 244:0000000000000000004040000004000000404000000000000000000000000000
;; 245:0000000000000000006060000006000000606000000000000000000000000000
;; 246:0000000000000000009090000009000000909000000000000000000000000000
;; 247:0000000000000000001010000001000000101000000000000000000000000000
;; </TILES1>

;; <SPRITES>
;; 000:0000000000000000000000000000000000000022000002220000022200002222
;; 001:0000000000000000040404002244400022cccc0024cfcf0022cccc0023333000
;; 002:0000000000000000000000000000000000000022000022220002222200022222
;; 003:0000000000000000040404002244400022cccc0024cfcf0022cccc0023333000
;; 004:0000000000000000000000000002220000000222000022220002222200022222
;; 005:0000000000000000040404002244400022cccc0024cfcf0022cccc0023333000
;; 008:0000000000000000000000000000000000000033000003330000033300003333
;; 009:0000000000000000040404003344400033cccc0034cfcf0033cccc0034444000
;; 010:0000000000000000000000000000000000000033000033330003333300033333
;; 011:0000000000000000040404003344400033cccc0034cfcf0033cccc0034444000
;; 012:0000000000000000000000000003330000000333000033330003333300033333
;; 013:0000000000000000040404003344400033cccc0034cfcf0033cccc0034444000
;; 016:0000222222202222222205552255553300000033000000c00000002000000000
;; 017:33333000333333003333555533330000333000000c0000000200000000000000
;; 018:0000222022202220222205552255553300000033000000c00000002000000000
;; 019:33333000333333003333555533330000333000000c0000000200000000000000
;; 020:0000000022200000222205552255553300000033000000c00000002000000000
;; 021:33333000333333003333555533330000333000000c0000000200000000000000
;; 024:0000333333303333333305553355554400000044000000c00000003000000000
;; 025:44444000444444004444555544440000444000000c0000000300000000000000
;; 026:0000333033303330333305553355554400000044000000c00000003000000000
;; 027:44444000444444004444555544440000444000000c0000000300000000000000
;; 028:0000000033300000333305553355554400000044000000c00000003000000000
;; 029:44444000444444004444555544440000444000000c0000000300000000000000
;; 032:0000000000000000000000000000000000000044000004440000044400004444
;; 033:0000000000000000050505004455500044cccc0045cfcf0044cccc0046666000
;; 034:0000000000000000000000000000000000000044000044440004444400044444
;; 035:0000000000000000050505004455500044cccc0045cfcf0044cccc0046666000
;; 036:0000000000000000000000000004440000000444000044440004444400044444
;; 037:0000000000000000050505004455500044cccc0045cfcf0044cccc0046666000
;; 040:0000000000000000000000000000000000000066000006660000066600006666
;; 041:0000000000000000040404006644400066cccc0064cfcf0066cccc0069999000
;; 042:0000000000000000000000000000000000000066000066660006666600066666
;; 043:0000000000000000040404006644400066cccc0064cfcf0066cccc0069999000
;; 044:0000000000000000000000000006660000000666000066660006666600066666
;; 045:0000000000000000040404006644400066cccc0064cfcf0066cccc0069999000
;; 048:0000444444404444444405554455556600000066000000c00000004000000000
;; 049:66666000666666006666555566660000666000000c0000000400000000000000
;; 050:0000444044404440444405554455556600000066000000c00000004000000000
;; 051:66666000666666006666555566660000666000000c0000000400000000000000
;; 052:0000000044400000444405554455556600000066000000c00000004000000000
;; 053:66666000666666006666555566660000666000000c0000000400000000000000
;; 056:0000666666606666666605556655559900000099000000c00000006000000000
;; 057:99999000999999009999555599990000999000000c0000000600000000000000
;; 058:0000666066606660666605556655559900000099000000c00000006000000000
;; 059:99999000999999009999555599990000999000000c0000000600000000000000
;; 060:0000000066600000666605556655559900000099000000c00000006000000000
;; 061:99999000999999009999555599990000999000000c0000000600000000000000
;; 064:0000000000000000000000000000000000000099000009990000099900009999
;; 065:0000000000000000040404009944400099cccc0094cfcf0099cccc0091111000
;; 066:0000000000000000000000000000000000000099000099990009999900099999
;; 067:0000000000000000040404009944400099cccc0094cfcf0099cccc0091111000
;; 068:0000000000000000000000000009990000000999000099990009999900099999
;; 069:0000000000000000040404009944400099cccc0094cfcf0099cccc0091111000
;; 072:0000000000000000000000000000000000000011000001110000011100001111
;; 073:0000000000000000040404001144400011cccc0014c7c70011cccc0012222000
;; 074:0000000000000000000000000000000000000011000011110001111100011111
;; 075:0000000000000000040404001144400011cccc0014c7c70011cccc0012222000
;; 076:0000000000000000000000000001110000000111000011110001111100011111
;; 077:0000000000000000040404001144400011cccc0014c7c70011cccc0012222000
;; 080:0000999999909999999905559955551100000011000000c00000009000000000
;; 081:11111000111111001111555511110000111000000c0000000900000000000000
;; 082:0000999099909990999905559955551100000011000000c00000009000000000
;; 083:11111000111111001111555511110000111000000c0000000900000000000000
;; 084:0000000099900000999905559955551100000011000000c00000009000000000
;; 085:11111000111111001111555511110000111000000c0000000900000000000000
;; 088:0000111111101111111105551155552200000022000000c00000001000000000
;; 089:22222000222222002222555522220000222000000c0000000100000000000000
;; 090:0000111011101110111105551155552200000022000000c00000001000000000
;; 091:22222000222222002222555522220000222000000c0000000100000000000000
;; 092:0000000011100000111155551155552200000022000000c00000001000000000
;; 093:22222000222222002222555522220000222000000c0000000100000000000000
;; 104:00000000000000000000000000000000000000aa00000aaa00000aaa0000aaaa
;; 105:000000000000000005050500aa555000aacccc00a1c7c700aacccc00a1111000
;; 106:00000000000000000000000000000000000000aa0000aaaa000aaaaa000aaaaa
;; 107:000000000000000005050500aa555000aacccc00a1c7c700aacccc00a1111000
;; 108:000000000000000000000000000aaa0000000aaa0000aaaa000aaaaa000aaaaa
;; 109:000000000000000005050500aa555000aacccc00a1c7c700aacccc00a1111000
;; 120:0000aaaa9990aaaa999906669966661100000011000000c00000002000000000
;; 121:11111000111111001111666611110000111000000c0000000200000000000000
;; 122:0000aaa09990aaa0999906669966661100000011000000c00000002000000000
;; 123:11111000111111001111666611110000111000000c0000000200000000000000
;; 124:0000000099900000999906669966661100000011000000c00000002000000000
;; 125:11111000111111001111666611110000111000000c0000000200000000000000
;; 128:0000004000000442000044320003422200032222222233222222222222302212
;; 129:0000000000000000400000004000000022400000334022202222222012223220
;; 136:0444444404444444043333330432232204321322033233223333333333033333
;; 137:4444444044444440333333402232234012312340223323303333333333333033
;; 144:23002222000222ff000222220002022200000022000002220000333300000000
;; 145:22200320f2222000222020002220200022200000220000003220000000000000
;; 152:300333333003332200033322000333420003334f000333330000330000003300
;; 153:33333003423330034233300022333000f2333000333330000033000000330000
;; 160:000000000000000004444444000000000004444400045f440004550000000044
;; 161:0000000000000000444444000444400044444440445f40000455400044000000
;; 168:0000000000000000006000000666000006677700666077776607777466077777
;; 169:0000000000000000666600006666600006666660777666604740666077706660
;; 176:0000044400000044000000440000000000000004000000000000000000000000
;; 177:4440000044000000440000004400000040000000440000004000000000000000
;; 184:6007777707777677777766777707760070007700000077770000000700000000
;; 185:c7c0000077000000667000007770000007770000000000000000000000000000
;; 192:0000000011999999015999950999999909959959099999990999999909999999
;; 193:0000000011100000190000009900000099000000990000009990099999999999
;; 200:0000002200000223000000210000002100000221000222210022333100000001
;; 201:2000000022000000200000002200000012222200133320001000000010000000
;; 208:0099999900000099000009990000099000000090000000000000000000000000
;; 209:999990099ff99000999990000a0990000a0a00000a0a00000000000000000000
;; 216:0000000100000001000000010000001100000011000000110000011100000111
;; 217:11000000110000001110000011100000e11e0000111110001eee100011111000
;; 240:0000000000001010000041400000111001111110011111101101001010000000
;; 241:00cc000c000c0cc0000c0c000002c20000cccc00dccccc00cccc0c00000c0000
;; 242:0000000000000000003333440323340000323000000000000000000000000000
;; </SPRITES>

;; <SPRITES1>
;; 205:440000004444a0a0444440a404444aa4a4424444a9422444a9444444aa444444
;; 206:44400000444a00a44444aa044444aa4422444444224444424444444444444444
;; 207:0400000044a0000044aa000044aa000024aaa000244aaa00444aa9004444aa00
;; 220:0000000000000000000000000000000a0000000a000000aa00000aaa0000aaa9
;; 221:a94eeeeeaaeecccca98ccccca98cacac998ca55599ac58ff9a9cc8df999ccccc
;; 222:eeeee444cccdeeeecccccccccccccacacccc555a5cc5fff5cdccfdfcddcccccc
;; 223:4444aaa04444aaa0eee49aaaccc9aaaaccc999aaccca99aacccc9aaacccc99aa
;; 236:000aaaa900aa9a9900a99a990aa9aa990a99aa990a9aa9990a9aa99a0a9aaa99
;; 237:a99ccccca99ccccc999ccccca99ccccc9999ccc19a99cccc9999accc99999ccc
;; 238:dcccccccdcccccccdddccccccccccccc1111cccc111ccccccccccccccccccccc
;; 239:cccca9aacc3c99a0dcca99a0cd9a99a0ca99a9a0c99a99a0a9999aa0a99999a0
;; 252:0a9a9999009a9aa9009a99990099a9a90000a9990000a99a00000a6600000666
;; 253:99a999ac999999669a9966669a66666696666666666666666666666666666666
;; 254:ccccccec3ceeeeec3ccccccc63ccccc3663cc336666336666666666666666666
;; 255:a9a9a9a036a9a9aa3669aaaa66666aaa6666666a666666666666666666666666
;; </SPRITES1>

;; <MAP>
;; 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000704132333000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 001:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006f000000000000000000000000000000000000000000000000000000040004000000000000000000000000000000000000000000000000000000000000000087078700000000001636152535000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 002:0000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000132323232323330000000000000000000000000000000000000000000000000000000096a6a6a6b6000000001727360000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 003:0000000000000000000000000000000000103000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000142424242424340000000000000000000000000000000000000000000000000000000098a8a8a8b8000000001727370000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 004:00000000000000000000000000000000001131000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001424242424243400000000000000000000000000000081810481040000000000000000000000000000000000172737000000000090a0b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 005:000000000000000000000000000000000011310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000014242424242434000000000000000000000000000090a0a0a0a0a0b00000000000005f000000000000000000172737000000000091a1b1000000000000000088000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 006:000000000000000000000000005f00000011310000000000000000000000000000000000000000000000000000008181810000000000000000000000000000000000000000000000000000000000000015252525252535000000000000000000000000000091a1a1a1a1a1b100000000000000000000000000000000182838000000000091a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 007:0000000000000000000000000000000000123200000000000000000000000000000000000000000000000000000090a0b00000000000000000000000000000000000000000000000000000000000000000430000000043000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000093a3b3000000000000000091a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 008:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b4000000000000000091a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 009:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b4000000000000000091a1b1133300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 010:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b4000000000000000092a2b2143400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 011:000000000000000000000000000000000000000000000000000000003e1e0000000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000500000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b40000000000001020202030143400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 012:000000000000000000000000008100000000000000000000000000000f1f0081000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b40000000000001121212131143400000000000000000000000000018104840787000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 013:00000000000000000000000090b000000000000000000000000090a0a0a0a0a0b00000000000000000000000000091a1b10000000000000000000000000001000041000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b40000000000001121212131143400000000000000000000000000b4b4b4b4b495000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 014:00000000000000000000000091b100000000000000000000000091a1a1a1a1a1b10000000000000000000000000091a1b10000000000000000000000102020202020202020203000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b40000000000001121212131143400000000000000000000000000b5a395a3b401000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 015:00000000410000410000000091b100000000000000000000810091a1a1a1a1a1b10000000000000000000000000091a1b10000000000000000000000112121212121212121213100000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b40000000000001222222232143400000000000000000000000000b5a395a3b434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:90a0a0a0a0a0a0a0a0b0000091b100000000000000008090a0a091a1a1a1a1a1b10000000000000000000000000091a1b10000000000000000000000112121212121212121213100000000000000000000000000000000000000000000000000000000000092a2a2a2a2a2b20000000000000000000000000095a5b50000000000000000000000143400000000000000000000000000b5a395a3b434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 017:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001b5b5b5b5b4340000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002121212121212121212121310a0a0a0a0a14242424242424242424340d0c
;; 018:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002f000000000000000038b5959595b4340000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002121212121212121212121310a19190a0a14242424242424242424340919
;; 019:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000212121210a21212122222232290d290d0a1424240a242424242424340d0c
;; 020:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000212121212121213190a0a0a0b00a090a0c14242424242425252525350a0d
;; 021:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000212121212121213191a1a1a1b10a0d0a191424242424340c0a0a0d0a0d0a
;; 022:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000212121212121213191a10aa1b119190a0a1424242424340d290a190a090a
;; 023:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000212121212121213191a1a1a1b10d09290a1424242424340993a4a4a4b30a
;; 024:0000000000000000000000000000000000005f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000222222222222223291a1a1a1b10a090d0a1525252525350a94a4a4a4b409
;; 025:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000096a6a6a6a6a6b60d91a1a1a1b1190e1e190d0a090d0a190994a4a4a4b40d
;; 026:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000097a7a70aa7a7b70a92a2a2a2b20a0f1f0a0c0d290c0a0a29940aa4a4b419
;; 027:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006e1e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000097a7a7a7a7a7b7290a090a290c5b6b6b5c7c0c090a29290a94a4a4a4b40c
;; 028:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f1f00000000000000000000000000000000000000000000000000000000000000000000000000000088000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000098a8a8a8a7a7b7290c290a190a5c5c6c7c0a0a0d0a190d0a94a4a4a4b4b3
;; 029:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000162626262626263600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0a290d97a7b70a090a0a0c19090a0a0c0a290a0a0c0a0a94a4a4a4a4a4
;; 030:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000172727272727273700000000000000000000000000000000000000000000000000000000000000000000000027270000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c0a292998a8b71626262626262626262626262636090a93a4a4a4a4a4a4
;; 031:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000172727272727273700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a090d0d0a090a1727272727270a272727272727370d0d95a4a4a4a4a4b5
;; 032:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000017272727272727370000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000090a29290a0c0a1727272727272727272727272737090a1995a5a5a5b50c
;; 033:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000172727272727273700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0a29290a190a18282828282828282828282828380a290d0a190a0a0c0c
;; 034:000000000000000000000000000000000013232323233300000000000000000000000000000000000000132424242433000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 035:000000000000000000000000000000000014242424243400000000000000000000000000000000000000242424242424000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000707000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 036:000000000000000000000000000000000015252525253500000000000000000000000000000000000000152525252535000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008500000000000000000000000000008500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001636000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 037:000000000000000000000000000000000000430000430000000000000000000000000000000000000000c3c6c6c3c6c3000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001737000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 038:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001737000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 039:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008787878787878787878787878787878787878787878787878787000000000000000000000000878787878787000000000000000087878787878787878787878787878787878787000000001737000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 040:000000000000000000000000000000000000000000003f00000000000000000000840000840000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000500000000000000001626262626262626262626262626262626262626262626262626263600000000000000000000162626262626000000000000001626262626262626262626262626262626262626262626262637000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 041:00000000000000000000000000000000000000000000000000000000000000000016262636000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000172626262626262626a7a7a726262626262626262626262626262626360000000000000016262626a7a7a726260000000000002626262626262626262626262626262626262626262626262637000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 042:000000000000000000000000000000000000000000000000000000000000000000171717370000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001726a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7262626360000000016262626a7a7a7a7a726262636000000002626262626262626262626262626262626262626262626262637000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 043:0000000000007e1e000000000000000000000000000000000000000000000000001727273700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000017a7a72626a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a726370000000017a7a7a7a7a7a7a7a7a7a72626000000162626262626262626430000432626262626262626262626262637000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 044:0000000000000f1f00c484000000000000000000000000000000000000000000001727273700000000000000000000000000000000000000000000000000000000000087008700870000000000000000000000000000000000000000840084008400000000000017a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a726a7a7a7a7a726370000000017a7a7a7a7a7a7a7a7a7a72626000000262626262626262626000000002626262626262626262626262637000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 045:000000000096a6a6a6a6b6000000000000000000000000000000000000000000001717173800000000000000000000000000000000000000000000000000000000000093a3a3a3a3b30000000000000000000000000000000000000096979797b60000000000001726a7a7a7a7a7a7a7a726a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7370000000017a7a7a7a7a7a7a7a7a7a72638000000262626262600000000000005000000262626262626262626262637000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 046:000000000097a7a7a7a7b7000000000000000000000000000000000000000000001717174600000000000000000000000000000000000000000000000000000000000094a4a4a4a4b40000000000000000000000000000000000969797979797b700000000000017262626a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a737000000001726a7a7a7a7a7a7a7a7a72600000000262626262626262600000000000000002626262626262626262637000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 047:000000000097a7a7a7a7b7000000000000000000000000000000000000000000001817170000000000000000000000000000000000000000000000000000000000000094a4a4a4a4b40000000000000000000000000000000000979797979797b70000000000001828282828282828282828282828282828282828282828282828282828380000000018282828282828282828282800000000182828282828282800000000000028282828282828282828282837000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 048:000000000097a7a7a7a7b7000000000000000000000000000000000000000000000017170000000000000000000000000000000000000000000000000000000000000095a5a5a5a5b40000000000000000000000000000000000979797979797b70000000000000000000000004600004000460000000000000000460040000000000000000000000000000000000043000000000000000000000000000000004600000000000000460000000000000000000038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 049:000000000097a7a7a7a7b7000000000000000000000000000000000000000000000017170000000000000000000000000000000000000000000000000000000000000000c30000c300000000000000000000000000000000000098a8a8a8a8a8b80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 050:000000000097a7a7a7a7b700000000000000000000000000000000000000000000001717000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c60000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 051:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000173700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 052:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000173700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 053:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000173700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 054:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000173700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 055:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008800000000000000000000000000000000000000000044440000173700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 056:000000000000000000000000006e1e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013330000173700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 057:000000000000000000000000000f1f000000000000000000000000000000000000000000000000000000000000000000000000000000070084000700000000000000000000000014340000173700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 058:0000000000000000000000001616161616a7000000000000000000000000000000000000000000000000000000000000000000000097979797979797000000000000000000000014340000173700000000000085000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 059:005f00000000000000000000a716161616a7000000000000000000000000000000000000000000000000000000000000000000000097464646469797000000000000000000000014340000173700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 060:0000000000000000000000a716161616a7a7000000000000000000000000000000000000000000000000000000000000000000000097272727279797000000000000000000000014340000173700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 061:00000000000000000000a7a71616161616a7000000000000000000000000000000000000000000000000000000000000000000000097979797979797000000000000000000000014340000173700000000162626262626262626000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 062:00000000000000000000c627272727272727000000000000000000000000000000000000000000000000000000000000000000000097974646469797000000000000000000000014340000173700000000172727272727272727000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 063:000000000000000000b4b427271717272727000000000000000000000000000000000000000000000000000000000000000000000097971616169797000000000000000000000014340000183800000000172727272727272727000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 064:000000000000000000c6c6c6c6c6c6c6c6c6000000000000000000000000000000000000000000000000000000000000000000000097971616979797000000000000000000000014340000c6c600000000172727272727272727000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 065:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000097971616979797000000000000000000000014340000000000000000172727272727272727000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 066:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000097979797979797000000000000000000000014340000000000000000172727272727272727000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 067:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000097979797979797000000000000000000000014340000000000000000172727272727272727000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 068:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000202020202020202121212121212120202000000000000096a6a6b6000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 069:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000112121212121212121212121212121213100000000000097a7a7b700000000000000000000000000000000000000000000000000000000004f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 070:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000112121212121212121212121212121213100000000000097a7a7b7000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 071:00000000000000000000000000000000000000000000c700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000112121212121212121212121212121213100000000000097a7a7b7000000000000000000000010202020202020202020202030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 072:0100810100000000000000000000000000000096a6a6a6b6000000000000000000000000008181048104000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000122222222222222222222222222222223200000000000097a7a7b700000000000000000000001121212121212121212121213100000000000000000000000000c404c400c4000004000404000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 073:1020202020203000000000000000000000000097a7a7a7b700000000000000000000000090a0a0a0a0a0b00000000000000000000000003f00000000000000000000000000000000000000000000440400040000000000000000000040000040000000000000400000400000000000000097a7a7b700000000000000000000001121212121212121212121213100000000000000000000000093a3a3a3a3a3a3a3a3a3a3a3a3b300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 074:1121212121213100000000000000000000000097a7a7a7b700000000000000000000000091a1a1a1a1a1b10000000000000000000000000000000000000000000000000000000000000000000013232323233300000000000000000000000000000000000000000000000000000000000097a7a7b700000000000000000000001121212121212121212121213100000000000000000000000094a4a4a4a4a4a4a4a4a4a4a4a4b400008500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 075:1121212121213100000000000000000000000098a8a8a8b800000000000000000000000091a1a1a1a1a1b10000000000000000000000000000000000000000000000000000000000000000000014242424243400000000000000000000000000000000000000000000000000000000000097a7a7b700000000000000000000001121212121212121212121213101000000000000000000000094a4a4a4a4a424a4a4a4a4a4a4b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 076:11212121212131000004470000040000000000460000460000000000000000000000000091a1a1a1a1a1b10000000000000000000000000000000000000000000000000000002e1e000000000014242424243400000000000000000000000000000000818400810081000000000000000097a7a7b700000000000000000000001121212121212121212121213132000000000000000000000094a4a4a4a4a4a4a4a4a424a4a4b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 077:12222222222232000013232323330000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000000000000000000000000000000000f1f00000000001424242424340000000000000000000000000000000090a0a0a0a0b0000082000000000098a8a8b800000000000000000000001122222222222232404012223100000000000000000000000094a424a4a4a4a4a4a4a4a424a4b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 078:00000000000040000014242424340000000000000000000000000000000000000000000091a1a1a1a1a1b1000000000000000000000000000000000000000000000000001020202020300000001525252525350000000000000000000000000000000091a1a1a1a1b10000000000000000c60000c600000000000000000000001100004000000000000000003100000000000000000000000094a4a4a4a4a4a4a4a5a5a5a4a4b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 079:00008500000000000014242424340000000000000000000000000000000000000000000091a1a1a1a1a1b1000000000000000000000000000000000000000000000000001121212121310000000000000000000000000000000000000000000000000091a1a1a1a1b100000000000000000000000000000000000000000000001100000000000000000000003100000000000000000000000095a5a5a5a5a5a5a5b5c3c395a5b500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 080:00000000000000000014242424340000000000000000000000000000000000000000000091a1a1a1a1a1b1000000000000000000000000000000000000000000000000001121212121310000000000000000000000000000000000000000000000000091a1a1a1a1b1000000000000000000000000000000000000000000000011000000000000000000000031000000000000000000000000000000000000000000c30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 081:00000000000000000014242424340000000000000000000000000000000000000000000091a1a1a1a1a1b1000000000000000000000000000000000000000000000000001121212121310000000000000000000000000000000000000000000000000091a1a1a1a1b1000000000000000000000000000000000000000000000011000000000000880000000031000000000000000000000000000000000000000000c30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 082:00000000000000000014242424340000000000000000000000000000000000000000000091a1a1a1a1a1b1000000000000000000000000000000000000000000000000001121212121310000000000000000000000000000000000000000000000000091a1a1a1a1b1000000000000000000000000000000000000000000000011000000000000000000000031000000000000000000000000000000000000000000c30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 083:00000000000000000014242424340000000000000000000000000000000000000000000091a1a1a1a1a1b1000000000000000000000000000000000000000000000000001222222222320000000000000000000000000000000000000000000000000091a1a1a1a1b1000000000000000000000000000000000000000000000011000000000000000000000031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 084:00000000000000000014242424340000000000000000000000000000000000000000000092a2a2a2a2a2b2000000000000000000000000000000000000000000000000004000004040000000000000000000000000000000000000000000000000000091a1a1a1a1b1000000000000000000000000000000000000000000000011272727272727272727272731000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 085:001120202020310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 086:001121212121310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 087:001222222222320000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 088:000040004040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 089:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000810000000081000081810000000000000000820000000000000000000000000000000000000000000000000000000000000000000000
;; 090:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000090a0a0a0a0a0a0a0a0a0a0b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 091:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000132323232323330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000162626262626262626262626262626360000000000000000000000000000000000000091a1a0a0a1a124a1a1a1a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 092:0000000000000000050000000000000000000000000000000000000000000000000000000000000000000000000000000005000000000000000014a4a4a4a4a4340000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000182828282828282828282828282828380000000000000000000000000000000000000091a124a1a1a1a1a1a1a1a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 093:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000014a4a4a4a4a434c4840084000000000000000000000000000000000000000000000000008700c400c400c40000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000046000000460000000000000000000000000000000000000000000000000091a1a1a1a1a124a1a1a1a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 094:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000014a4a4a4a4a4a4232323233300000000000000000000000000000000000000000096a6a6a6a6a6a6a6a6b6000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002424a1a124a1a1a1a1a1a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 095:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013a4a4a424a4a4a4a4a4a4a43400000000000000000000000000000000000000000097a7a7a7a7a7a7a7a7b70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1a1a124a1a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 096:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008400c414a4a4a4a4a4a4a4a4a4a4a43400000000000000000000000000000000000000000097a7a4a4a4a4a4a7a7b78487008400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1a1a1a124a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 097:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013232323a4a4a4a4a4a4a4a4a4a4a4a43400000000000000000000000000000000000000000097a7a4a4a4a4a4a4a7a7a6a6a6a6b6000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000092a2a2a2a2a2a2a2a2a2a2b2000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 098:000000000000000000000000000000000000000000c1008100c100008100000000000000000000000000000000000000000000000014a4a4a4a4a4a4a4a4a4a4a424a4a4a43400000000000000000000000000000000000000000097a7a4a4a4a4a4a4a4a7a7a7a7a7b70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c000000000c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 099:000000000000000000000000000000000000000090a0a0a0a0a0a0a0a0a0a0b000000000000000000000000000000000000000000014a4a4a4a4a4a4a4a4a4a4a4a4a4a4243400000000000000000000000000000000000000000097a7a4a4a4a4a4a4a4a4a4a4a4a7b70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c000000000c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 100:000000000000000000000000000000000000000091a1a1a1a1a1a1a1a1a1a1b100000000000000000000000000000000000000000014a4a4a4a4a424a494a4b4a4a4a4a4a43400000000000000000000000000000000000000000097a7a4a4a494b4a4a4a4a4a4a4a7b70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 101:000000000000000000000000000000000000000091a1a1a1a1a1a1a1a1a1a1b100000000000000000000000000000000000000000014a4a4a4a4a4a4a494a4b4a4a4a4a4a43400000000000000000000000000000000000000000097a7a4a4a494b4a4a4a4a4a4a4a7b70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000090a0a0a0a0a0a0a0a0b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 102:000000000000000000000000000000000000000091a1a1a1a1a1a1a1a1a1a1b100000000000000000000000000000000000000000014a4a424a4a4a4a494a4b425252525253500000000000000000000000000000000000000000097a7a4a4a494b4a4a4a4a4a4a4a7b70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1a1a1a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 103:000000000000000000000000000000000000000091a1a1a1a1a1a1a1a1a1a1b100000000000000000000000000000000000000000014a4a4a4a4a4a4a494a4b40043c300c30000000000000000000000000000000000000000000097a7a4a4a494b4a7a7a7a7a7a7a7b70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000092a2a2a2a2a2a2a2a2b2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 104:000000000000000000000000000000000000000091a1a1a1a1a1a1a1a1a1a1b1000000000000000000000000000000000000000000152525252525253594a4b400000000c30000000000000000000000000000000000000000000097a7a7a7a794b4a8a8a8a8a8a8a8b80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 105:000000000000000000000000000000000000000092a2a2a2a2a2a2a2a2a2a2b2000000000000000000000000000000000000000000000000000000000094a4b400000000c30000000000000000000000000000000000000000000098a8a8a8a894b40000c300c600c3000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 106:00000000000000000000000000000000000000000000c000c00094b40000c000000000000000000000000000000000000000000000000000000000000094a4b400000000000000000000000000000000000000000000000000000000c300c30094b40000c3000000c3000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 107:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b400000000000000000000000000000000000000000000000000000000c300000094b40000c3000000c3000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004e1e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 108:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b400000000000000000000000000000000000000000000000000000000c300000094b40000c300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f1f000044000400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 109:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b4000000000000000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000232323232323232323232323330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 110:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b4000000000000000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000242424242424242424242424340000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 111:000000000000000000000000000000000000000000006f00000094b400000000000000000000000000000000000000000000000000000000000000000094a4b4000000000000000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000242424242424242424242424340000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 112:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b40000000000000000005e1e00000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000242424242424242424242424340000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 113:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b40000000000000000000f1f00000000000000000000000000000000000000000094b400000000000000000000000088000000000000000000000000000000000000000000000000000000000000000000000000000000242424242424242424242424340000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 114:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b400000000000093a3a3a3a3a3a3b300000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000242424242424242424242424340000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 115:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b400000000000094a4a4a4a4a4a4b400000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000252525252525252525252525350000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 116:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b400000000000094a4a4a4a4a4a4b400000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004300430043000043000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 117:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b400000000000094a4a4a4a4a4a4b400000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 118:000000000000000000000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000094a4b400000000000095a5a5a5a5a5a5b500000000000000000000000000000000000094b400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 120:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000400010000004400000400000000000000000000000000000000000000000000000000000000000000000000
;; 121:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000102020202020202020202020202020203000000000000000000000000000000000000000000000000000000000000000
;; 122:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000122222222222222222222222222222223200000000000000000000000000000000000000000000000000000000000000
;; 123:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000040000000000000004000000040000000000000000000000000000000000000000000000000000000000000000000
;; 126:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000400000400440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 127:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000102020202020202020202020202030000000000000002f0000000000000000000000000000000000000000000000000000000000000000000000000000
;; 128:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000012222222222222222222222222223200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 129:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000400000400000004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <WAVES>
;; 001:33311223455632221335674200023467
;; 002:0000000000000000eeeeeeeeeeeeeeee
;; 003:01234456789abcefedcba99776543210
;; 004:ffffffff00000000ffffffff00000000
;; 005:0123456789abcdef0123456789abcde0
;; 006:00000000000006789aeeeeeeeeeeeeee
;; 007:5556789aabbccddeeeeeedddccba9887
;; 008:089abbbba998887778996574b84444bb
;; </WAVES>

;; <SFX>
;; 000:621062106221623362746273626f62dd62db62cb628c625e624f62406260620062006200620062006200620062006200620062006200620062006200405000000f0f
;; 001:02360236023602350281028d028b028a024a024a024b024e024202000200020002000200020002000200020002000200020002000200020002000200b04000000000
;; 002:827482748284828382838283827282718240822d822d822d823d823d823d823d82008200820082008200820082008200820082008200820082008200c00000000000
;; 003:030603160326033603460366037a0382039303a403a403b403500330034003400350039003b003c003d003d003d00300030003000300030003000300c00000000d0d
;; 004:834a834a830a830a830a8307830783478347834783078307830a830a830a830a830a830a830a830783078307830783078307830a830a830a830a830a580000000a0a
;; 005:854a854a854a850a850a850a850785078547854785478507850a850a850a850a850a850a850a850785078507850785078507850a850a850a850a850a480000000c0c
;; 016:62003270828d323b22e262ee02e802d72238f244f252f280f2d4f2e4f2f4f23ff260f24bf24df21ef24ef2daf22df28cf24bf230f290f2e6f2e5f2f7481000000000
;; 017:63003370838d333b23e263ee03e803d72338534453520380f3d4f3e4f3f4f33ff360f34bf34df31ef34ef3daf32df38cf34bf330f390f3e6f3e5f3f7689000000000
;; 018:919391b79197918691839172915f919d919a91009100e100e100e100e100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100502000000000
;; 048:06000600160026004600460046005600660076008600860086009600a600a600b600b600b600b600b600c600d600d600e600f600f600f600f600f600400000000000
;; 049:070047008700a700c700d700d700e700e700f700f700f700f700f700f700f700f700f700f700f700f700f700f700f700f700f700f700f700f700f700400000000000
;; 050:0800580f780fa80fb80fb80ec80dc80ce80cf80cf80ef80ef800f800f800f800f800f800f800f800f800f800f800f800f800f800f800f800f800f800400000000000
;; 051:080018001800180018002800180018002800280038003800480048005800680078007800880088009800a800b800c800d800d800e800e800f800f800400000000000
;; 052:080018001800180018002800180018002800280038003800480048005800680078007800880088009800a800b800c800d800d800e800e800f800f800400000000000
;; 060:00006000b000d000e000e000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000400000000000
;; 061:00002000600080009000a000b000c000e000e000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000400000000000
;; </SFX>

;; <PATTERNS>
;; 000:6ff9050000201000000000a06008050000001000200000a06008050000001000000000a0600805100020400805000000600805000021100000000000600805000021100020000000600805000000100020000020600805000020a008050000006008050000201000200000206008050000a01000200000006008050000001000200000006008050000a04008050000a06008050000001000200000216008050000a01000200000a06008050000a01000200000a0600805000020400805000000
;; 001:000000000021000000000000900048000021100010000021000000000010000010000010800018000010100010000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 002:500016000000a00016000000700016000010800016100010b00016000000900016000010800016100010900016100010000000000000000000000000000000000000800099000000000000b00029000000900029000000000021d00099000021000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 003:b00006e00006000000700006000000700006d00006000000f00006700006800006600006500006900006000000700006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 004:000000f0000af00008f00004a0000cf00008d00004c0000ab0000cf0000660000cb0000040000460000eb00006d0000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 010:6889050000201000000000a06008050000001000200000a06008050000001000000000a0600805100020400805000000600805000021100000000000600805000021100020000000600805000000100020000020600805000020a008050000006008050000201000200000206008050000a01000200000006008050000001000200000006008050000a04008050000a06008050000001000200000216008050000a01000200000a06008050000a01000200000a0600805000020400805000000
;; 011:6ff9070000201000000000a06008070000001000200000a06008070000001000000000a0600807100020400807000000600807000021100000000000600807000021100020000000600807000000100020000020600807000020a008070000006008070000201000200000206008070000a01000200000006008070000001000200000006008070000a04008070000a06008070000001000200000216008070000a01000200000a06008070000a01000200000a0600807000020400807000000
;; 012:90080b60080b50080b00080160080b00080100000000080100080100000000080100000040080b60080bd0080b000801b0080b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000090080b60080b50080b00000060080b00000000000000000000080100080100080100000040080b60080bd0080b000000f0080b000000000000000000000801000000100801000000100801000000100801000000100801000000100801000000
;; 013:90080b60080b50080b00080160080b00080100000000080100080100000000080100000040080b60080bd0080b000801b0080b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000090080b60080b50080b00000060080b00000000000000000000080100080100080100000040080b60080bd0080b000000f0080b000000000000000000000801000000100801000000c0080b00000090080b00000090080b00000060080b000000
;; 014:7008d50000007008d50000007008d50000004008d50000001008d10000001008d1000000a008d5000000a008d50000000000000000000000000000000000000000000000000000000000000000001008d10000007008d50000007008d50000007008d50000005008d50000001008d11008d10008d10000001008d10000001008d10000007008d50000007008d50000007008d50008d15008d50000001008d10000001008d1000000a008d5000000a008d5000000000000000000000000000000
;; </PATTERNS>

;; <TRACKS>
;; 000:b00000b43000b83000b83000b43c00b83cc3b43cc3083cc3043cc30830000000000000000000000000000000000000006f0000
;; 001:300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000200
;; </TRACKS>

;; <FLAGS>
;; 000:00101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000000000000000000010101000000000000000000000000000000010000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </FLAGS>

;; <SCREEN>
;; 004:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000fee000e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 005:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000ff000ee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 006:0000000000000000000000000000000000000000000000000000000ff00000000000000000000000000000fe00fe0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 007:00000000000000000000000000000000000000000000000000000000ff0000000000000000000000000000fe00ff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 008:000000000000000000000000000000000000000000000000000000000f0ff000000000000000000000000efe000fe000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 009:0000000000000000000000000000000000000000000000000000000ff000f00000000000000000000000eef000ef0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 010:000000000000000000000000000000000000000000000000000d000f0000f00000000000000000000000efff00f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 011:000000000000000000000000000000000000000000000000000de0ffffff00000000000000000000000000eefffe0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 012:00000000000eeeeeeeee000000000000000000000fff0000000deff00ff00000000000000000000000000000eee00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 013:0000000000ededddddfeee000000000000000ff00ff00000000def00000000000000000000000000000000edddd00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 014:0000000000ede00000dddfee00000000000ff0f000fff000000def000000000000eee00000000000000000dfffdd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 015:0000000000fdef0ff000ddee00000000000000f0fffff00ddddddddddde0ddd00eddddd000d0000dd0000def0ffd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:0000000000fdefff00000ddeeddde000000000fffffffddfffdfffffddddddddeddddddd00d0000dd0000ddef0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 017:0000000000fdeff0000000deeddde00000000000000edd0000de00000ddeeeeddd0000ddd0de000dd00000ddde000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 018:0000000000ffdff00fff00def0dde0dddd000ddd000ed00000de00000dde0000ddff000ed0de000dd0000000dde00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 019:0000000000fddff0f00ff00df00000d00000deed000edde000de00000dde0000ddf0fffed0de000dde0000000dde0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 020:0000000000fddf00f000f00df0f00dd0e000df0d0000dddde0de00000dd00000d0f0f0fed0de000ddf00000000dde000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 021:0000000000fddf00fff0f0fdedf000ddde00df0de00000dddede00000dd00000d0f00ffed0de000ddf000000000dde00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 022:0000000000ffdff0000ffffdfdf00000dde0df0dee00d000dddd00000dd00000d0ffffedd0de000ddf06660f000fdee0000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 023:0000000000ffdff0000fffedfdf000000dd0df0dddeed000ddfde00d00d00000dd0eeedd00de00eddfe666d0ffffdee0000000000000011111111111000000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 024:0000000000ffdfff0ffffeedfdf00dd000deddddfddeddddfffddedd00d00ffffddddd0f00ddfffddff666eddffddeee000000000001111111111111100000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 025:0000000000ffdffffffffeddfdfffdddddee00d0ffff0ffffffffddfff22fffff3fffffffffddddddddfffeedddeee00000000000011111111111110000000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 026:0000000000ffdffffffeeddffdddfffeeee00ffffffff0ffffffff2222222ff333ffffff00ffddddffddddeeeeee00999990000000111100000f1100000001111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 027:0000000000f0deeeeeeedffffffffffffffffff0f0ffffffff2222222222fff333000000ffffffffffff66fff99009999999900000111000000f1100000001111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 028:000000000000eeeedddfffffffffffffffff0ffffffffff222222222222f00033300000000ffff4ffff666ffff9999fff999900000111000000f1100000011111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 029:000000000000dddffffffff000ffffff00000000000ffff2222222fffff00003330000440000044ffff666000fe99ff0fff9990000111000000f1110000011111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 030:000000000000dd0ff0000000000000000000000000000ff222222ff0000000033300444400000440000666600ff99f0000f999000011100000ff1100000111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 031:0000000000000000000000000000000000000000000000022222ff000000000333000044000004440000066000f9900000f99900001111000ff11111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 032:00000000000000000000000000000000000000220000000200220000000000033300004440000444000006600009900000ff99000001111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 033:000000000000000000000000000000000000002200000000002200000000000f33000044400004444000066000099000000f9900000ff11111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 034:000000000000000000000000000000000000002220000000222200000000000f3300004440000444f000066000099000000099000000fffffff11111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 035:000000000000000000000000000000000000002222200022222200000000000f3300004440000444f000066000099000000099900000000000f111111101111f1110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 036:000000000000000000000000000000000000002222222222222200000000000f33000044400e4444f000066000099000000099900000000000f11fff110f110ff111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 037:000000000000000000000000000000000000002222222222222202222222200f33000044444444440000066000099000000099900000000000f1100ff100f100ff11000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 038:000000000000000000000000000000000000002222222222f222222222fff00f3300000ff4444f440000066000099000000009900000000000f11000f100ff000f11000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 039:000000000000000000000000000000000000022222222222f2222222fff0000f33000000000000440000066000099000000000000000000011111000f0000f100011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 040:000000000000000000000000000000000000022222222222f2222ffff000000f33300000000000440000000000000000000000d000d001111111100000000f0000f1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 041:00000000000000000000000000000000000002222222222ff2222f000000000f3330000000000044000000000d000d00d00d11111111111111111000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 042:000000000000000000000000000000000000022f222222ff022220000000000f3330000000000044000000e111111111111111111111111111100000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 043:000000000000000000000000000000000000022f2222f2f0022220000000000f33303300000000440000001111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 044:000000000000000000000000000000000000022f0222f2f0022220000000000fff333300400000440000011111100000101111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 045:00000000000000000000000000000000000002ff022ff0f00222200000000000ff33330f400000440000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 046:00000000000000000000000000000000000002f0022f000000222000000000000fffff0f440000440000110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 047:00000000000000000000000000000000000002f0002f000000222000000000000000000ff444444ff000100000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 048:000000000000000000000000000000000000020000000000002200000000000000000000f44444fff0001000011111100000000000d0000000000000000000000000000000000000000000000000004400000044400000040000000000000000000000000000000000000000000000000000000000000000
;; 049:0000000000000000000000000000000000000000000000000002000000000000000000000ddddff000000100001111100d00000000d0000000000000000000000000000000000000000000000000004444a0a0444a00a444a000000000000000000000000000000000000000000000000000000000000000
;; 050:0000000000000000000000000000000000000000000000000000000000000000000000ddddddddd000000110000111100d0000000dd000000000000000000000000000000000000000000000000000444440a44444aa0444aa00000000000000000000000000000000000000000000000000000000000000
;; 051:000000000000000000000000000000000000000000000000000000000000000000000dddeeeeeee000000011001111100d0000000dd00000000000000000000000000000000000000000000000000004444aa44444aa4444aa00000000000000000000000000000000000000000000000000000000000000
;; 052:00000000000000000000000000000000000000000000000000000000000000000000ddde000000000000000111110110dd000000edd0dde00dddddd0000000000000e0000000000000000000000000a44244442244444424aaa0000000000000000000000000000000000000000000000000000000000000
;; 053:00000000000000000000000000000000000000000000000000000000000000000000dde0000000000000000000dd0000dde00eeedddddd00ddffffdddd00dddddd000ddddd00000000000000000000a942244422444442244aaa000000000000000000000000000000000000000000000000000000000000
;; 054:0000000000000000000000000000000000000000000000000000000000000000000dde00000000000000000000dd0000ddee00dddddd0000dff00eed0dddddd0ddd0dd00ddd0000000000000000000a944444444444444444aa9000000000000000000000000000000000000000000000000000000000000
;; 055:0000000000000000000000000000000000000000000000000000000000000000000dde00000000000000000000dd0000ddddd0dddd000000de000ddd00ddd0000dd0d0000dd0000000000000000000aa444444444444444444aa000000000000000000000000000000000000000000000000000000000000
;; 056:0000000000000000000000000000000000000000000000000000000000000000000dde00000000000000ddddd0000ddddddd0000dde00000dddddd0000ddd00000d0ddd000d0000000000000000000a94eeeeeeeeee4444444aaa00000000000000000000000000000000000000000000000000000000000
;; 057:0000000000000000000000000000000000000000000000000000000000000000000dde000000000000dedeeee0d00edddd000000dde00000de00000000ddde00000000dd0000000000000000000000aaeecccccccdeeee4444aaa00000000000000000000000000000000000000000000000000000000000
;; 058:0000000000000000000000000000000000000000000000000000000000000000000dde000000000000ddd0000dd00000dd000000dde00000ddf00000000dde000000000dd000000000000000000000a98ccccccccccccceee49aaa0000000000000000000000000000000000000000000000000000000000
;; 059:0000000000000000000000000000000000000000000000000000000000000000000dde000000000000dd00000dd00000dde00000dde00ee0ddf0000e000dde000000000dd00000000000000000000aa98cacaccccccacaccc9aaaa0000000000000000000000000000000000000000000000000000000000
;; 060:0000000000000000000000000000000000000000000000000000000000000000000dde00000000000edd00000dd00000dee00000ddeeeed0ddf00ede000dde0000000000ddd000000000000000000a998ca555cccc555accc999aa0000000000000000000000000000000000000000000000000000000000
;; 061:0000000000000000000000000000000000000000000000000000000000000000000dde000000000dd0dd00000de00000dde000e00ddeedd00dffffdd000dde00000dd0000dd00000000000000000aa99ac58ff5cc5fff5ccca99aa0000000000000000000000000000000000000000000000000000000000
;; 062:0000000000000000000000000000000000000000000000000000000000000000000ddd000000000dd0dd00000de00000dde00ee00ddddd000ddddddd000dd000000ed00eddd0000000000000000aaa9a9cc8dfcdccfdfccccc9aaa0000000000000000000000000000000000000000000000000000000000
;; 063:00000000000000000000000000000000000000000000000000000000000000000000dde00000000dd0dd00000dde0000dddeeed00dddd00000ddddd0000dd000000eddddddd000000000000000aaa9999cccccddcccccccccc99aa0000000000000000000000000000000000000000000000000000000000
;; 064:00000000000000000000000000000000000000000000000000000000000000000000dddee00000dd00dd00000dddd0000dddddd00000000000000000000dd000000eeeeee0000000000000000aaaa9a99cccccdccccccccccca9aa0000000000000000000000000000000000000000000000000000000000
;; 065:0000000000000000000000000000000000000000000000000000000000000000000000ddeee00dd000dd000000ddd0000ddddd00000000000000000000000000000000000000000000000000aa9a99a99cccccdccccccccc3c99a00000000000000000000000000000000000000000000000000000000000
;; 066:00000000000000000000000000000000000000000000000000000000000000000000000ddddddd00000000000000000000000000000000000000000000000000000000000000000000000000a99a99999cccccdddcccccdcca99a00000000000000000000000000000000000000000000000000000000000
;; 067:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000aa9aa99a99ccccccccccccccd9a99a00000000000000000000000000000000000000000000000000000000000
;; 068:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a99aa999999ccc11111ccccca99a9a00000000000000000000000000000000000000000000000000000000000
;; 069:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a9aa9999a99cccc111cccccc99a99a00000000000000000000000000000000000000000000000000000000000
;; 070:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a9aa99a9999accccccccccca9999aa00000000000000000000000000000000000000000000000000000000000
;; 071:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a9aaa9999999ccccccccccca99999a00000000000000000000000000000000000000000000000000000000000
;; 072:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a9a999999a999accccccceca9a9a9a00000000000000000000000000000000000000000000000000000000000
;; 073:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009a9aa9999999663ceeeeec36a9a9aa0000000000000000000000000000000000000000000000000000000000
;; 074:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009a99999a9966663ccccccc3669aaaa0000000000000000000000000000000000000000000000000000000000
;; 075:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000099a9a99a66666663ccccc366666aaa0000000000000000000000000000000000000000000000000000000000
;; 076:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a99996666666663cc3366666666a0000000000000000000000000000000000000000000000000000000000
;; 077:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a99a6666666666633666666666660000000000000000000000000000000000000000000000000000000000
;; 078:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a666666666666666666666666660000000000000000000000000000000000000000000000000000000000
;; 079:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006666666666666666666666666660000000000000000000000000000000000000000000000000000000000
;; 080:00000000000000000000000000000000000000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000000000000000000000000000000000000000000000
;; 081:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 082:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 083:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 084:00000000000000000000000000000000000000000000000000cfffdffffddddffddffffffffffffffffffffddddffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 085:00000000000000000000000000000000000000000000000000cfffddfffddffdfddffffddddfdffddfffffddfffffddddfddfdfffdddfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 086:00000000000000000000000000000000000000000000000000cfffdddffddffdfddfffdffddfdffddfffffddfddfdffddfdddddfddfddffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 087:00000000000000000000000000000000000000000000000000cfffdddffddddffddfffdffddffddddfffffddffdfdffddfdfdfdfdddffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 088:00000000000000000000000000000000000000000000000000cfffddfffddfffffdddffddddffffddffffffddddffddddfdfdfdffdddfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 089:00000000000000000000000000000000000000000000000000cfffdffffffffffffffffffffffdddfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 090:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 091:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 092:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 093:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 094:00000000000000000000000000000000000000000000000000cffffffffddddffddfffffffffffffffffffddddfffffffffffffffffddddffddffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 095:00000000000000000000000000000000000000000000000000cffffffffddffdfddffffddddfdffddffffffddffdfffdffdddffffffddffdfddffffddddfdffddffdddffddddfffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 096:00000000000000000000000000000000000000000000000000cffffffffddffdfddfffdffddfdffddffffffddffdfdfdfddffdfffffddffdfddfffdffddfdffddfddfddfddffdffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 097:00000000000000000000000000000000000000000000000000cffffffffddddffddfffdffddffddddffffffddffdddddfddffdfffffddddffddfffdffddffddddfdddfffddfffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 098:00000000000000000000000000000000000000000000000000cffffffffddfffffdddffddddffffddffffffddffddfddffdddffffffddfffffdddffddddffffddffdddffddfffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 099:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffdddfffffffffffffffffffffffffffffffffffffffffffffdddfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 100:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 101:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 102:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 103:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 104:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 105:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 106:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 107:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 108:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 109:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 110:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 111:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 112:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 113:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 114:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 115:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 116:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 117:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 118:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 119:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 120:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 121:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 122:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 123:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 124:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 125:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 126:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 127:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 128:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 129:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 130:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 131:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 132:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 133:00000000000000000000000000000000000000000000000000cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc00000000000000000000000000000000000000000000000000
;; 134:00000000000000000000000000000000000000000000000000cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000000000000000000000000000000000000000000000
;; </SCREEN>

;; <PALETTE>
;; 000:1a1c2c710c95b13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

;; <PALETTE1>
;; 000:1a1c2c710c95b13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE1>

