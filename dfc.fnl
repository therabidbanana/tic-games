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

(fn draw-sprite! [{: sprite : w : h : scale : trans : x : y
                   : animate : ticks
                   : flip : rotate
                   : anchor-x : anchor-y : shift-x : shift-y}]
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
    (spr sprite x y (or trans -1) scale (or flip 0) (or rotate 0) w h)))

(fn draw-box! [{: w : h : x : y : bg-color : border-color}]
  (if bg-color (rect x y w h bg-color))
  (if border-color (rectb x y w h border-color)))

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
   (fn [self {: box : text : tag : character}]
     (let [box (box-styles (or box {}))
           lines (text-box-lines {: text :w box.inner-w})
           pages (pages-for-lines {:max-h (max box.inner-h 12) : lines})
           page 1]
       ($ui:push (merge self {: character : tag : box : pages : page :ticks 0}))))
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
         ($ui:pop)))})

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
   (mapv #($:render (merge (or $.state {}) screen-state)) self.entities)))

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
         :select! (fn [self name] (let [screen (?. self.screens name)]
                                    (tset self :active screen)
                                    (screen:prepare)))
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
(var player-x 96)
(var player-y 24)
(var player-sprite 256)

(fn ui-testbed []
  (var map-details
       {:map {}
        :sprites [{:h 2 :w 2 :sprite 34 :trans 14 :x 34 :y 100 :action #(set player-sprite 34)}
                  {:h 2 :w 2 :sprite 36 :trans 14 :x 134 :y 20 :action #(set player-sprite 36)}
                  {:h 2 :w 2 :sprite 66 :trans 14 :x 160 :y 120 :action #(set player-sprite 66)}
                  {:h 2 :w 2 :sprite 1 :trans 14 :x 12 :y 62 :action #(set player-sprite 1)}]})
  ($ui:menu! {:box {:x 140}
              :tag :test
              :options [{:label "Say Foobar?" :keep-open? true :action #($ui:textbox! {:box {:x 140} :text "Foobar!"})}
                        {:label "Open Map" :action #(do ($ui:clear-all! :test) ($ui:sprite-selector! map-details))}
                        {:label "Clear UI" :action #($ui:clear-all! :test)}
                        {:label "Cancel"}]})
  )

(fn draw-entity [{ : character &as ent} state _entities]
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


(var next-color {:red :orange :orange :yellow :yellow :green :green :blue :blue :purple :purple :red})
(var prev-color {:red :purple :orange :red :yellow :orange :green :yellow :blue :green :purple :blue})

(fn player-collides? [tile]
  (and tile.solid? (or tile.would-paint? (= tile.color :grey))))

(fn player-react [self {: x : y : dx : dy : color : dir : hp : invuln &as state} {: entities &as game}]
  (let [max-speed 1.5
        drag 0.05
        add-speed 0.5
        gravity 0.05
        dx (or dx 0)
        dy (or dy 0)
        dx (if (btn 2) (max (- dx add-speed) (* -1 max-speed))
               (btn 3) (min (+ dx add-speed) max-speed)
               (< dx 0) (min (+ dx drag) 0)
               (max (- dx drag) 0)
               )
        dy (if (btn 0) (min -1 dy)
               (btn 1) (min (+ dy add-speed) max-speed)
               (< dy -2) (+ dy (* 4 gravity))
               (< dy 0.15) (+ dy gravity)
               (min (+ dy gravity) 0.15))
        x (+ x dx)
        y (+ y dy)
        dir (if (< dx -0.1) -1 (> dx 0.1) 1 (or dir 1))
        left? (= dir -1)
        x (clamp x 0 (- (* 8 240) 8)) ;; Limit to edges
        y (clamp y -8 122)
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
        color (if (btnp 6) (?. prev-color color)
                  (btnp 5) (?. next-color color)
                  damaged? intersected.color
                  color)
        new-invuln (if damaged? 200 (max (- (or invuln 1) 1) 0))
        dy (if (and bounced? (btn 1)) -4 bounced? -2.5 dy)
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
    (if (btnp 4)
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

(defscreen $screen :title
  {:tick
   (fn []
     (cls 8)
     )
   :draw
   (fn []
     (print "Disastrous Flying" 22 22 15 false 2)
     (print "Critters" 68 52 15 false 2)
     (print "Disastrous Flying" 23 23 13 false 2)
     (print "Critters" 69 53 13 false 2))
   :prepare
   (fn []
     (poke 0x03FF8 8)
     ($ui:clear-all!)
     ($ui:menu! {:box {:x 50 :w 140}
                 :options [{:label "Play Game" :action #($screen:select! :game)}
                           {:label "UI Test" :keep-open? true :action #(ui-testbed)}]}))})

(var sprite-colors {:red 256 :orange 264 :blue 320 :green 296 :purple 328 :yellow 288})
(var color-cycle [:red :orange :yellow :green :blue :purple])

(var player
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
      :state {:x player-x :y player-y :color :yellow}
      :tag :player
      :character
      {;; Test weird blink patterns
       :animate {:period 200 :steps [{:t 0 :index 1} {:t 100 :index 2} {:t 112 :index 1}
                                     {:t 115 :index 3} {:t 130 :index 1}]}
       :trans 0
       :x player-x :y player-y :w 2 :h 2}})

(fn enemy-react [{: color &as self} {: hp : x : y : dx : dy : ticks &as state} {&as game}]
  (let [x (if dx (+ x dx) x)
        y (clamp (if dy (+ y dy) y) 1 120)
        dx (if (or (< x 1) (> x (* 239 8))) (- 0 dx) dx)
        dy (if (or (< y 1) (> y 130))
               (- 0 (or dy 1))
               (math.sin (/ ticks 40)))
        {: would-paint? } (game:fetch-map-tile {:x (+ x 7) :y (+ y 7) : color})]
    (if (<= hp 0)
        :die
        (do
          (if would-paint? (game:paint-tile! {:x (+ x 7) :y (+ y 7) : color}))
          (merge state {: x : y : dx : dy})))))

(var enemy-sprite-colors {:red 384 :orange 392 :yellow 416 :green 424 :blue 448 :purple 456})
(fn build-enemy [{: color &as base-state}]
  (let [color (or color :red)
        sprite (?. enemy-sprite-colors color)]
    {:render draw-entity
     :react enemy-react
     :tag :enemy
     :critter true
     :color color
     :collision-box (fn [{: state}] {:x state.x :y state.y :w 16 :h 16})
     :state (merge {: color} (or base-state {}))
     :take-damage! (fn [self bullet]
                     (tset self.state :hp (- (or self.state.hp 1) 1)))
     :character
     {:sprite sprite :trans 0 :w 2 :h 2}}))

(fn portal-react [{: color &as self}
                  {: hp : cycle : x : y : dx : dy :  ticks &as state}
                  {: entities &as game}]
  (let [x (if dx (+ x dx) x)
        y (clamp (if dy (+ y dy) y) 1 125)
        dx (if (or (< x 1) (> x (* 239 8))) (- 0 dx) dx)
        dy (math.sin (/ ticks 40))
        cycle (or cycle (+ 197 (// (* (math.random) 90) 1)))
        {: would-paint? } (game:fetch-map-tile {:x (+ x 7) :y (+ y 7) : color})
        player (->> (filterv #(= :player $.tag) entities)
                    first)]
    (if (<= hp 0)
        :die
        (do
          (if would-paint? (game:paint-tile! {:x (+ x 7) :y (+ y 7) : color}))
          (if (and (= (% ticks cycle) 0) (< (math.abs (- player.state.x x)) 240))
              (game:add-entity! (build-enemy {:color color
                                              :dx (* -1 (math.random))
                                              :x (+ x (- 10 (* 10 (math.random))))
                                              :y (+ y (- 10 (* 10 (math.random))))
                                              :hp 1})))
          (merge state {: x : y : dx : dy : cycle})))))

(var enemy-portal-colors {:red 32 :orange 40 :yellow 80 :green 88 :blue 128 :purple 136 :white 176})
(fn build-portal [{: color : hp &as base-state}]
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
                 {: hp : cycle : x : y : dx : dy :  ticks &as state}
                 {: entities &as game}]
              (let [dy (* 0.15 (math.sin (/ ticks 40)))
                    player-ent (->> (filterv #(= :player $.tag) entities) first)]
                (if (touches? (self:collision-box) (player-ent:collision-box))
                    ($screen:select! :title)
                    :else
                    (merge state {:y (+ y dy) :dy dy}))))
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
    (print (.. "Grey tiles: " current.grey) 10 20 13)
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

(fn draw-hud [{: entities &as game} { : color-bar}]
  (let [portals (filterv #(?. $ :portal) entities)
        critters (filterv #(?. $ :critter) entities)]
    (print (.. "Portals: " (count portals)) 10 10 13)
    (print (.. "Critters: " (count critters)) 100 10 13))
  (draw-hud-colorbar color-bar))

(fn draw-sky! [{: ticks : screen-x}]
  )

(fn draw-stats [first-player]
  (print (.. "HP:" (or first-player.state.hp 3)) 10 120 13)
  )

(fn spawn-players! [{: entities &as self} during-game]
  (let [player-ent (->> (filterv #(= :player $.tag) self.entities) first)]
    (if player-ent
        :noop
        (do
          ;; TODO - fix for other color maps
          (for [x 0 239]
            (for [y 0 16]
              (let [tile (mget x y)]
                (if
                 (= 240 tile)
                 (let [map-x (- (* x 8) 120)]
                   (tset self.state :screen-x map-x)
                   (self:add-entity! (merge player {:state {:invuln (if during-game 200 0) :x (* x 8) :y (* y 8) :color :orange}})))
                 (> (* 100 (math.random)) 95)
                 (let []
                   (if during-game (self:paint-tile! {:color :grey :x (* x 8) :y (* y 8)}))
                   )))))))))

(fn spawn-home-portal! [{: entities &as self} during-game]
  (let [home-ent    (->> (filterv #(= :home $.tag) self.entities) first)
        ;; TMP: testing home
        enemy-count (- (->> (filterv #(= :enemy $.tag) self.entities) count) 100)]
    (if (or home-ent (> enemy-count 0))
        :noop
        (do
          ;; TODO - fix for other color maps
          (for [x 0 239]
            (for [y 0 16]
              (let [tile (mget x y)]
                (if
                 (= 240 tile)
                 (let [shifted-y (- y 6)]
                   (self:add-entity! (build-home-portal {:x (* x 8) :y (* shifted-y 8)})))
                 ))))))))

(defscreen $screen :game
  {:state {}
   :tick
   (fn [self {: ticks : color-bar : screen-x : screen-y &as screen-state}]
     ;; (if (btnp 7) ($screen:select! :pause))
     (react-entities! self screen-state)
     (spawn-players! self true)
     (spawn-home-portal! self true)
     ;; (draw-sky! screen-state)
     (let [player-ent (->> (filterv #(= :player $.tag) self.entities) first)
           player-offset (- player-ent.state.x screen-x)
           diff (- (clamp player-offset 80 160) player-offset)
           shifted-x (- screen-state.screen-x diff)
           new-screen-x (clamp shifted-x 0 (- (* 8 240) 240))]
       (if (= (% ticks 60) 0)
           (self:recalculate-color-bar!))
       (if (empty? self.entities)
           ($screen:select! :title))
       ;; (icollect [_ v (ipairs self.entities)]
       ;;   (if (and (= :enemy v.tag) (> screen-state.ticks 60))
       ;;       (v:take-damage! {})))
       ;; (print (.. "Count " (count self.entities)) 20 20 13 )
       (cls 8) ;; Allow pretty sky
       {:ticks (+ screen-state.ticks 1) :screen-x new-screen-x : color-bar : screen-y}))
   :draw
   (fn [self {: screen-x : screen-y : color-bar &as screen-state}]
     (let [player-ent (->> (filterv #(= :player $.tag) self.entities) first)]
       (draw-stats player-ent)
       (draw-sky! screen-state)
       (draw-map! {:x 0 :w 240 :sx (- 0 screen-x) :trans 0
                   :on-draw (fn [tile x y]
                              (if (between? tile 242 247)
                                  (do (self:add-entity!
                                       (build-portal {
                                                      :color (?. color-cycle (- tile 241))
                                                      :dx -0.5 :x (* x 8) :y (* y 8) :hp 10}))
                                      (mset x y 0)
                                      0)
                                  tile)
                              )
                   })
       (draw-entities! self screen-state)
       (draw-hud self screen-state))
     )
   :entities []
   :add-entity! (fn [self ent] (into self.entities [ent]))
   :fetch-map-tile (fn fetch-map-tile [{: state &as self} {: x : y : color}]
                     (let [tile-x (// x 8)
                           tile-y (// y 8)
                           tile (mget tile-x tile-y)
                           colorable? (between? tile 1 144)
                           tile-color (tile-color tile)
                           solid? (fget tile 0)
                           would-paint? (and
                                         (not= tile-color :grey)
                                         (not= tile-color :none) (not= color tile-color))]
                       {:x (* 8 tile-x)
                        :y (* 8 tile-y)
                        : tile-x : tile-y : solid? : tile : colorable? :color tile-color : would-paint?}))
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
   (fn [{: state &as self}]
     (let [{: map-y} state
           color-bar {:grey 0}]
       (for [x 0 239]
         (for [y 0 16]
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
     (tset self :state {:ticks 0 :screen-x 0 :color-bar {} :screen-y 0})
     ($ui:clear-all!)
     (self:recalculate-color-bar!)
     (spawn-players! self false)
     ;; (self:add-entity! (build-enemy {:dx -0.5 :x 200 :y 40 :hp 1}))
     ;; (self:add-entity! (build-enemy {:dx -0.5 :dy 1 :x 240 :y 100 :hp 1}))
     )})

(fn _G.BDR [line]
  (let [scans 144
        PALETTE_ADDR 0x03FC0
        CHANGE_COL 8
        color (* (// (* 0xff (/ line scans)) 32) 32)]
    (poke (+ (* CHANGE_COL 3) PALETTE_ADDR) color)))

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
;; 009:0222222222333333233333332333333323333333233323332333333323333333
;; 010:2222222233333333333333333333333333333333333333333333333333333333
;; 011:2222222033333322333333323333333233333332333233323333333233333332
;; 016:0000000000000000002222000024220000222200002222000024222000242220
;; 017:1222222212223222122232221232222212322222122222221222322212223222
;; 018:2223222222223332222232223232223323322232232223322222222222232222
;; 019:2222222122232221222322212222232122222321222222212223222122232221
;; 024:0000000000000000003333000033330003333330033333303334433333344333
;; 025:2333333323333333233333332333333323333333233333332333333323333333
;; 026:3333333332333333333333333332233333223333332333333333323333333333
;; 027:3333333233333332333333323333333233333332333333323333333233333332
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
;; 057:0777777777555555766666667665666676666566765667767666666776667666
;; 058:7777777755555555666666666665666666666665656666666766656676666666
;; 059:7777777055555577666666676665666766666667657766676666756766666767
;; 064:0000000000000000003333300034443003344433004444400044244000442440
;; 065:1444444414443444144444441444444414444444144444441444444414444444
;; 066:4444444444444444444444443444444344444444434443444444444444434444
;; 067:4444444144444441444344414444444144444341444444414443444144444441
;; 072:006666000665566006655660066ff66006666660066ff6600667766006677660
;; 073:7666766676666776766666677666666676666666766666667666677676666667
;; 074:6766666666766666676667766676775666676666667666667766666667666666
;; 075:6666766766666777766676676757666766776667666676676666676767666667
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
;; 105:0888888888221111811111118111111181111112811111118112211181111111
;; 106:8888888811111111111111221111111121111111111111111111111111111111
;; 107:8888888011111188111111181122111811111118111111181111122811111118
;; 112:0000000000000000008888800089998008899988009999900099b9900099b990
;; 113:8a9999998a9999998a9999998a9999998a9999998a9999998a9999998a999999
;; 114:9999999999999b99999999999999999999aa999999ba99999999999999999999
;; 115:999999a8999999a899a999a899b999a8999999a8999999a8999999a8999999a8
;; 120:0000000000000000001111000013110000111100000110000003100000031000
;; 121:8111111181111111811111118111111181111222811111118111111181111111
;; 122:1111111111111221111111111111111121111111111111111111111111111111
;; 123:1111111811111118111111182111111811111118111111181111122811111118
;; 128:0099990009000000900999009090009090909090900990909000009009999900
;; 129:8a9999998a9999998a9999998a999ab98a999aa98aa9999988aaaaaa08888888
;; 130:999999999999999999999999999999999999999999999999aaaaaaaa88888888
;; 131:999999a8999999a8999999a8999999a8999999a89999aaa8aaaaaa8888888880
;; 136:0011110001000000100111001010001010101010100110101000001001111100
;; 137:8111111181221111811111118111112281111111811111118811111108888888
;; 138:1111111111111111111111112111111111111111111112211111111188888888
;; 139:1111111822211118111111181111111811122118111111181111118888888880
;; 153:0fffffffffddeeeefeefeeeefeeefeeffeeeeffffeeeeeeffeeddefefeeeefee
;; 154:ffffffffeeeeeeeeeeeeeeddfffffeeedeeeefffeeeffeeeeeefeeeeeeeeffff
;; 155:fffffff0eeeeeeffeeeeeeefeeddeeefeffeeeefeeffeeefeefeeddfffeeeeef
;; 168:000000000000dd00000eedd00000eed000eeee000feeeff00ffdeff0ddfdeff0
;; 169:feeefeeefeeeffeefeeeeeeefeeeeeeefeeeedddfeeeeeeefeeeefeefeeffeff
;; 170:feeeeeefefeeeddfeefffefeefeefefefeeeefeefeeeeefefeeeeeeffeeeeeef
;; 171:feeeeeefefeeeeefeeffeeefdeeffeefeeeeeeefeeeeeeefeeeeeddfeeeeeeef
;; 176:00cccc000c000000c00ccc00c0c000c0c0c0c0c0c00cc0c0c00000c00ccccc00
;; 185:feeeeeeffeddeeeffeeeeeeefeeeeeddfeeeeeeefeeeeeeeffeeeeee0fffffff
;; 186:eeeeeeeeeffeeefefeffefeedeeeffeeeeeeefeeeeeeefffeeeeeeeeffffffff
;; 187:eeeeeeefdddeeeefeeefeeefeeffeeefefffdeeffeeffeefeeeeeefffffffff0
;; 202:feeeeeefefeeeddfeefffefeefeefefefeeeefeefeeeeefefeeeeeeffeeeeeef
;; 217:7666766676666776766666677666666676666666766666667666677676666667
;; 218:feeeeeefefeeeddfeefffefeefeefefefeeeefeefeeeeefefeeeeeeffeeeeeef
;; 224:00cccccc0c000123c0000964c0000000c00000b0c00000bbc00000bbcbbb000b
;; 225:cccccc00100dddc0100000dc100000dcb0b0000cbbb0000cbbc0000cbb0c0bbc
;; 226:00cccccc0c000222c0000222c0000000c00000b0c00000bbc00000bbcbbb000b
;; 227:00cccccc0c000333c0000333c0000000c00000b0c00000bbc00000bbcbbb000b
;; 228:00cccccc0c000444c0000444c0000000c00000b0c00000bbc00000bbcbbb000b
;; 229:00cccccc0c000666c0000666c0000000c00000b0c00000bbc00000bbcbbb000b
;; 230:00cccccc0c000999c0000999c0000000c00000b0c00000bbc00000bbcbbb000b
;; 231:00cccccc0c000111c0000111c0000000c00000b0c00000bbc00000bbcbbb000b
;; 234:feeeeeefefeeeddfeefffefeefeefefefeeeefeefeeeeefefeeeeeeffeeeeeef
;; 240:cbbb000bcbfbb00bcbfbbbbbcbfbbbbbcbbbbbb2cbbbbd33cb888d22cb888d22
;; 241:bb00cbbcbb00bfbcbbbbbfbcbbbbbfbc2bbbbbbc33dbbbbc22d888bc22d888bc
;; 242:0000000000000000002020000002000000202000000000000000000000000000
;; 243:0000000000000000003030000003000000303000000000000000000000000000
;; 244:0000000000000000004040000004000000404000000000000000000000000000
;; 245:0000000000000000006060000006000000606000000000000000000000000000
;; 246:0000000000000000009090000009000000909000000000000000000000000000
;; 247:0000000000000000001010000001000000101000000000000000000000000000
;; </TILES>

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

;; <MAP>
;; 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000704132333000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 001:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000400000000000000000000000000000000000000000000000000000000000000008707870000000000163615253500000000000000000000000000000000000000000000000000000000005f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 002:0000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000132323232323330000000000000000000000000000000000000000000000000000000096a6a6a6b6000000001727360000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 003:00000000000000000000000000000000001030000000000000000000000000000000000000000000000000000000000000000000004f0000000000000000000000000000000000000000000000000000142424242424340000000000000000000000000000000000000000000000000000000098a8a8a8b8000000001727370000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 004:0000000000000000000000000000000000113100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004f000000001424242424243400000000000000000000000000000081810481040000000000000000000000000000000000172737000000000090a0b0000000000000000000000000000000000000000000000000002f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 005:000000000000000000000000000000000011310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000014242424242434000000000000000000000000000090a0a0a0a0a0b00000000000000000000000000000007f172737000000000091a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 006:000000000000000000000000005f00000011310000000000000000000000000000000000000000000000000000008181810000000000000000000000000000000000000000000000000000000000000015252525252535000000000000000000000000000091a1a1a1a1a1b100000000000000000000000000000000182838000000000091a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 007:0000000000000000000000000000000000123200000000000000000000000000000000000000000000000000000090a0b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000093a3b3000000000000000091a1b1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 008:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b4000000000000000091a1b10000000000000000000000000000000000000000000000005f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 009:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b4000000000000000091a1b1133300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 010:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b4000000000000000092a2b2143400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 011:000000000000000000000000000000000000000000000000000000003e1e0000000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b100006f0000000000000000000094a4b400000000000010202020301434003f0000000000000000006f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 012:000000000000000000000000008100000000000000000000000000000f1f0081000000000000000000000000000091a1b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b40000000000001121212131143400000000000000000000000000018104840787000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 013:00000000000000000000000090b000000000000000000000000090a0a0a0a0a0b00000000000000000000000000091a1b10000000000002f00000000000001000000000000000000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b40000000000001121212131143400000000000000000000000000b4b4b4b4b495000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 014:00000000000000000000000091b100000000000000000000000091a1a1a1a1a1b10000000000000000000000000091a1b10000000000000000000000102020202020202020203000000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b40000000000001121212131143400000000000000000000000000b5a395a3b401000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 015:00000000000000000000000091b100000000000000000000810091a1a1a1a1a1b10000000000000000000000000091a1b10000000000000000000000112121212121212121213100000000000000000000000000000000000000000000000000000000000091a1a1a1a1a1b10000000000000000000000000094a4b4000000000000122222223214340000000000002f000000000000b5a395a3b434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:90a0a0a0a0a0a0a0a0b0000091b100000000000000008090a0a091a1a1a1a1a1b10000000000000000000000000091a1b10000000000000000000000112121212121212121213100000000000000000000000000000000000000000000000000000000000092a2a2a2a2a2b20000000000000000000000000095a5b50000000000000000000000143400000000000000000000000000b5a395a3b434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 017:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001b5b5b5b5b434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 018:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002f000000000000000038b5959595b434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <WAVES>
;; 000:9aaabbbbcccdddeeeeeeeeedccba9887
;; 001:33311223455632221335674200023467
;; 002:0000000000000000eeeeeeeeeeeeeeee
;; 003:01234456789abcefedcba99776543210
;; 004:00000000000333333333333000000000
;; </WAVES>

;; <SFX>
;; 000:620062006200620062006200620062006200620062006200620062006200620062006200620062006200620062006200620062006200620062006200300000000000
;; 001:020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200304000000000
;; 002:030003000300030003000300030003000300030003000300030003000300030003000300030003000300030003000300030003000300030003000300200000000000
;; 003:0000007000800080009000a000a000a000a000a00090008000500030004000400050009000b000c000d000d000d00080006000500040003000300000385000000000
;; 016:62003270828d323b22e262ee02e802d7223852445252028012d412e402f4023f0260424b024d021e224e02da522d028c024b0230529002e602e532f7484000000000
;; 017:63003370838d333b23e263ee03e803d7233853445352038013d413e403f4033f0360434b034d031e234e03da532d038c034b0330539003e603e533f7689000000000
;; 018:919391b79197918691839172915f919d919a910001000100010001000100010001000100010001000100010001000100010001000100010001000100502000000000
;; </SFX>

;; <PATTERNS>
;; 000:6000260000201000000000a06000260000001000000000a06000260000001000000000a06000264000260000001000000000000000210000000000000000000000210000000000000000000000000000000000a00000000000000000a00000000000000000000000a00000000000000000a00000000000000000000000000000a00000000000000000a00000000000a00000000000000000000000210000000000a00000a00000a00000000000a00000000000a0000000000000000000000000
;; 001:000000000021000000000000a00018000021100010000021000000000010000010000010800018000010100010000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 002:500016000000a00016000000700016000010800016100010b00016000000900016000010800016100010900016100010000000000000000000000000000000000000800099000000000000b00029000000900029000000000021d00099000021000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 003:b00006e00006000000700006000000700006d00006000000f00006700006800006600006500006900006000000700006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 004:000000f0000af00008f00004a0000cf00008d00004c0000ab0000cf0000660000cb0000040000460000eb00006d0000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </PATTERNS>

;; <TRACKS>
;; 000:100000100000180000180000180000180300180300100300100000100000000400000500000000000000000000000000a00300
;; 001:300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000200
;; </TRACKS>

;; <FLAGS>
;; 000:00101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000101010000000000010101000000000001010100000000000000000000000000010101000000000000000000000000000101010000000000000000000000000001010100000000000000000000000000000100000000000000000000000000000001000000000000000000000000000000010000000000000000000000000000000000000000000
;; </FLAGS>

;; <PALETTE>
;; 000:1a1c2c710c95b13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

