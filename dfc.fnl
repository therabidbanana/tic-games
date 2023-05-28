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
      (+ v acc))))

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

(fn inside? [{: x : y : h : w &as box} {:x x1 :y y1 &as point}]
  (and (>= x1 x) (<= x1 (+ x w))
       (>= y1 y) (<= y1 (+ y h))))

(fn touches? [{: x : y : h : w &as ent1} {&as ent2}]
  (or
   (inside? ent2 {:x (+ 0 x) :y (+ 0 y)})
   (inside? ent2 {:x (+ w x) :y (+ 0 y)})
   (inside? ent2 {:x (+ 0 x) :y (+ h y)})
   (inside? ent2 {:x (+ w x) :y (+ h y)})))

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
  (rect x y w h bg-color)
  (rectb x y w h border-color))

(fn draw-map! [{: w : h : x : y : sx : sy : trans}]
  (map (or x 0) (or y 0) (or w 30) (or h 17) (or sx 0) (or sy 0) (or trans -1)))

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
            reacted (mapv #(let [up ($:react (merge (or $.state {}) screen-state))]
                             (if (= :die up)
                                 nil
                                 (table? up)
                                 (merge $ {:state up})
                                 $))
                          self.entities)]
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
  (draw-sprite! (merge character state)))

(fn bullet-react [{&as bullet} {: x : y &as state} entities]
  (let [intersected (-?>> (filterv #(= :enemy $.tag) entities)
                          (filterv (partial touches? bullet))
                          first)
        x (+ x state.dx)
        y (+ y state.dy)]
    (if intersected
        (do (intersected:take-damage!) :die)
        (merge state {: x : y}))))

(fn build-bullet [{&as player-ent}]
  (let []
    {:react (fn [{&as bullet} {&as state} entities]
              )
     :render ()}))

(var next-color {:red :orange :orange :yellow :yellow :green :green :blue :blue :purple :purple :red})
(fn player-react [self {: x : y : color &as state} _entities]
  (let [x (if (btn 2) (- x 1) (btn 3) (+ x 1) x)
        y (if (btn 0) (- y 1) (btn 1) (+ y 1) y)
        color (if (btnp 5) (?. next-color color) color)
        ]
    (if (btnp 4)
        ;; TODO: Is there a less sneaky way to add entity?
        (^in _entitites (build-bullet )))
    (merge state {: x : y : color})))

(defscreen $screen :pause
  {:tick
   (fn []
     (cls 0)
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
     (cls 0)
     (print "Disastrous Flying" 22 22 15 false 2)
     (print "Critters" 68 52 15 false 2)
     (print "Disastrous Flying" 23 23 13 false 2)
     (print "Critters" 69 53 13 false 2)
     )
   :prepare
   (fn []
     (poke 0x03FF8 8)
     ($ui:clear-all!)
     ($ui:menu! {:box {:x 50 :w 140}
                 :options [{:label "Play Game" :action #($screen:select! :game)}
                           {:label "UI Test" :keep-open? true :action #(ui-testbed)}]}))})

(var sprite-colors {:red 256 :orange 288 :blue 320 :green 352 :purple 264 :yellow 328})
(var color-cycle [:red :orange :yellow :green :blue :purple])

(var player
     {:render (fn draw-player [{: character &as ent} {: color &as state} _others]
                (let [sprite (?. sprite-colors color)]
                  (trace sprite)
                  (draw-sprite! (merge (merge character state) {: sprite}))))
      :react player-react
      :state {:x player-x :y player-y :color :yellow}
      :tags [:player]
      :character
      {;; Test weird blink patterns
       :animate {:period 200 :steps [{:t 0 :index 1} {:t 100 :index 2} {:t 112 :index 1}
                                     {:t 115 :index 3} {:t 130 :index 1}]}
       :trans 0
       :x player-x :y player-y :w 2 :h 2}})

(fn enemy-react [self {: x : y : dx : dy : ticks} _entities]
  (let [x (if dx (+ x dx) x)
        y (if dy (+ y dy) y)
        dy (math.sin (/ ticks 40))]
    (if (< x -10)
        :die
        {: x : y : dx : dy})))

(fn build-enemy [base-state]
  {:render draw-entity
   :react enemy-react
   :state (merge {} (or base-state {}))
   :character
   {:sprite 432 :trans 0 :w 2 :h 2}})

(defscreen $screen :game
  {:state {:ticks t}
   :tick
   (fn [self screen-state]
     (if (btnp 7) ($screen:select! :pause))
     (react-entities! self screen-state)
     (cls 0)
     (draw-entities! self screen-state)
     (if (empty? self.entities)
         ($screen:select! :title))
     (print (.. "Pressed 5? " (if (btnp 5) "true" "false")) 20 20 13 )
     (print (.. "HELLO WORLD! t=" screen-state.ticks) 84 84 13)
     {:ticks (+ screen-state.ticks 1)})
   :entities []
   :add-entity! (fn [self ent] (into self.entities [ent]))
   :prepare
   (fn prepare-game [self]
     (poke 0x03FF8 10)
     (tset self :entities [])
     (tset self :state {:ticks 0})
     ($ui:clear-all!)
     (self:add-entity! (merge player {:state {:x 0 :y 20 :color :orange}}))
     (self:add-entity! (build-enemy {:dx -0.5 :x 200 :y 40}))
     (self:add-entity! (build-enemy {:dx -0.5 :dy 1 :x 240 :y 100}))
     )})

(fn _G.BOOT []
  ($screen:select! :title)
  )

(fn _G.TIC []
  ($screen:tick!))

;; <TILES>
;; 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
;; 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
;; 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
;; 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
;; 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
;; 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
;; 032:0033006603300660330066003006600300660033066003306600330060033006
;; 033:0033006603300660330066003006600300660033066003306600330060033006
;; 034:eccccccccc222222c3333333c3222222c3ccccccc3cc0cccc3cc0cccc3cc0ccc
;; 035:ccccceee2222ccee33330cee22230ceeccc30ccc0cc30c0c0cc30c0c0cc30c0c
;; 036:ebbbbbbbbb777777b1111111b1777777b1ccccccb1cc6cccb1cc6cccb1cc6ccc
;; 037:bbbbbeee7777bbee11110bee77710beeccc10bbb6cc10b0b6cc10b0b6cc10b0b
;; 048:0033006603300660330066003006600300660033066003306600330060033006
;; 049:0033006603300660330066003006600300660033066003306600330060033006
;; 050:c3ccccccc3333333c333c333c3333cccc3333333c2222222cc000cccecccccec
;; 051:ccc300cc33330ccec3330cee33330cee33330cee2222ccee000cceeecccceeee
;; 052:b1ccccccb1111111b111c111b1111cccb1111111b7777777bb000bbbebbbbbeb
;; 053:ccc100bb11110bbec1110bee11110bee11110bee7777bbee000bbeeebbbbeeee
;; 066:ebbbbbbbbb111111b2222222b2111111b2ccccccb2cc3cccb2cc3cccb2cc3ccc
;; 067:bbbbbeee1111bbee22220bee11120beeccc20bbb3cc20b0b3cc20b0b3cc20b0b
;; 082:b2ccccccb2222222b222c222b2222cccb2222222b1111111bb000bbbebbbbbeb
;; 083:ccc200bb22220bbec2220bee22220bee22220bee1111bbee000bbeeebbbbeeee
;; 096:00000000000000330000003300000000000000b0000000bb000000bb0bbb000b
;; 097:00000000310000003100000001000000b1b00000bbb00000bbb00000bb000bb0
;; 112:0bbb000b0bfbb00b0bfbbbbb0bfbbbbb0bbbbbb20bbbbd330b888d220b888d22
;; 113:bb000bb0bb00bfb0bbbbbfb0bbbbbfb02bbbbbb033dbbbb022d888b022d888b0
;; </TILES>

;; <SPRITES>
;; 000:0000000000000000000000000000000000000022000002220000022200002222
;; 001:0000000000000000040404002244400022cccc0024cfcf0022cccc0023333000
;; 002:0000000000000000000000000000000000000022000022220002222200022222
;; 003:0000000000000000040404002244400022cccc0024cfcf0022cccc0023333000
;; 004:0000000000000000000000000002220000000222000022220002222200022222
;; 005:0000000000000000040404002244400022cccc0024cfcf0022cccc0023333000
;; 008:0000000000000000000000000000000000000011000001110000011100001111
;; 009:0000000000000000040404001144400011cccc0014c7c70011cccc0012222000
;; 010:0000000000000000000000000000000000000011000011110001111100011111
;; 011:0000000000000000040404001144400011cccc0014c7c70011cccc0012222000
;; 012:0000000000000000000000000001110000000111000011110001111100011111
;; 013:0000000000000000040404001144400011cccc0014c7c70011cccc0012222000
;; 016:0000222266602222666605556655553300000033000000c00000006000000000
;; 017:33333000333333003333555533330000333000000c0000000600000000000000
;; 018:0000222066602220666605556655553300000033000000c00000006000000000
;; 019:33333000333333003333555533330000333000000c0000000600000000000000
;; 020:0000000066600000666605556655553300000033000000c00000006000000000
;; 021:33333000333333003333555533330000333000000c0000000600000000000000
;; 024:0000111166601111666605556655552200000022000000c00000001000000000
;; 025:22222000222222002222555522220000222000000c0000000100000000000000
;; 026:0000111066601110666605556655552200000022000000c00000001000000000
;; 027:22222000222222002222555522220000222000000c0000000100000000000000
;; 028:0000000066600000666655556655552200000022000000c00000001000000000
;; 029:22222000222222002222555522220000222000000c0000000100000000000000
;; 032:0000000000000000000000000000000000000033000003330000033300003333
;; 033:0000000000000000040404003344400033cccc0034cfcf0033cccc0034444000
;; 034:0000000000000000000000000000000000000033000033330003333300033333
;; 035:0000000000000000040404003344400033cccc0034cfcf0033cccc0034444000
;; 036:0000000000000000000000000003330000000333000033330003333300033333
;; 037:0000000000000000040404003344400033cccc0034cfcf0033cccc0034444000
;; 040:00000000000000000000000000000000000000aa00000aaa00000aaa0000aaaa
;; 041:000000000000000005050500aa555000aacccc00a1c7c700aacccc00a1111000
;; 042:00000000000000000000000000000000000000aa0000aaaa000aaaaa000aaaaa
;; 043:000000000000000005050500aa555000aacccc00a1c7c700aacccc00a1111000
;; 044:000000000000000000000000000aaa0000000aaa0000aaaa000aaaaa000aaaaa
;; 045:000000000000000005050500aa555000aacccc00a1c7c700aacccc00a1111000
;; 048:0000333366603333666605556655554400000044000000c00000006000000000
;; 049:44444000444444004444555544440000444000000c0000000600000000000000
;; 050:0000333066603330666605556655554400000044000000c00000006000000000
;; 051:44444000444444004444555544440000444000000c0000000600000000000000
;; 052:0000000066600000666605556655554400000044000000c00000006000000000
;; 053:44444000444444004444555544440000444000000c0000000600000000000000
;; 056:0000aaaa9990aaaa999906669966661100000011000000c00000002000000000
;; 057:11111000111111001111666611110000111000000c0000000200000000000000
;; 058:0000aaa09990aaa0999906669966661100000011000000c00000002000000000
;; 059:11111000111111001111666611110000111000000c0000000200000000000000
;; 060:0000000099900000999906669966661100000011000000c00000002000000000
;; 061:11111000111111001111666611110000111000000c0000000200000000000000
;; 064:0000000000000000000000000000000000000088000008880000088800008888
;; 065:0000000000000000040404008844400088cccc0084cfcf0088cccc0081111000
;; 066:0000000000000000000000000000000000000088000088880008888800088888
;; 067:0000000000000000040404008844400088cccc0084cfcf0088cccc0081111000
;; 068:0000000000000000000000000008880000000888000088880008888800088888
;; 069:0000000000000000040404008844400088cccc0084cfcf0088cccc0081111000
;; 072:0000000000000000000000000000000000000044000004440000044400004444
;; 073:0000000000000000050505004455500044cccc0045cfcf0044cccc0046666000
;; 074:0000000000000000000000000000000000000044000044440004444400044444
;; 075:0000000000000000050505004455500044cccc0045cfcf0044cccc0046666000
;; 076:0000000000000000000000000004440000000444000044440004444400044444
;; 077:0000000000000000050505004455500044cccc0045cfcf0044cccc0046666000
;; 080:0000888877708888777705557755551100000011000000c00000006000000000
;; 081:11111000111111001111555511110000111000000c0000000600000000000000
;; 082:0000888077708880777705557755551100000011000000c00000006000000000
;; 083:11111000111111001111555511110000111000000c0000000600000000000000
;; 084:0000000077700000777705557755551100000011000000c00000006000000000
;; 085:11111000111111001111555511110000111000000c0000000600000000000000
;; 088:0000444477704444777705557755556600000066000000c00000002000000000
;; 089:66666000666666006666555566660000666000000c0000000200000000000000
;; 090:0000444077704440777705557755556600000066000000c00000002000000000
;; 091:66666000666666006666555566660000666000000c0000000200000000000000
;; 092:0000000077700000777705557755556600000066000000c00000002000000000
;; 093:66666000666666006666555566660000666000000c0000000200000000000000
;; 096:0000000000000000000000000000000000000066000006660000066600006666
;; 097:0000000000000000040404006644400066cccc0064cfcf0066cccc0069999000
;; 098:0000000000000000000000000000000000000066000066660006666600066666
;; 099:0000000000000000040404006644400066cccc0064cfcf0066cccc0069999000
;; 100:0000000000000000000000000006660000000666000066660006666600066666
;; 101:0000000000000000040404006644400066cccc0064cfcf0066cccc0069999000
;; 112:0000666677706666777705557755559900000099000000c00000006000000000
;; 113:99999000999999009999555599990000999000000c0000000600000000000000
;; 114:0000666077706660777705557755559900000099000000c00000006000000000
;; 115:99999000999999009999555599990000999000000c0000000600000000000000
;; 116:0000000077700000777705557755559900000099000000c00000006000000000
;; 117:99999000999999009999555599990000999000000c0000000600000000000000
;; 128:0000000000001010000041400000111001111110011111101101001010000000
;; 144:00cc000c000c0cc0000c0c000002c20000cccc00dccccc00cccc0c00000c0000
;; 160:0000000000000000003333440323340000323000000000000000000000000000
;; 176:0000004000000442000044320003422200032222222333332232333322202313
;; 177:0000000000000000400000004000000022400000334022203232322013332220
;; 182:0000000000000000006000000666000006677700666077776607777466077777
;; 183:0000000000000000666600006666600006666660777666604740666077706660
;; 192:2200333300033377000333330003022200000022000002220000222200000000
;; 193:3330002073333000333030002220300022200000220000002220000000000000
;; 198:6007777707777677777766777707760070007700000077770000000700000000
;; 199:c7c0000077000000667000007770000007770000000000000000000000000000
;; 208:0000002200000223000000210000002100000221000222210022333100000001
;; 209:2000000022000000200000002200000012222200133320001000000010000000
;; 214:0000000011999999015999950999999909959959099999990999999909999999
;; 215:0000000011100000190000009900000099000000990000009990099999999999
;; 224:0000000100000001000000010000001100000011000000110000011100000111
;; 225:11000000110000001110000011100000e11e0000111110001eee100011111000
;; 230:0099999900000099000009990000099000000090000000000000000000000000
;; 231:999990099ff99000999990000a0990000a0a00000a0a00000000000000000000
;; </SPRITES>

;; <MAP>
;; 000:020200000000000000000000000000000000000000000000000000020202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 001:020000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 015:020000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:020200000000000000000000000000000000000000000000000000000202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <WAVES>
;; 000:9aaabbbbcccdddeeeeeeeeedccba9887
;; 001:33311223455632221335674200023467
;; </WAVES>

;; <SFX>
;; 000:518141813181319f319f219f219f219f319f319e319e419d518c718c718b718b718a718c71808184818781a38182818281a181865192318e218b218b385000000000
;; </SFX>

;; <PATTERNS>
;; 000:9000a80000200000208000a8000000000000000000c000a8000000000000000000a000a8000000b000a80000007000a80000000000210000000000000000000000210000000000000000000000000000007000a8000000000000d000a80000000000000000008000a80000000000008000a8000000000000000000000000d000a8000000000000b000a8000000a000a80000000000000000006000290000000000a07000a80000a00000000000a00000009000a8000000000000000000000000
;; 001:00000050002b000000000000000000100021d0002b100021000000000000000000000021000000000021000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 002:500029000000a00029000000000000700029700029000021000000000000100021000000000000000000000000000000000000000000000000000000000000000000800099000000000000b00029000000900029000000000021d00099000021000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </PATTERNS>

;; <TRACKS>
;; 000:100000200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a00000
;; 001:300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000200
;; </TRACKS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

