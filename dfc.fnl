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

(fn draw-entity [{ : character} state _entities]
  (draw-sprite! (merge character state)))

(fn player-react [self {: x : y : ticks : sprite} _entities]
  (let [x (if (btn 2) (- x 1) (btn 3) (+ x 1) x)
        y (if (btn 0) (- y 1) (btn 1) (+ y 1) y)]
    (if (btnp 5)
        :die
        {: x : y : sprite})))

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
     (print "Disastrous Flying" 24 24 13 false 2)
     (print "Critters" 70 54 13 false 2)
     )
   :prepare
   (fn []
     (poke 0x03FF8 8)
     ($ui:clear-all!)
     ($ui:menu! {:box {:x 50 :w 140}
                 :options [{:label "Play Game" :action #($screen:select! :game)}
                           {:label "UI Test" :keep-open? true :action #(ui-testbed)}]}))})

(var player
     {:render draw-entity
      :react player-react
      :state {:x player-x :y player-y :sprite player-sprite}
      :character
      {:sprite player-sprite
       :ticks t
       ;; Test weird blink patterns
       :animate {:period 200 :steps [{:t 0 :index 1} {:t 100 :index 2} {:t 112 :index 1}
                                     {:t 115 :index 3} {:t 130 :index 1}]}
       :trans 14
       :x player-x :y player-y :w 2 :h 2}})


(defscreen $screen :game
  {:state {:ticks t}
   :tick
   (fn [self screen-state]
     (if (btnp 6) ($screen:select! :pause))
     (react-entities! self screen-state)
     (cls 0)
     (draw-entities! self screen-state)
     (if (empty? self.entities)
         ($screen:select! :title))
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
     (self:add-entity! (merge player {:state {:x 0 :y 20 :sprite player-sprite}}))
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
;; </TILES>

;; <SPRITES>
;; 000:00000000000000000000000000000000000000bb00000bbb00000bbb0000bbbb
;; 001:000000000000000004040400bb444000bbcccc00b4cfcf00bbcccc00b9999000
;; 002:00000000000000000000000000000000000000bb0000bbbb000bbbbb000bbbbb
;; 003:000000000000000004040400bb444000bbcccc00b4cfcf00bbcccc00b9999000
;; 004:000000000000000000000000000bbb0000000bbb0000bbbb000bbbbb000bbbbb
;; 005:000000000000000004040400bb444000bbcccc00b4cfcf00bbcccc00b9999000
;; 016:0000bbbb3330bbbb333322223322229900000099000000600000006000000000
;; 017:9999900099999900999922229999000099900000060000000600000000000000
;; 018:0000bbb03330bbb0333322223322229900000099000000600000006000000000
;; 019:9999900099999900999922229999000099900000060000000600000000000000
;; 020:0000000033300000333322223322229900000099000000600000006000000000
;; 021:9999900099999900999922229999000099900000060000000600000000000000
;; 032:0000000000000000000000000000000000000011000001110000011100001111
;; 033:0000000000000000040404001144400011cccc0014cfcf0011cccc0019999000
;; 034:0000000000000000000000000000000000000011000011110001111100011111
;; 035:0000000000000000040404001144400011cccc0014cfcf0011cccc0019999000
;; 036:0000000000000000000000000001110000000111000011110001111100011111
;; 037:0000000000000000040404001144400011cccc0014cfcf0011cccc0019999000
;; 048:0000111133301111333322223322229900000099000000600000006000000000
;; 049:9999900099999900999922229999000099900000060000000600000000000000
;; 050:0000111033301110333322223322229900000099000000600000006000000000
;; 051:9999900099999900999922229999000099900000060000000600000000000000
;; 052:0000000033300000333322223322229900000099000000600000006000000000
;; 053:9999900099999900999922229999000099900000060000000600000000000000
;; 064:0000000000000000000000000000000000000088000008880000088800008888
;; 065:0000000000000000040404008844400088cccc0084cfcf0088cccc0089999000
;; 066:0000000000000000000000000000000000000088000088880008888800088888
;; 067:0000000000000000040404008844400088cccc0084cfcf0088cccc0089999000
;; 068:0000000000000000000000000008880000000888000088880008888800088888
;; 069:0000000000000000040404008844400088cccc0084cfcf0088cccc0089999000
;; 080:0000888833308888333322223322229900000099000000600000006000000000
;; 081:9999900099999900999922229999000099900000060000000600000000000000
;; 082:0000888033308880333322223322229900000099000000600000006000000000
;; 083:9999900099999900999922229999000099900000060000000600000000000000
;; 084:0000000033300000333322223322229900000099000000600000006000000000
;; 085:9999900099999900999922229999000099900000060000000600000000000000
;; 096:0000000000000000000000000000000000000066000006660000066600006666
;; 097:0000000000000000040404006644400066cccc0064cfcf0066cccc0069999000
;; 098:0000000000000000000000000000000000000066000066660006666600066666
;; 099:0000000000000000040404006644400066cccc0064cfcf0066cccc0069999000
;; 100:0000000000000000000000000006660000000666000066660006666600066666
;; 101:0000000000000000040404006644400066cccc0064cfcf0066cccc0069999000
;; 112:0000666633306666333322223322229900000099000000600000006000000000
;; 113:9999900099999900999922229999000099900000060000000600000000000000
;; 114:0000666033306660333322223322229900000099000000600000006000000000
;; 115:9999900099999900999922229999000099900000060000000600000000000000
;; 116:0000000033300000333322223322229900000099000000600000006000000000
;; 117:9999900099999900999922229999000099900000060000000600000000000000
;; </SPRITES>

;; <MAP>
;; 000:020200000000000000000000000000000000000000000000000000020202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 001:020000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 015:020000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 016:020200000000000000000000000000000000000000000000000000000202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

