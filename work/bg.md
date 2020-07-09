# Background Generator
This is a simple background generator. It is fun.

(ex
(* 2 2)
[:p "This is rendered by the hiccup compiler."]
ex)

## Method
It just randomly prints "\" or "/" in a grid filling up your canvas.

(ex
(use 'stylo.draw)

(let [bg-col (hsl-str (rand-int 360) (rand-int 101) (rand-int 101))
      l-col (hsl-str (rand-int 360) (rand-int 101) (rand-int 101))
      lines [(assoc-in (ln [0 0] [1 1]) [1 :stroke] l-col)
             (assoc-in (ln [0 1] [1 0]) [1 :stroke] l-col)]
      sc 10
      w 300
      h 300]
  (figure [w h sc]
    "wasd"
    (rect (/ w sc) (/ h sc) bg-col)
    (for [x (range (/ w sc))
          y (range (/ h sc))]
      (mv [x y] (get lines (rand-int 2))))))

ex)

