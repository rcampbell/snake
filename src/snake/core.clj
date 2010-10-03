(ns snake.core
  (:import [java.util Timer TimerTask]
	   [javax.swing JFrame]
	   [java.awt Graphics Toolkit]
	   [java.awt.image BufferStrategy]
	   [java.awt.event KeyListener KeyEvent]))

(def keys {KeyEvent/VK_UP :up
	   KeyEvent/VK_DOWN :down
	   KeyEvent/VK_LEFT :left
	   KeyEvent/VK_RIGHT :right})

(def board {:width 100 :height 100 :scale 10
	    :expand-w #(* (% :width) (% :scale))
	    :expand-h #(* (% :height) (% :scale))})

(def key (ref nil))

(def frame (doto (new JFrame)
	     (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
	     (.setSize ((board :expand-w) board)
		       ((board :expand-h) board))
	     (.setTitle "Snake")
	     (.setVisible true)
	     (.createBufferStrategy 2)
	     (.addKeyListener (proxy [KeyListener] []
				(keyPressed [e] (dosync (ref-set key (keys (.getKeyCode e)))))
				(keyReleased [e] ())
				(keyTyped [e] ())))))

(defn- project [x y]
  (let [offset (dec (board :scale))
	x (* (board :scale) x)
	y (* (board :scale) y)]
    [x y offset offset]))

(defn- render [{:keys [snake food]}]
  (let [bf (.getBufferStrategy frame)
	g (.getDrawGraphics bf)]
    (.clearRect g 0 0 ((board :expand-w) board) ((board :expand-h) board))
    (doseq [[x y] (concat snake food)]
      (let [[x y w h] (project x y)]
	(.fillRect g x y w h)))
    (.dispose g)
    (.show bf)
    (.sync (Toolkit/getDefaultToolkit))))

(defn- move [snake]
  (let [[x y] (last snake)
	body (vec (rest snake))]
    (case @key
	  :up (vec (conj body [x (dec y)]))
	  :down (vec (conj body [x (inc y)]))
	  :left (vec (conj body [(dec x) y]))
	  :right (vec (conj body [(inc x) y]))
	  snake)))

(defn- eat [ass]
  (let [eaten (clojure.set/intersection (set @snake) @food)]
    (if-not (empty? eaten)
      (dosync (alter food disj (first eaten))
	      (alter snake #(vec (cons %2 %1)) ass)
	      (alter food conj (mk-food))))))

(defn- mk-food [] [(rand-int (board :width))
		   (rand-int (board :height))])

(def snake (ref [[10 10] [10 11] [10 12]]))
(def food (ref #{(mk-food)}))

(defn- cycle []
  (let [ass (first @snake)]
    (dosync (alter snake move))
    (eat ass)
    (render {:snake @snake :food @food})))

(def timer (doto (new Timer)
	     (.schedule (proxy [TimerTask] [] (run [] (cycle))) (long 100) (long 100))))



  


