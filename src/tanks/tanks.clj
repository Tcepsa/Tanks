;; (compile 'tanks.DynaFrame)
;; TODO: Add a proper ns block and main- method rather than having everything run whenever it is compiled

(comment
  (doseq [listener (.getKeyListeners frame)] (.removeKeyListener frame listener))
  (.stop timer))



(def MAIN-FRAME-TITLE "Tanks!")

 (def frame (tanks.DynaFrame. MAIN-FRAME-TITLE))

(println "Resetting...")

(import [javax.swing JPanel Timer]
		  [java.awt Dimension Color Font RenderingHints]
		  [java.awt.event ActionListener KeyListener KeyEvent])

(defmacro with-action [component event & body]
  `(. ~component addActionListener
      (proxy [ActionListener] []
        (actionPerformed [~event] ~@body))))

(defmacro with-key-event [component event & body]
  `(. ~component addKeyListener
		(proxy [KeyListener] []
		  (keyTyped [~event])
		  (keyPressed [~event] ~@body)
		  (keyReleased [~event]))))

(defmacro with-color [g color & body]
  `(let [old-color# (.getColor ~g)]
	  (.setColor ~g ~color)
	  ~@body
	  (.setColor ~g old-color#)))

(def WIDTH 600)
(def HEIGHT 500)

(def TANK-WIDTH 15)
(def TANK-HEIGHT 5)

(def GRAVITY 0.1)


(def PERIOD 13)

(def BG-COLOR Color/BLACK)

(def FONT (Font. "Sans" Font/PLAIN 25))

(def TERRAIN-ROUGHNESS 20)

(def current-player (atom 0))

(def rectangle (atom {:x 10 :y 10 :width 100 :height 100 :vel-x 5 :vel-y 5 :fg-color Color/BLUE :border-color Color/GREEN}))

(def tanks [(atom {:x (int (rand (/ WIDTH 2))) :angle 45 :power 50 :color Color/WHITE}) (atom {:x (int (+ (/ WIDTH 2) (rand (/ WIDTH 2)))) :angle 135 :power 50 :color Color/WHITE})])

(def shell (atom {:x -1 :y -1 :vel-x 0 :vel-y 0 :tick 0 :tick-threshold 0}))

(def game-state (atom :running))

(def dead-tanks (atom []))

(defn reset-shell! []
  (reset! shell (assoc @shell :x -1 :y -1 :vel-x 0 :vel-y 0)))

(defn generate-terrain [roughness]
  (let [lower-bound (/ HEIGHT 5)
		  upper-bound (* 4 (/ HEIGHT 5))]
	 (loop [x 1
			  accum [(int (+ (/ HEIGHT 5) (rand (* 3 (/ HEIGHT 5)))))]]
		(if (>= x WIDTH)
		  accum
		  (if (reduce #(or %1 %2) (for [tank (map deref tanks)] (and (> x (tank :x)) (< x (+ (tank :x) TANK-WIDTH)))))
			 (recur (inc x) (conj accum (last accum)))
			 (let [previous (last accum)
					 delta (int (- (rand roughness) (/ roughness 2)))
					 delta (if (or (> (+ previous delta) upper-bound) (< (+ previous delta) lower-bound)) (- delta) delta)]
				(recur (inc x) (conj accum (+ previous delta)))))))))

(def terrain (atom (generate-terrain 20)))

(def TERRAIN-COLOR Color/MAGENTA)

(defn restart-game! []
  (reset-shell!)
  (reset! current-player 0)
  (reset! (tanks 0) {:x (int (rand (/ WIDTH 2))) :angle 45 :power 50 :color Color/WHITE})
  (reset! (tanks 1) {:x (int (+ (/ WIDTH 2) (rand (/ WIDTH 2)))) :angle 135 :power 50 :color Color/WHITE})
  (reset! terrain (generate-terrain TERRAIN-ROUGHNESS))
  (reset! dead-tanks [])
  (reset! game-state :running))

(defn current-player-tank []
  (nth tanks @current-player))

(defn adjust-current-player-tank [property new-value]
  (let [tank (current-player-tank)]
	 (reset! tank (assoc @tank property new-value))))

(defn switch-to-next-player! []
  (reset! current-player (mod (inc @current-player) (count tanks))))

(defn next-turn! []
  (reset-shell!)
  (switch-to-next-player!))


(defn update-rectangle! []
  (let [{:keys [x y width height vel-x vel-y] :as old-rectangle} @rectangle
		  new-vel-x (if (or (> (+ x vel-x width) WIDTH) (< (+ x vel-x) 0)) (- vel-x) vel-x)
		  new-vel-y (if (or (> (+ y vel-y height) HEIGHT) (< (+ y vel-y) 0)) (- vel-y) vel-y)
		  new-x (+ x new-vel-x)
		  new-y (+ y new-vel-y)]
	 (reset! rectangle (assoc old-rectangle :x new-x :y new-y :vel-x new-vel-x :vel-y new-vel-y))))

(reset! rectangle (assoc @rectangle :vel-x 3))

(defn new-position-from-velocity [{:keys [x y vel-x vel-y]}]
  {:x (+ x vel-x) :y (+ y vel-y)})

(defn update-shell! []
  (let [{:keys [x y vel-x vel-y tick tick-threshold]} @shell]
	 (if (>= tick tick-threshold)
		(let [new-position (new-position-from-velocity @shell)
				new-vel-y (- (:vel-y @shell) GRAVITY)]
		  (reset! shell (assoc @shell :x (new-position :x) :y (new-position :y) :vel-y new-vel-y :tick 0)))
		(reset! shell (assoc @shell :tick (inc (@shell :tick)))))))

(defn launch-shell! []
  (println "Launching shell!")
  (let [{:keys [x power angle]} @(tanks @current-player)
		  power (/ power 10.0)
		  rad-angle (/ (* angle Math/PI) 180)
		  old-shell @shell
		  new-shell (assoc old-shell :x (int (+ x (/ TANK-WIDTH 2)))
								 :y (int (+ TANK-HEIGHT (@terrain x) 1))
								 :vel-x (* power (Math/cos rad-angle))
								 :vel-y (* power (Math/sin rad-angle)))]
	 (reset! shell new-shell)
	 (println "Shell: " @shell)))

(defn update-animation! []
  (when (>= (:y @shell) 0)
	 (update-shell!)
	 (when (and (> (:x @shell) 0) (< (:x @shell) WIDTH))
		(doseq [tank tanks]
		  (let [{:keys [x color]} @tank
				  min-x x
				  max-x (+ min-x TANK-WIDTH)
				  min-y (@terrain x)
				  max-y (+ min-y TANK-HEIGHT)
				  shell-x (:x @shell)
				  shell-y (:y @shell)]
			 (when (and (> shell-x min-x) (< shell-x max-x) (> shell-y min-y) (< shell-y max-y))
				(reset! dead-tanks (conj @dead-tanks {:x min-x :y max-y :color color}))
				(reset-shell!)
				(reset! game-state :finale))))
		 (println @shell)
		(if (and (= @game-state :running) (< (:y @shell) (@terrain (int (:x @shell)))))
		  (next-turn!))))

  (when (= @game-state :finale)
	 (let [{:keys [x y color] :as top-tank} (last @dead-tanks)]
		(if (> y HEIGHT)
		  (reset! game-state :finished)
		  (reset! dead-tanks (conj @dead-tanks (assoc top-tank :y (+ y (int (* 0.9 TANK-HEIGHT))))))))))




(defn draw-terrain [g]
  (doseq [x (range WIDTH)]
	 (with-color g TERRAIN-COLOR
		(.drawRect g x (- HEIGHT (@terrain x)) 1 (@terrain x)))))

(defn draw-tank [g {:keys [x y color]}]
  (with-color g color
	 (.drawRect g x (- HEIGHT (+ y TANK-HEIGHT)) TANK-WIDTH TANK-HEIGHT)))

(defn draw-tanks [g]
  (doseq [{:keys [x color]} (map deref tanks)]
	 (let [height (@terrain x)
			 y (- HEIGHT (+ height TANK-HEIGHT))]
		(draw-tank g {:x x :y height :color color}))))

(defn draw-dead-tanks [g]
  (doseq [dead-tank @dead-tanks]
	 (draw-tank g dead-tank)))

(defn draw-status [g]
  (.setFont g FONT)
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
  (with-color g Color/WHITE 
	 (loop [tanks tanks
			  x-offset 0]
		(if (empty? tanks)
		  nil
		  (let [{:keys [power angle]} (deref (first tanks))]
			 (.drawString g "Power: " x-offset 20)
			 (.drawString g (str power) (+ x-offset 85) 20)
			 (.drawString g "Angle: " x-offset 45)
			 (.drawString g (str angle) (+ x-offset 85) 45)
			 (recur (rest tanks) (+ x-offset 180)))))
	 (cond (= @game-state :running) (.drawString g (str "Player " (+ @current-player 1) "'s turn") (- WIDTH 180) 20)
			 (= @game-state :finished) (do (.drawString g (str "Player " (+ @current-player 1) " Wins!") (- WIDTH 220) 20)
													 (.drawString g (str "Press 'r' to restart") (- WIDTH 220) 45)))))

(defn draw-shell [g]
  (let [{:keys [x y]} @shell]
;;	 (println "new shell coords (" x "," y ")")
	 (with-color g Color/GREEN
		(.fillRect g x (- HEIGHT y) 5 5))))

(def panel (proxy [JPanel] []
				 (paintComponent [g]
									  (let [{:keys [x y width height fg-color border-color]} @rectangle]
										 (proxy-super paintComponent g)
										 (with-color g BG-COLOR
											(.fillRect g 0 0 WIDTH HEIGHT))
										 (with-color g border-color
											(.drawRect g x y width height))
										 (with-color g fg-color
											(.fillRect g (+ x 1) (+ y 1) (- width 1) (- height 1)))
										 (draw-terrain g)
										 (draw-tanks g)
										 (draw-dead-tanks g)
										 (draw-status g)
										 (draw-shell g)
										 (when (and (< (:y @shell) 0) (not (= (:x @shell) -1)))
											(next-turn!))))
				 (getPreferredSize []
										 (Dimension. WIDTH HEIGHT))))

(.display frame panel)

(with-key-event frame e
  (println "Key event: " (.getKeyCode e))
  (cond (= @game-state :running) (when (< (:y @shell) 0)
											  (let [code (.getKeyCode e)
													  tank (deref (current-player-tank))
													  {:keys [power angle]} tank]
												 (cond (= code KeyEvent/VK_UP) (when (< power 100) (adjust-current-player-tank :power (inc power)))
														 (= code KeyEvent/VK_DOWN) (when (> power 0) (adjust-current-player-tank :power (dec power)))
														 (= code KeyEvent/VK_LEFT) (when (< angle 180) (adjust-current-player-tank :angle (inc angle)))
														 (= code KeyEvent/VK_RIGHT) (when (> angle 0) (adjust-current-player-tank :angle (dec angle)))
														 (= code KeyEvent/VK_S) (switch-to-next-player!)
														 (= code KeyEvent/VK_SPACE) (launch-shell!)
														 :else (println "Do nothing here!"))))
		  (= @game-state :finished) (let [code (.getKeyCode e)]
												(cond (= code KeyEvent/VK_R) (restart-game!)))))

(def timer (Timer. PERIOD nil))

(with-action timer e
  (update-rectangle!)
  (update-animation!)
  (.repaint panel))

(.start timer) 
