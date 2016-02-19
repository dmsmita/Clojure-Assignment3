(ns assignment3-ns
  (:require [quil.core :as q ])
  (:require [clojure.java.io :as io] )
)

;; State holds the index of the command that is being run.
(def state (atom {:mode "step" :current_command_index -1 }))

;;Represents the commands file as a vector of maps, each line of the file represents a command map
(def commands (atom []))

;;holds the turtle position, pen state and angle at which the turle is pointing
(def turtle_state (atom {:x 0 :y 0 :angle 90 :pen true}))

;; given a line from commands.txt, this function returns a command map
(defn process-line [line]

  (def line_tokens (.split line " "))

  (def command (first line_tokens))
  (def attr (second line_tokens))

  {:command command :attr attr :raw_command line}
)

;; Reads the commands.txt file to create a commands vector and updates the state with command count
(defn read-commands-file []
  (def data-file (io/file (io/resource "commands.txt" )))

  (def lines (.split (slurp data-file) "\n"))

  (doseq [line lines]
    (swap! commands conj (process-line line))
  )

  (swap! state assoc-in [:commands_size] (count lines))
)


(defn setup []
  (q/no-loop)
  (read-commands-file)
)

(defn is-valid-index [index]
  (> index -1)
)

(defn update-angle [angle]
  (swap! turtle_state assoc-in [:angle] angle)
)

(defn update-xy [ x y]
  (do
    (swap! turtle_state assoc-in [:x] x)
    (swap! turtle_state assoc-in [:y] y)
  )
)

(defn update-pen [ pen]
  (swap! turtle_state assoc-in [:pen] pen)
)

(defn execute-command [command]
  (def command_str (get command :command))
  (def attr (get command :attr))

  (if (and (= "pen" command_str) (= "up" attr))
    (update-pen false)
  )

  (if (and (= "pen" command_str)(= "down" attr))
    (update-pen true)
  )

  (if (= "turn" command_str)

    (do
     (def angle (Integer/parseInt attr))
     (update-angle (mod (+ angle (get @turtle_state :angle)) 360))
    )
  )

  (if (= "move" command_str)
    (do
      (def cur-x (get @turtle_state :x))
      (def cur-y (get @turtle_state :y))

      (def steps (Integer/parseInt attr))

      (def angle_radians (q/radians (get @turtle_state :angle)))

      (def new-x (+ (* (Math/cos angle_radians) steps) cur-x))
      (def new-y (+ (* (Math/sin angle_radians) steps) cur-y))

      (if (get @turtle_state :pen)
        (q/line cur-x cur-y new-x new-y))

      (update-xy new-x new-y)
    )
  )

)

(defn turtle_home []
  (update-xy  50 50)
  (update-angle 90)
  (update-pen true)
)

;; Draw clears the screen and resets turtle position to home and executes all the commands till current_command_index
(defn draw []
   (q/background 240)
   (q/fill 0)

   (turtle_home)

   (def ci (get @state :current_command_index))

   (if (is-valid-index ci)
     (do
       (q/text (get (nth @commands ci ) :raw_command) 10 10)

       (doseq [i (range (+ 1 ci))]
         (do
          (execute-command (nth @commands i))
         )
       )
     )
   )
)

;;increment :current_command_index and redraw the screen
(defn do-command []
    (def ci (get @state :current_command_index))
    (if (< (+ ci 1) (get @state :commands_size))
      (swap! state assoc-in [:current_command_index] (+ ci 1))
    )
   (q/redraw)
)

;;decrement :current_command_index and redraw the screen
(defn undo-command []
  (def ci (get @state :current_command_index))
    (if (>= (- ci 1) -1)
      (swap! state assoc-in [:current_command_index] (- ci 1))
    )
  (q/redraw)
)

;;sets the current index to the end of commands vector.
(defn switch-to-run-mode []

  (swap! state assoc-in [:current_command_index] (- (get @state :commands_size)  1))
  (swap! state assoc-in [:mode] "step")

  (q/redraw)
)

(defn keyboard-action []

  (let [key (q/key-as-keyword)]
    (if (or (= key :up) (= key :right))
     (do-command)
    )

    (if (or (= key :down) (= key :left))
      (undo-command)
    )

    (if (= key :r)
      (switch-to-run-mode)
    )
  )
  )

(q/defsketch assignment3
  :setup setup
  :draw draw
  :key-pressed keyboard-action
  :features [:keep-on-top]
 )
