(in-package #:cl-zombsole)

(defparameter *rules* nil
  "The current rules of the game")

(defparameter *available-rules* nil
  "The list of all available rules for the game")

(defparameter *game* nil
  "The current game instance")

(defparameter *players* (make-hash-table :test 'equalp)
  "The list of the current players")

(defun play-game (options)
  ;; Initiate a game, using the command line arguments as configuration.
  (let ((rules-name (getf options :rules))
        (initial-zombies (getf options :initial-zombies))
        (minimum-zombies (getf options :minimum-zombies))
        (debug (getf options :debug))
        (use-basic-icons (getf options :basic-icons))
        (max-frames (getf options :max-frames))
        (map-name (getf options :map))
        (size (getf options :size))
        (player-names (getf options :players))
        (slynk-server (getf options :slynk))
        (swank-server (getf options :swank))
        map
        players-extended)

    (when swank-server
      (format t "starting Swank server ...")
      (swank:create-server :port 4005 :interface "0.0.0.0" :dont-close t :style :spawn))

    (when slynk-server
      (format t "starting Slynk server ...")
      (slynk:create-server :port 4006 :interface "0.0.0.0" :dont-close t :style :spawn))

    (setf player-names (uiop:split-string player-names :separator '(#\, #\:)))

    (setf players-extended '())

    (loop :for (player-name count) :on player-names :by #'cdr
          :for count-integer = (or (and count (parse-integer count :junk-allowed t)) 1)
          :do (if count-integer
                  (alexandria:appendf players-extended
                                      (loop :repeat count-integer :collect player-name))))

    (if size
        (setf size (loop :for dimension :in (uiop:split-string size :separator '(#\x))
                         :collect (parse-integer dimension))))

    (if map-name
        (let ((map-file (merge-pathnames (asdf:system-relative-pathname :cl-zombsole "maps/") map-name)))
          (unless (probe-file map-file)
            (error "Map file not found"))
          (setf map (load-map map-file))
          (if size
              (if (or (< (first size) (first (zmap-size map)))
                      (< (second size) (second (zmap-size map))))
                 (error "Map (~a) doesn't fit in specified size (~a) ~% (leave it empty to use best fit)"
                        (zmap-size map) size)
                 (setf (zmap-size map) size))))
        (progn
          (unless size
            (setf size (list 30 10)))
          (setf map (make-zmap :size size))))

    ;; create and start game
    (let ((game
            (make-instance
             'game
             :player-names players-extended
             :rules-name rules-name
             :game-map map
             :initial-zombies initial-zombies
             :game-minimum-zombies minimum-zombies
             :game-debug debug
             :game-use-basic-icons use-basic-icons)))
      (play game max-frames))))

(defun icon-box-p (char)
  (or (= (char-code char) #x2612)
      (eq (char-downcase char) #\b)))

(defun icon-wall-p (char)
  (or (= (char-code char) #x2593)
      (eq (char-downcase char) #\w)))

(defparameter *default-color* :white)

(defparameter *healing-range* 3)

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defun to-position (something)
  "Converts something (thing/position) to a position tuple."
  (if (listp something)
      something
      (thing-position something)))

(defun distance (a b)
  "Calculates distance between two positions or things."
  (destructuring-bind (x1 y1) (to-position a)
    (destructuring-bind (x2 y2) (to-position b)
      (sqrt (+ (expt (abs (- x1 x2)) 2) (expt (abs (- y1 y2)) 2))))))

(defun sort-by-distance (something others)
  (sort (copy-list others) (lambda (c d) (< (distance something c)
                                            (distance something d)))))

(defun closest (something others)
  "Returns the closest other to something (things/positions)."
  (if others
      (car (sort-by-distance something others))))

(defun adjacent-positions (something)
  "Calculates the 4 adjacent positions of something (thing/position)."
  (let ((position (to-position something))
        (deltas '((0 1) (0 -1) (1 0) (-1 0))))
    (loop :for delta :in deltas
          :collect (list (+ (car position) (car delta))
                         (+ (cadr position) (cadr delta))))))

(defun possible-moves (something things)
  "Calculates the possible moves for a thing."
  (remove-if (lambda (position) (gethash position things))
             (adjacent-positions something)))

(defstruct world
  size
  (debug t)
  (things (make-hash-table :test 'equal))
  (decoration (make-hash-table :test 'equal))
  (timestamp -1)
  events
  (deaths 0))

(defclass thing ()
  ((max-life
    :accessor max-life
    :allocation :class
    :initform 1)
   (name
    :accessor thing-name
    :initarg :thing-name)
   (icon
    :accessor thing-icon
    :initarg :thing-icon)
   (icon-basic
    :accessor thing-icon-basic
    :initarg :thing-icon-basic)
   (color
    :accessor thing-color
    :initarg :thing-color)
   (life
    :accessor thing-life
    :initarg :thing-life)
   (position
    :accessor thing-position
    :initarg :thing-position
    :initform nil)
   (status
    :accessor thing-status
    :initform "")
   (ask-for-actions
    :accessor thing-ask-for-actions
    :initarg :thing-ask-for-actions
    :initform nil)
   (dead-decoration
    :accessor thing-dead-decoration
    :initarg :thing-dead-decoration
    :initform nil)
   (is-decoration
    :accessor thing-is-decoration
    :initarg :thing-is-decoration
    :initform nil)))

(defmethod spawn-thing ((obj world) (thing thing))
  ;; "Add a thing to the world, or to the decoration layer.
  ;;  The thing will be spawned into the position it has in its position
  ;;  attribute."
  (if (thing-is-decoration thing)
      (setf (gethash (thing-position thing) (world-decoration obj)) thing)
      (let ((other (gethash (thing-position thing) (world-things obj))))
        (if (not other)
            (setf (gethash (thing-position thing) (world-things obj)) thing)
            (error "Can't place ~a in a position occupied by ~a."
                   (thing-name thing) other)))))

(defmethod event ((obj world) (thing thing) message)
  ;; "Log an event."
  (setf (world-events obj)
        (append (world-events obj)
                (list (list (world-timestamp obj)
                            thing
                            message)))))

(defmethod do-step ((obj world))
  (incf (world-timestamp obj))
  (let ((actions (get-actions obj)))
    (setf actions (alexandria:shuffle actions))
    (execute-actions obj actions)
    (clean-dead-things obj)))

(defmethod spawn-in-random ((obj world) things
                            &optional possible-positions (fail-if-cant t))
  ;; "Spawn a group of things  in random positions."
  (let (spawns)
    (if (not possible-positions)
      (setf spawns (loop :for i :below (first (world-size obj))
                         :with res = nil
                         :do (loop :for j :below (second (world-size obj))
                                   :do (push (list i j) res))
                         :finally (return (nreverse res))))
      (setf spawns (copy-list possible-positions)))

    ;; remove occupied positions, and shuffle
    (delete-if (lambda (spawn)
                 (gethash spawn (world-things obj)))
               spawns)
    (setf spawns (alexandria:shuffle spawns))

    ;; try to spawn each thing
    (loop :for thing :in things
          :do (if spawns
                  (progn
                    (setf (thing-position thing) (alexandria:lastcar spawns))
                    (setf spawns (butlast spawns))
                    (spawn-thing obj thing))
                  (if fail-if-cant
                      (error "Not enough space to spawn ~a" (thing-name thing))
                      (return-from spawn-in-random))))))

(defmethod get-actions ((obj world))
  ;; "For each thing, call its next_step to get its desired action."
  (let (actions
        (actors (remove-if (lambda (thing) (not (thing-ask-for-actions thing)))
                           (alexandria:hash-table-values (world-things obj)))))
    (loop :for thing :in actors
          :do (handler-bind
                  ((error (lambda (e)
                            (event obj thing (format nil "error with next_step: ~a~%" e))
                            (if (world-debug obj)
                                (error "Error: ~a~%" e)))))
                (let ((next-step (next-step thing  (world-things obj) (world-timestamp obj))))
                  (cond ((= (length next-step) 2)
                         (destructuring-bind (action parameter) next-step
                           (push (list thing action parameter) actions)))
                        ((null next-step)
                         (event obj thing "idle"))
                        (t (error "invalid next_step result: ~a~%" next-step))))))
    (nreverse actions)))

(defmethod execute-actions ((obj world) actions)
  ;; "Execute actions, and add their results as events."
  (loop :for (thing action parameter) :in actions
        :do (handler-bind ((error
                             (lambda (e)
                               (event obj thing
                                      (format nil "error executing ~a action: ~a" action e))
                               (if (world-debug obj)
                                   (error "Error ~a" e)))))
              (let (event)
                (setf event
                      (alexandria:switch (action :test #'string-equal)
                        ("move" (move obj thing parameter))
                        ("attack" (attack obj thing parameter))
                        ("heal" (heal obj thing parameter))))
                (event obj thing event)))))

(defmethod clean-dead-things ((obj world))
  ;; "Remove dead things, and add dead decorations."
  (let ((dead-things (remove-if (lambda (thing) (> (thing-life thing) 0))
                                (alexandria:hash-table-values (world-things obj)))))
    (loop :for thing :in dead-things
          :do (when (thing-dead-decoration thing)
                (setf (thing-position (thing-dead-decoration thing)) (thing-position thing))
                (spawn-thing obj (thing-dead-decoration thing)))
              (remhash (thing-position thing) (world-things obj))
              (event obj thing "died")
              (incf (world-deaths obj)))))

(defmethod move ((obj world) (thing thing) destination)
  ;; "Apply move action of a thing.
  ;; target: the position to go to."
  (unless(listp destination)
    (error "Destination of movement should be a tuple or list"))
  (let ((obstacle (gethash destination (world-things obj)))
        event)
    (cond (obstacle
           (setf event (format nil "hit ~a with his head"
                               (thing-name obstacle))))
          ((> (distance (thing-position thing) destination) 1)
           (setf event "tried to walk too fast, but physics forbade it"))
          (t
           ;; we store position in the things, because they need to know it,
           ;; but also in our dict, for faster access
           (setf (gethash destination (world-things obj)) thing)
           (remhash (thing-position thing) (world-things obj))
           (setf (thing-position thing) destination)
           (setf event (format nil "moved to ~a" destination))))
    event))

(defmethod attack ((obj world) (thing thing) target)
  ;; "Apply attack action of a thing.
  ;; target: the thing to attack."
  (unless (typep target 'thing)
    (error "Target of attack should be a thing"))
  (if (> (distance (thing-position thing) (thing-position target))
         (weapon-max-range (figthting-thing-weapon thing)))
      (format nil "tried to attack ~a, but it is too far for a ~a"
              (thing-name target) (weapon-name (figthting-thing-weapon thing)))
      (let* ((damage-range (weapon-damage-range (figthting-thing-weapon thing))))
        (decf (thing-life target)
              (destructuring-bind (start end) damage-range
                (random-from-range start end)))
        (format nil "injured ~a with a ~a"
                (thing-name target)
                (weapon-name (figthting-thing-weapon thing))))))

(defmethod heal ((obj world) (thing thing) target)
  ;; "Apply heal action of a thing.

  ;; target: the thing to heal."
  (unless (typep target 'thing)
    (error "Target of healing should be a thing"))

  (if (> (distance (thing-position thing) (thing-position target)) *healing-range*)
      (format nil "tried to heal ~a, but it is too far away" (thing-name target))
      (let* ((max-life (max-life target))
             (heal (random-from-range (/ max-life 10) (/ max-life 4))))
        (setf (thing-life target) (min max-life (+ heal (thing-life target))))
        (format nil "healed ~a" (thing-name target)))))

(defstruct weapon
  name max-range damage-range)

(load-time-value
 (progn

   (defconstant +zombie-claws+ (make-weapon :name "ZombieClaws"
                                         :max-range 1.5
                                         :damage-range (list 5 10)))

   (defconstant +knife+ (make-weapon :name "Knife"
                                     :max-range 1.5
                                     :damage-range (list 5 10)))

   (defconstant +axe+ (make-weapon :name "Axe"
                                   :max-range 1.5
                                   :damage-range (list 75 100)))

   (defconstant +gun+ (make-weapon :name "Gun"
                                   :max-range 6
                                   :damage-range (list 10 50)))

   (defconstant +rifle+ (make-weapon :name "Rifle"
                                     :max-range 10
                                     :damage-range (list 25 75)))

   (defconstant +shot-gun+ (make-weapon :name "shot-gun"
                                        :max-range 3
                                        :damage-range (list 75 100)))))

(defmethod next-step ((obj thing) things timestamp)
  nil)

(defclass box (thing)
  ((max-life
    :accessor max-life
    :allocation :class
    :initform 10)
   (icon
    :accessor icon
    :allocation :class
    :initform (code-char #x2612))
   (icon-basic
    :accessor icon-basic
    :allocation :class
    :initform #\@)))

(defmethod initialize-instance :after ((obj box) &key)
  (setf (slot-value obj 'name) "box")
  (setf (slot-value obj 'icon) (code-char #x2612))
  (setf (slot-value obj 'icon-basic) #\@)
  (setf (slot-value obj 'color) :yellow)
  (setf (slot-value obj 'life) 10))

(defclass wall (thing)
  ((icon
    :accessor icon
    :allocation :class
    :initform (code-char #x2593))
   (icon-basic
    :accessor icon-basic
    :allocation :class
    :initform #\#)
   (max-life
    :accessor max-life
    :allocation :class
    :initform 200)))

(defmethod initialize-instance :after ((obj wall) &key)
  (setf (slot-value obj 'name) "wall")
  (setf (slot-value obj 'icon) (code-char #x2593))
  (setf (slot-value obj 'icon-basic) #\#)
  (setf (slot-value obj 'color) :white)
  (setf (slot-value obj 'life) 200))

(defclass dead-body (thing)
  ((icon
    :accessor icon
    :allocation :class
    :initform (code-char #x2620))
   (icon-basic
    :accessor icon-basic
    :allocation :class
    :initform #\#)
   (name
    :initarg dead-body-name)
   (color
    :initarg dead-body-color)
   (position
    :initarg dead-body-position)))

(defmethod initialize-instance :after ((obj dead-body) &key)
  (setf (slot-value obj 'icon) (code-char #x2620))
  (setf (slot-value obj 'icon-basic) #\=)
  (setf (slot-value obj 'life) 0)
  (setf (slot-value obj 'is-decoration) t))

(defclass objective-location (thing)
  ((icon
    :accessor icon
    :allocation :class
    :initform (code-char #x2591))
   (icon-basic
    :accessor icon-basic
    :allocation :class
    :initform #\*)))

(defmethod initialize-instance :after ((obj objective-location) &key)
  (setf (slot-value obj 'name) "objective")
  (setf (slot-value obj 'icon) (code-char #x2591))
  (setf (slot-value obj 'icon-basic) #\*)
  (setf (slot-value obj 'color) :blue)
  (setf (slot-value obj 'life) 0)
  (setf (slot-value obj 'is-decoration) t))

(defclass figthting-thing (thing)
  ((ask-for-actions
    :initarg :thing-ask-for-actions
    :initform t)
   (weapon
    :accessor figthting-thing-weapon
    :initarg :figthting-thing-weapon)))

(defclass player (figthting-thing)
  ((icon
    :accessor icon
    :allocation :class
    :initform (code-char #x2A30))
   (icon-basic
    :accessor icon-basic
    :allocation :class
    :initform #\x)
   (max-life
    :accessor max-life
    :allocation :class
    :initform 100)
   (rules
    :accessor player-rules
    :initarg :player-rules)
   (objectives
    :accessor player-objectives
    :initarg :player-objectives)))

(defmethod initialize-instance :after ((obj player) &key)
  (setf (slot-value obj 'weapon)
        (alexandria:random-elt (list +gun+ +shot-gun+ +rifle+ +knife+ +axe+)))
  (setf (slot-value obj 'dead-decoration)
        (make-instance 'dead-body
                       :thing-name "dead"
                       :thing-color (slot-value obj 'color)))
  (setf (slot-value obj 'life) (slot-value obj 'max-life)))

(defclass zombie (figthting-thing)
  ((icon
    :accessor icon
    :allocation :class
    :initform (code-char #x2A30))
   (icon-basic
    :accessor icon-basic
    :allocation :class
    :initform #\x)
   (max-life
    :accessor max-life
    :allocation :class
    :initform 100)))

(defmethod initialize-instance :after ((obj zombie) &key)
  (setf (slot-value obj 'life) (random-from-range
                                (/ (slot-value obj 'max-life) 2)
                                (slot-value obj 'max-life)))
  (setf (slot-value obj 'name) "zombie")
  (setf (slot-value obj 'color) :green)
  (setf (slot-value obj 'weapon) +zombie-claws+)
  (setf (slot-value obj 'dead-decoration)
        (make-instance 'dead-body
                       :thing-name "zombie remains"
                       :thing-color :green)))

(defmethod next-step ((obj zombie) things timestamp)
  ;; "Zombies attack if in range, else move in direction of players."
  (let (action humans positions)
    (setf humans (remove-if (lambda (thing) (not (typep thing 'player)))
                            (alexandria:hash-table-values things)))
    (setf positions (possible-moves (thing-position obj) things))
    (if humans
        (let ((target (closest obj humans)))
          (if (< (distance (thing-position obj) (thing-position target))
                 (weapon-max-range (figthting-thing-weapon obj)))
              (setf action (list "attack" target))
              (if positions
                  (setf action (list "move" (closest target positions)))
                  (loop :for pos :in (sort-by-distance target (adjacent-positions obj))
                        :for thing = (gethash pos things)
                        :if (or (typep thing 'box) (typep thing 'wall))
                          :do (return-from next-step (list "attack" thing))))))
        (if positions
            (setf action (list "move" (alexandria:random-elt positions)))))
    action))

(defmacro create-player (name color weapon &body body)
  `(progn
     (defclass ,name (player) ())

     (defmethod next-step ((obj ,name) things timestamp)
       (progn ,@body))

     (setf (gethash (symbol-name ',name) *players*)
           (lambda (rules objectives)
             (make-instance ',name :thing-name ',name :player-rules rules
                                   :player-objectives objectives
                                   :thing-color ,color
                                   :figthting-thing-weapon (or ,weapon +gun+))))))

(defclass rules ()
  ((rules-game
    :accessor rules-game
    :initarg :rules-game
    :initform (error "Must supply an instance of a game"))))

(defmethod players-alive-p ((obj rules))
  (loop :for player in (game-players (slot-value obj 'rules-game))
        :do (if (> (thing-life player) 0)
                (return t))))

(defmethod game-ended-p ((obj rules))
  (not (players-alive-p obj)))

(defmethod game-won-p ((obj rules))
  (if (players-alive-p obj)
      ;; never should happen, but illustrative
      (list t "you won a game that never ends (?!)")
      (list nil "everybody is dead :(")))

(defmacro create-rules (name game-ended-body game-won-body)
  `(progn (defclass ,name (rules) ())
          (defmethod game-ended-p ((obj ,name))
            (progn ,@game-ended-body))

          (defmethod game-won-p ((obj ,name))
            (progn ,@game-won-body))
          (pushnew ',name *available-rules*)))

(create-rules extermination
              ;; A kind of game where players must exterminate all zombies.
              ;; Team wins when all zombies are dead.
              ((flet ((zombies-alive-p ()
                        (some (lambda (thing)
                                (typep thing 'zombie))
                              (alexandria:hash-table-values
                                   (world-things (game-world (rules-game obj)))))))
                 (or (not (players-alive-p obj)) (not (zombies-alive-p)))))

              ((if (players-alive-p obj)
                   (list t "zombies exterminated! :)")
                   (list nil "players exterminated! :("))))

(create-rules safe-house
              ;;A kind of game where players must get into a safe house.
              ;;Team wins when all alive players are inside the safe house.
              ((unless (zmap-objectives (game-map (rules-game obj)))
                 (error "Safe house game requires objectives defined."))

               (if (players-alive-p obj)
                   (every (lambda (player)
                            (find (thing-position player)
                                  (zmap-objectives (game-map (rules-game obj)))
                                  :test #'equal))
                          (remove-if (lambda (player)
                                       (<= (thing-life player) 0))
                                     (game-players (rules-game obj))))
                   t))

              ((if (players-alive-p obj)
                   (list t "everybody made it into the safehouse :)")
                   (list nil "nobody made it into the safehouse :("))))

;; TODO: implement 'evacuation

(defstruct zmap
  ;; game map
  size things player-spawns zombie-spawns objectives)

(defun load-map (file-path &optional (encoding :utf-8))
  "Import data from a utf-8 map file."
  (let (zombie-spawns
        player-spawns
        objectives
        things
        (max-row 0)
        (max-col 0)
        position
        lines)
    (with-open-file (stream file-path :external-format encoding :if-does-not-exist :error)
      (loop :for line = (read-line stream nil)
            :while line
            :do (push line lines))
      (loop :for line :in (nreverse lines)
            :for row-index :from 0
            :do (setf max-row row-index)
                (loop :for char :across line
                      :for col-index :from 0
                      :do (unless (= (char-code char) 32) ;;filtering out space chars
                              (setf max-col (max col-index max-col)))
                          (setf position (list col-index row-index))
                          (cond ((icon-box-p char)
                                 (push (make-instance 'box :thing-position position) things))
                                ((icon-wall-p char)
                                 (push (make-instance 'wall :thing-position position) things))
                                ((eq (char-downcase char) #\p)
                                 (push position player-spawns))
                                ((eq (char-downcase char) #\z)
                                 (push position zombie-spawns))
                                ((eq (char-downcase char) #\o)
                                 (push position objectives)
                                 (push (make-instance 'objective-location :thing-position position)
                                       position))
                                ((eq char (code-char 32))
                                 ;; do nothing
                                 )
                                (t (error "Unrecognized char: ~a" char))))))

    (make-zmap :size (list max-col max-row)
               :things (nreverse things)
               :player-spawns (nreverse player-spawns)
               :zombie-spawns (nreverse zombie-spawns)
               :objectives objectives)))
