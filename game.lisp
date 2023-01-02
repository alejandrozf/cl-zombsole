(in-package #:cl-zombsole)

(defclass game ()
  ((players
    :accessor game-players
    :initarg :game-players
    :initform nil)
   (rules
    :accessor game-rules
    :initarg :game-rules
    :initform nil)
   (world
    :accessor game-world
    :initarg :game-world
    :initform nil)
   (map
    :accessor game-map
    :initarg :game-map)
   (minimum-zombies
    :accessor game-minimum-zombies
    :initarg :game-minimum-zombies
    :initform 0)
   (use-basic-icons
    :accessor game-use-basic-icons
    :initarg :game-use-basic-icons
    :initform nil)
   (debug
    :accessor game-debug
    :initarg :game-debug
    :initform nil)))

(defmethod initialize-instance :after ((obj game) &key initial-zombies player-names rules-name)
  ;; An instance of game controls the flow of the game.

  ;;  This includes player and zombies spawning, game main loop, deciding when
  ;;  to stop, importing map data, drawing each update, etc.
  (setf *rules* (make-instance
                 (find-symbol (string-upcase rules-name) 'cl-zombsole)
                 :rules-game obj))

  (setf *game* obj)

  (setf (slot-value obj 'rules) *rules*)

  (setf (slot-value obj 'world)
        (make-world :size (zmap-size (slot-value obj 'map))
                    :debug (slot-value obj 'debug)))

  (loop :for thing :in (zmap-things (slot-value obj 'map))
        :do (spawn-thing (slot-value obj 'world) thing))

  (if (> (hash-table-count *players*) 0)
      (setf (slot-value obj 'players)
            (let (result)
              (dolist (player-name player-names result)
                (let ((player-fun (gethash player-name *players*)))
                  (when player-fun
                    (push (funcall player-fun *rules* (zmap-objectives (slot-value obj 'map)))
                            result))))))
      (error "You must define at least one player"))

  (spawn-players obj)
  (spawn-zombies obj initial-zombies))

(defmethod spawn-players ((obj game))
  ;; Spawn players using the provided player create functions.
  (spawn-in-random (game-world obj) (game-players obj) (zmap-player-spawns (game-map obj))))

(defmethod spawn-zombies ((obj game) count)
  ;; Spawn N zombies in the world.
  (spawn-in-random (game-world obj)
                   (loop :for i :below count :collect (make-instance 'zombie))
                   (zmap-zombie-spawns (game-map obj))
                   nil))

(defun delimited-colored-string (string color)
  (concatenate 'string
               (cl-ansi-text:make-color-string color)
               string
               cl-ansi-text::+reset-color-string+))

(defmethod position-draw ((obj game) position)
  ;; Get the string to draw for a given position of the world.
  ;; decorations first, then things over them
  (let ((thing (or (gethash position (world-things (game-world obj)))
                   (gethash position (world-decoration (game-world obj)))))
        icon)
    (if thing
        (progn
          (if (game-use-basic-icons obj)
              (setf icon (thing-icon-basic thing))
              (setf icon (thing-icon thing)))
          (delimited-colored-string (format nil "~a" icon) (thing-color thing)))
        " ")))

(defmethod play ((obj game) &optional (frames-per-second 2.0))
  ;;Game main loop, ending in a game result with description.
  (loop
    (do-step (game-world obj))

    ;; maintain the flow of zombies if necessary
    (let ((zombies (remove-if (lambda (thing) (not (typep thing 'zombie)))
                              (alexandria:hash-table-values (world-things (game-world obj))))))
      (if (< (length zombies) (game-minimum-zombies obj))
          (spawn-zombies obj (- (game-minimum-zombies obj) (length zombies))))

      (draw obj)

      (if (game-debug obj)
          (break)
          (sleep (/ 1.0 frames-per-second)))

      (when (game-ended-p (game-rules obj))
        (destructuring-bind (won-p description)
            (game-won-p (game-rules obj))
          (print "")
          (if won-p
              (cl-ansi-text:with-color (:green)
                (princ "WIN!"))
              (cl-ansi-text:with-color (:red)
                (princ "GAME OVER")))
          (princ description)

          (return (list won-p description)))))))


(defun cls()
  ;; TODO: make it more general, this is *nix only
  (format t "~A[H~@*~A[J" #\escape))


(defmethod draw ((obj game))
  ;; Draws the world
  (let ((screen ""))

    (loop :for x :below (first (world-size (game-world obj)))
          :do (terpri)
              (loop :for y :below (second (world-size (game-world obj)))
                    :collect (format t "~a"
                                     (position-draw obj (list x y)))))

    (setf screen (format nil "~a ~%ticks: ~a deaths: ~a"
                         screen
                         (world-timestamp (game-world obj))
                         (world-deaths (game-world obj))))

    (loop :for player :in (sort (copy-list (game-players obj))
                                #'string< :key (lambda (p) (symbol-name (thing-name p))))
          :for weapon-name = (or (ignore-errors (weapon-name (figthting-thing-weapon player)))
                                 "unarmed")
          :do (let (life-bar player-stats)
                (if (> (thing-life player) 0)
                    (let* ((life-chars-count
                             (floor (* (/ 10.0 (max-life player)) (thing-life player))))
                           (life-chars (format nil "~v@{~A~:*~}" (code-char #x2588)))
                           (no-life-chars (format nil "~v@{~A~:*~}" (- 10 life-chars-count) (code-char #x2591))))
                      (setf life-bar (format nil "~a ~a~a" (code-char #x2665) life-chars no-life-chars)))
                    (setf life-bar (format nil "~a [dead]" (code-char #x2620))))

                (setf player-stats (format nil "~a ~a <~a ~a ~a>: ~a"
                                           life-bar
                                           (thing-name player)
                                           (thing-life player)
                                           (thing-position player)
                                           weapon-name
                                           (if (string-equal (thing-status player) "")
                                               "-"
                                               (thing-status player))))
                (setf screen (format nil "~a ~% ~a" screen
                                     (delimited-colored-string player-stats (thing-color player))))))
    (when (game-debug obj)
      (setf screen (format nil "~%~% ~a" screen))
      (loop :for (timestamp thing event) :in (world-events (game-world obj))
            :do (if (= timestamp (world-timestamp (game-world obj)))
                    (setf screen (delimited-colored-string
                                  (format nil "~a ~a ~a" (thing-name thing) event screen)
                                  (thing-color thing))))))
    (format t "~a~%" screen)
    (sleep 1)
    (cls)))
