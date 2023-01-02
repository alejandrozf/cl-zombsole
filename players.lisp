(in-package #:cl-zombsole)

(create-player hamster :white nil
  (setf (slot-value obj 'status) "wii wi wiiii")
  (let ((moves (possible-moves obj things)))
    (if moves
        (list "move" (alexandria:random-elt moves)))))

(create-player randoman :yellow nil
  (let (target
        (action (alexandria:random-elt '("move" "attack" "heal"))))
    (if (find action '("attack" "heal"))
        (progn
          (setf (slot-value obj 'status) (format nil "~aing" action))
          (setf target (alexandria:random-elt (alexandria:hash-table-values things))))
        (progn
          (setf (slot-value obj 'status) "moving")
          (setf target (slot-value obj 'position))
          (incf (nth (random 2) target) (random-from-range -1 1))))
    (list action target)))

(create-player sniper :blue nil
    (let ((zombies (remove-if (lambda (thing) (not (typep thing 'zombie)))
                              (alexandria:hash-table-values things))))
      (if zombies
          (progn (setf (slot-value obj 'status) "shutting stuff")
                 (list "attack" (closest obj zombies)))
          (tagbody (setf (slot-value obj 'status) "waiting for targets")))))

(create-player troll :red nil
  (setf (slot-value obj 'status) "healing myself")
  (list "heal" obj))
