#|Zomsole game runner.

Usage:
    ./play.sh --help
    ./play.sh --list-rules
    ./play.sh --list-maps
    ./play.sh --rules RULES --players PLAYERS [-m MAP] [-s SIZE] [-z INITIAL_ZOMBIES] [-n MINIMUM_ZOMBIES] [-d] [-b] [-f MAX_FRAMES]

    Example:
    ./play.sh --rules extermination --players sniper,troll -n 50 -m to_the_closet -y
    ./play.sh --rules extermination --players sniper,troll -n 50 -m to_the_closet -w
    ./play.sh --rules extermination  --players sniper,troll -n 50 -m to_the_closet -y -w

    RULES:     Should be the name of a type of game. Use list_rules to see a complete list.
    PLAYERS:  Should be a list with the structure playerA,playerB,playerC,...
              You can also specify how much instances of each player, like this:
              playerA:3,playerB,playerC:10,...

Options:
    -h --help            Show this help.
    -m MAP               The map name to use (an empty world by default)
                         Use list_maps to list available maps.
    -s SIZE              The size of the world. Format: COLUMNSxROWS
    -z INITIAL_ZOMBIES   The initial amount of zombies [default: 0]
    -n MINIMUM_ZOMBIES   The minimum amount of zombies at all times [default: 0]
    -d                   Debug mode (lots of extra info, and step by step game play)
    -f MAX_FRAMES        Maximum frames per second [default: 2].
    -i                   Isolate the players process using docker, to prevent hacks to
                         the world (you will need docker installed for this to work,
                         and the isolator built and running. See the project docs for
                         more info).

--list-rules:
    Will list available game rules.

--list-maps:
    Will list available game maps.
|#


(ql:quickload :cl-zombsole)

;;; First of all, we need to define command line options. We do this with
;;; `define-opts' macro.

(opts:define-opts
  (:name :help
   :description "Show this help."
   :short #\h
   :long "help")
  (:name :rules
   :description "Should be the name of a type of game. Use list_rules to see a complete list."
   :long "rules"
   :arg-parser #'identity
   :meta-var "RULES")
  (:name :players
   :description "Should be a list with the structure playerA,playerB,playerC,...
              You can also specify how much instances of each player, like this:
              playerA:3,playerB,playerC:10,..."
   :arg-parser #'identity
   :long "players"
   :meta-var "PLAYERS")
  (:name :debug
   :description "Debug mode (lots of extra info, and step by step game play)"
   :short #\d)
  (:name :slynk
   :description "Spawn an Slynk server so we can connect and modify the game in live"
   :short #\y)
  (:name :swank
   :description "Spawn a Swank server so we can connect and modify the game in live"
   :short #\w)
  (:name :map
   :description "The map name to use (an empty world by default)"
   :short #\m
   :arg-parser #'identity
   :long "map"
   :meta-var "MAP")
  (:name :size
   :description "The size of the world. Format: COLUMNSxROWS"
   :arg-parser #'identity
   :short #\s
   :meta-var "SIZE")
  (:name :initial-zombies
   :description "The initial amount of zombies [default: 0]"
   :short #\z
   :arg-parser #'parse-integer
   :meta-var "INITIAL-ZOMBIES"
   :default 0)
  (:name :minimum-zombies
   :description "The minimum amount of zombies at all times [default: 0]"
   :short #\n
   :arg-parser #'parse-integer
   :meta-var "MINIMUN-ZOMBIES"
   :default 0)
  (:name :max-frames
   :description "Maximum frames per second [default: 2]."
   :short #\f
   :arg-parser #'parse-integer
   :meta-var "MAX-FRAMES"
   :default 2)
  (:name :basic-icons
   :description "Use basic icons if you have trouble with the normal icons."
   :short #\b)
  (:name :list-rules
   :description "Will list available game rules."
   :long "list-rules")
  (:name :list-maps
   :description "Will list available game maps."
   :long "list-maps"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(multiple-value-bind (options free-args)
    (handler-case
        (handler-bind ((opts:unknown-option #'unknown-option))
          (opts:get-opts))
      (opts:missing-arg (condition)
        (format t "fatal: option ~s needs an argument!~%"
                (opts:option condition)))
      (opts:arg-parser-failed (condition)
        (format t "fatal: cannot parse ~s as argument of ~s~%"
                (opts:raw-arg condition)
                (opts:option condition)))
      (opts:missing-required-option (con)
        (format t "fatal: ~a~%" con)
        (opts:exit 1)))

  (when-option (options :help)
               (opts:describe
                :prefix "Zombsole game runner"
                :suffix "Options for run the game Zombsole"
                :usage-of "play"
                :args     "GAME PLAYERS"))

  (when-option (options :list-rules)
               (format t "These are the rules: ~a ~%"
                       cl-zombsole::*available-rules*))

  (when-option (options :list-maps)
               (format t "These are the maps: ~a ~%"
                       (uiop:directory-files
                        (asdf:system-relative-pathname :cl-zombsole "maps/"))))

  (when free-args
    (warn (format nil "free args found, ~{~a~^, ~}~%" free-args)))

  (unless (some (lambda (c) (member c options :test #'eq))
                '(:help :list-rules :list-maps))
    (cl-zombsole::play-game options)))
