; ===== HELPER FUNCTIONS =====
; ----- String functions -----
(define (string-split s delim)
  (let loop ((left (string->list s)) (parts '()) (part ""))
    (if (null? left)
      (reverse (cons part parts))
      (if (equal? (car left) delim)
        (loop (cdr left) (cons part parts) "")
        (loop (cdr left) parts (string-append part (string (car left))))))))

; Returns true if the string contains the given char.
(define (string-contains-char str c)
  (> (length (string-split str c)) 1))

; Returns true if the string if the given prefix is at the beginning
; of the given string.
(define (string-prefix? prefix str)
  (if (> (string-length prefix) (string-length str))
      #f
      (string=? prefix (substring str 0 (string-length prefix)))))

; Drops the first n characters of str.
(define (string-drop str n)
  (substring str n (string-length str)))

; ----- Aliases -----
(define *aliases* (make-hash))
(define (define-alias alias command)
  (set! *aliases* (hash-set *aliases* alias command)))

; ----- Triggers -----
(define *triggers* (make-hash))
(define (define-trigger pattern action)
  (set! *triggers* (hash-set *triggers* pattern action)))

; ----- GMCP commands -----
(define *gmcp-commands* (make-hash))
(define (define-gmcp command action)
  (set! *gmcp-commands* (hash-set *gmcp-commands* command action)))

; ----- Convenience functions -----
; Send input, optionally add to history, and write to scrollback buffer.
(define (full-send hist s)
  (if hist
    (composite (list (send s)
                     (add-to-history s)
                     (writeln (string-append "{y" s))))
    (composite (list (send s)
                     (writeln (string-append "{y" s))))))

; Print a message.
(define (message m)
  (println (string-append "{c=====[ " m " ]=====")))

; Function for reversing a direction.
(define (reverse-dir dir)
  (cond ((string=? dir "n") "s")
        ((string=? dir "s") "n")
        ((string=? dir "e") "w")
        ((string=? dir "w") "e")
        (else #f)))

; ===== SPECIAL COMMANDS =====
(define saved-path '())

; Function for handling special commands. Returns an action to perform.
(define (run-command cmd)
  (cond
    ; Start a new path.
    ((string=? cmd "path")
     (set! saved-path '())
     do-nothing)
    ; Add to the path.
    ((string=? cmd "addpath")
     (set! saved-path (cons "n" saved-path))
     (println "added"))
    ; Backtrack to the path start.
    ((string=? cmd "backtrack")
     (let ((backpath saved-path))
       (composite
         (list (message (string-append "Backtracking " (foldr string-append "" backpath)))
               (composite (map (curry full-send #f) (map reverse-dir backpath)))))))
    ; Quit
    ((string=? cmd "quit")
     quit)
    ; Reload config.
    ((string=? cmd "reload")
     reload-config)
    ; Eval.
    ((string-prefix? "eval " cmd)
     (letrec* ((to-eval (string-drop cmd 5))
               (result (eval (read (open-input-string to-eval)))))
              (println (string-append "> " to-eval (string #\newline)
                                      "{g" (string-repr result)))))
    ; Invalid command.
    (else
      (println (string-append "{rInvalid command: " cmd)))))

; ===== HOOKS ======
; Hook to run when the config file is loaded. Returns an action to perform.
(define (load-hook)
  (composite
    (list
      (bind "F5" (send-hook "test;commands"))
      (bind "F6" (send-hook "more;tests"))
      (bind "M-G" (scroll-lines -9999999))
      (bind "M-g" (scroll-lines 9999999))
      (bind "S-Up" (scroll-lines 1))
      (bind "S-Down" (scroll-lines -1))
      (bind "M-d" toggle-buffer)
      (bind "M-t" (send-gmcp "request room"))
      (bind "M-r" (composite (list reload-config
                                   (println "{bConfig reloaded.")))))))

; Returns the action to perform for the given input. If hist is true,
; add the input to the history.
(define (send-helper hist input)
  (cond
    ; Empty input.
    ((string=? input "")
     (send input))
    ; Multiple commands.
    ((string-contains-char input #\;)
     (composite (cons (if hist (add-to-history input) do-nothing)
                      (map (curry send-helper #f) (string-split input #\;)))))
    ; Aliases (recursive).
    ((hash-contains? *aliases* input)
     (composite (list 
                  (if hist (add-to-history input) do-nothing)
                  (send-helper #f (hash-get *aliases* input)))))
    ; Commands.
    ((string-prefix? "#" input)
     (composite (list
                  (if hist (add-to-history input) do-nothing)
                  (run-command (string-drop input 1)))))
    ; Search.
    ((string-prefix? "/" input)
     (composite (list
                  (if hist (add-to-history input) do-nothing)
                  (search-backwards (substring input 1 (string-length input))))))
    ; Everything else.
    (else
      (full-send hist input))))

; Hook to run when input is sent. Returns an action to perform.
(define (send-hook input)
  (send-helper #t input))

; Hook to run when GMCP is received.
(define (gmcp-hook iden data)
  (let ((out (println (string-append "{gGMCP:"
                                     iden
                                     (string #\newline)
                                     (string-repr data)))))
    (if (hash-contains? *gmcp-commands* iden)
        (composite (list out ((hash-get *gmcp-commands* iden) data)))
        out)))

; Hook to run when data is received.
(define (recv-hook data)
  (let ((lines (string-split data #\newline))
        (patterns (hash-keys *triggers*)))
    (composite
     (map (lambda (line)
            (composite
             (map (lambda (pattern)
                    (let ((action (hash-get *triggers* pattern))
                          (matches (string-matches line pattern)))
                      (if matches
                          (action matches)
                          do-nothing)))
                  patterns)))
          lines))))

; ===== MUD-SPECIFIC =====
(define-alias "test" "4n4e;world")
(define-trigger "Welcome"
  (lambda (m)
    (composite
     (list
      (println (string-append "{m===Triggered on " (car m) "==="))
      (send-helper #f "look")))))
(define-gmcp "room.info" (lambda (data) (println "{yROOM FOUND")))
