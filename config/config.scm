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

; ----- Convenience functions -----
; Send input, add to history, and print to scrollback buffer.
(define (full-send s)
  (composite (list (send s)
                   next-history
                   (println (string-append "{y" s)))))

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
    ;((string=? cmd "reload") ; Reload the config file.
    ; (list (tome:reload-config)
    ;       (tome:write-scrollback "Reloaded the config file.\n")))

    ; Start a new path.
    ((string=? cmd "path")
     (set! saved-path '())
     next-history)
    ; Add to the path.
    ((string=? cmd "addpath")
     (set! saved-path (cons "n" saved-path))
     (composite (list (println "added") next-history)))
    ; Backtrack to the path start.
    ((string=? cmd "backtrack")
     (let ((backpath saved-path))
       (composite
         (list (message (string-append "Backtracking " (foldr string-append "" backpath)))
               (composite (map full-send (map reverse-dir backpath)))))))
    ; Invalid command.
    (else
      (println (string-append "{rInvalid command: " cmd)))))

; ===== HOOKS ======
; Hook to run when input is sent. Returns an action to perform.
(define (send-hook input)
  (cond
    ; Empty input.
    ((string=? input "")
     (full-send input))
    ; Multiple commands.
    ((string-contains-char input #\;)
     (composite (map send-hook (string-split input #\;))))
    ; Aliases (recursive).
    ((hash-contains? *aliases* input)
     (send-hook (hash-get *aliases* input)))
    ; Commands.
    ((string-prefix? "#" input)
     (run-command (string-drop input 1)))
    ; Search.
    ;((string-prefix? "/" input)
    ; (begin
    ;   (search-backwards (substring input 1))
    ;   '())) ; Note that an empty list is returned.
    ; Everything else.
    (else
      (full-send input))))

; Hook to run when data is received from the server. Returns an action
; to perform.
;(define (recv-hook data)
;  (println data))

; ===== MUD-SPECIFIC =====
(define-alias "test" "4n4e;world")
