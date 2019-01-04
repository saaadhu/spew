#lang racket
(require racket/match)

(define (emit-asm frag)
  (display frag) (list))

(define (create-pseudo)
  (let [(inc-pseudo
         ((lambda()
           (let [(x 100)]
             (lambda () (set! x (+ x 1)) x)))))]
    (format "R~a" (inc-pseudo))))

(define (save-arg arg)
  (list 'move (create-pseudo) arg))

(define (move reg arg)
  (emit-asm (format "mov ~a, ~a" reg arg)))

(define (matcher pattern)
  (match pattern
    ['() (emit-asm ".size")]
    ['ret (emit-asm "ret") ]
    [(list 'ret arg) (list (save-arg arg) 'ret)]
    [(list 'move reg arg) (move reg arg)]))

(define (code-gen pattern)
  (let [(newpattern (matcher (car pattern)))]
    (unless (empty? newpattern)
      (code-gen (append newpattern (cdr pattern))))))

