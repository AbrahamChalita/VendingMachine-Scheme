#lang racket
(define machine (open-input-file "/Users/ferra/Desktop/machine.txt"))
(define money (read machine))
(define slots (read machine))
(close-input-port machine)
(define transactions (open-input-file "/Users/ferra/Desktop/transacciones.txt"))
(define transacciones (read transactions))
(close-input-port transactions)


(define (check-inv prod lista)
  (if (null? lista) #f
      (if (equal? (caar lista) prod) (if (>= (caddr (car lista)) 1) #t #f)
          (check-inv prod (cdr lista))))
  )

(define (enough? trans slots)
  (if (null? slots) #f
      (if (equal? (caar slots) (car trans)) (if (enough-money? (car (cdr trans)) 0 (cadr (car slots))) #t #f)
          (enough? trans (cdr slots)))
  ))

(define (enough-money? sequence start target)
  (if (null? sequence) #f
      (if (>= (+ (car sequence) start) target) #t
          (enough-money? (cdr sequence) (+ (car sequence) start) target)))
  
  )

(define (check-transaction trans slots)
  (cond
    ((equal? (check-inv (car trans) slots) #f) (quote "No inventory"))
    ((equal? (enough? trans slots) #f) (quote "Not enough money"))
    (else (and (check-inv (car trans) slots) (enough? trans slots)) #t))
  )

(define (update-inv prod slots)
  (if (null? slots) '()
      (if (equal? (caar slots) prod) (append (list (cons (caar slots)
                                                   (cons (car (cdr (car slots)))
                                                   (cons (- (car (cdr (cdr (car slots)))) 1) null))))
                                                   (update-inv prod (cdr slots)))
          (append (list (cons (caar slots) (cons (car (cdr (car slots)))
                                     (cons (car (cdr (cdr (car slots)))) null))))
                                     (update-inv prod (cdr slots)))))
  )
; comienza función de filtrado de dinero
; Recibe secuencia de tipo '(2 6) ---> e.g de las monedas de 2, hay 6

(define (update-money seq money)
  (if (null? money) '()
      (if (equal? (car seq) (caar money)) (append(list (cons (caar money)
                                                 (cons (+ (car (cdr (car money))) (cadr seq)) null)))
                                                 (update-money seq (cdr money)))
          (append (list (cons (caar money) (cons (car (cdr (car money))) null)))
                  (update-money seq (cdr money)))))
  )

(define (encode lista)
  (if (null? lista) '()
      (cons
       (cons (car lista) (cons (counter (car lista) lista) null))(encode (elimina-ocurrencias (car lista) lista))))
  )

(define (elimina-ocurrencias value lista)
  (cond
    ((null? lista) '())
    ((equal? value (car lista)) (elimina-ocurrencias value (cdr lista)))
    (else (cons (car lista) (elimina-ocurrencias value (cdr lista)))))
  )

(define (counter value lista)
  (cond
    ((null? lista) 0)
    ((equal? value (car lista)) (+ 1 (counter value (cdr lista))))
    (else (counter value (cdr lista))))
  )

(define (last_element l)
  (cond ((null? (cdr l)) (car l))
        (else (last_element (cdr l)))))

(define (repeat-money seq money)
  (if (null? seq) '()
      (filter (lambda (x) (if (equal? x '()) #f #t))
              (cons (update-money (car seq) money)
                    (repeat-money (cdr seq) (update-money (car seq) money)))))
  )

(define final-money
  (lambda (x) (last_element x)))

(define (test slots monedas transacciones)
  (if (null? transacciones) '()
      (if (equal? (check-transaction (car transacciones) slots) #t) (checks slots monedas transacciones)
          (check-transaction (car transacciones) slots)))
  )

(define (checks slots monedas transacciones)
  (list "Transaction successful" (update-inv (caar transacciones) slots)
                                 (final-money (repeat-money (encode (cadar transacciones)) monedas))))

(define (allcheck slots monedas transacciones)
  (if (null? transacciones) '()
     (if (pair? (test slots monedas transacciones))
         (cons (test slots monedas transacciones)
               (allcheck (cadr (test slots monedas transacciones))
                         (caddr (test slots monedas transacciones))
                         (cdr transacciones)))
      (cons (list (test slots monedas transacciones)) (allcheck slots monedas (cdr transacciones)))))
  )


(define value 13)

(define testmoney '((1 24) (2 10) (5 14) (10 10) (20 5) (50 4)))


(define (change value money)
  (if (null? money) '()
      (if (= (quotient value (caar money)) 0) (change value (cdr money))
          (if (= (quotient value (caar money)) value) (cons (list (caar money) (quotient value (caar money))) (change value (cdr money)))
              (if (>= (quotient value (caar money)) 1) (cons (list (caar money) (quotient value (caar money))) (change (remainder value (caar money)) (cdr money)))
                  (change value (cdr money))
                  ))))) 
