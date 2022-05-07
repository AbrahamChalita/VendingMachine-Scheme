#lang racket
(display "Enter file name: ")
(define file (read-line))
(define machine (open-input-file "machine.txt"))
(define money (read machine))
(define slots (read machine))
(close-input-port machine)
(define transactions (open-input-file file))
(define transacciones (read transactions))
(close-input-port transactions)

; determines if the inserted sequence of coins does not traspass the established limit
(define (check_coins_limit trans money)
  (if (null? money) #f
      (if (equal? (caar money) (car trans)) (if (>= (+ (car (cdr (car money)))
                                                (car (cdr trans)))
                                                (car (cddr (car money)))) #f #t)
          (check_coins_limit trans (cdr money)))))
      
; determines if product (format 'A , 'B , etc) exists in the slots data
(define (contains list x)
	(cond [(null? list) #f]
		[(equal? (caar list) x) #t]
		[else (contains (cdr list) x)]))

; determines if there is enough
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

(define (known-coin? coin list)
  (if (null? list) #f
      (if (equal? coin (car list)) #t
          (known-coin? coin (cdr list))))
  )

(define all-true?
  (lambda (lst)
    (or (null? lst)
        (and (eq? (car lst) #t)
             (all-true? (cdr lst))))))

(define (check-transaction trans slots monedas)
  (cond
    ((equal? (contains slots (car trans)) #f) (quote "Unvalid product (Does not exist)"))
    ((equal? (check-inv (car trans) slots) #f) (quote "No inventory"))
    ((equal? (enough? trans slots) #f) (quote "Not enough money introduced"))
    ((equal? (all-true? (map (lambda (x) (known-coin? x (map car monedas))) (cadr trans))) #f) (quote "An unknown coin was introduced"))
    ((equal? (all-true? (map (lambda (x) (check_coins_limit x money)) (encode (car (cdr trans))))) #f) (quote "Ups, limit coin reached"))
    ((not (pair? (enough-change? trans slots monedas))) (enough-change? trans slots monedas))
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


(define (update-money-minus seq money)
  (if (null? money) '()
      (if (equal? (car seq) (caar money)) (append(list (cons (caar money)
                                                 (cons (- (car (cdr (car money))) (cadr seq)) null)))
                                                 (update-money-minus seq (cdr money)))
          (append (list (cons (caar money) (cons (car (cdr (car money))) null)))
                  (update-money-minus seq (cdr money)))))
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


(define (repeat-money-down seq money)
  (if (null? seq) '()
      (filter (lambda (x) (if (equal? x '()) #f #t))
              (cons (update-money-minus (car seq) money)
                    (repeat-money-down (cdr seq) (update-money-minus(car seq) money)))))
  )

(define final-money
  (lambda (x) (last_element x)))

(define (test slots monedas transacciones)
  (if (null? transacciones) '()
      (if (equal? (check-transaction (car transacciones) slots monedas) #t) (checks slots monedas transacciones)
          (check-transaction (car transacciones) slots monedas)))
  )

(define (checks slots monedas transacciones)
  (list "Transaction successful" (update-inv (caar transacciones) slots)
                                 (final-money (repeat-money (encode (cadar transacciones)) monedas))
                                 (if (pair? (car (enough-change? (car transacciones) slots monedas)))   
                                     (list "Change: " (enough-change? (car transacciones) slots monedas)
                                           "Updated money: "(final-money (repeat-money-down
                                                                          (enough-change? (car transacciones) slots monedas)
                                                                          (final-money (repeat-money (encode (cadar transacciones)) monedas)))))
                                     (enough-change? (car transacciones) slots monedas))))

(define (allcheck slots monedas transacciones)
  (if (null? transacciones) '()
     (if (pair? (test slots monedas transacciones))
         (if (equal? (car (last_element (test slots monedas transacciones))) "No change needed!")
             (cons (test slots monedas transacciones)
                   (allcheck (cadr (test slots monedas transacciones))
                             (caddr (test slots monedas transacciones))
                             (cdr transacciones)))
             (cons (test slots monedas transacciones)
                   (allcheck (cadr (test slots monedas transacciones))
                             (last_element (last_element (test slots monedas transacciones)))
                             (cdr transacciones))))
      (cons (list (test slots monedas transacciones)) (allcheck slots monedas (cdr transacciones)))))
  )


(define (change value money)
  (if (null? money) '()
      (if (= (quotient value (caar money)) 0) (change value (cdr money))
          (if (and (= (quotient value (caar money)) value) (>= (cadar money) (quotient value (caar money))))
              (cons (list (caar money) (quotient value (caar money)))
                    (change value (cdr money)))
              (if (and (>= (quotient value (caar money)) 1) (>= (cadar money) (quotient value (caar money))))
                  (cons (list (caar money) (quotient value (caar money)))
                        (change (remainder value (caar money)) (cdr money)))
                  (change value (cdr money))
                  )))))

; returns change if any (numeric value)
(define (change? trans slots)
  (if (null? slots) 0
      (if (equal? (car trans) (caar slots)) (- (apply + (cadr trans)) (cadar slots))
          (change? trans (cdr slots))))
  )

(define (sum-change change)
  (if (null? change) 0
      (+ (apply * (car change)) (sum-change (cdr change))))
  )

(define (enough-change? trans slots money)
  (cond
    ((equal? (change? trans slots) 0) (list "No change needed!"))
    ((empty? (change (change? trans slots) (reverse money))) (quote "No available change"))
    ((not (equal? (sum-change (change (change? trans slots) (reverse money))) (change? trans slots))) (quote "Not enough change available"))
    ((not (positive? (sum-change (change (change? trans slots) (reverse money))))) (quote "Error in calculation"))
    (else (change (change? trans slots) (reverse money))))
  )


; Results section

(define resultados (allcheck slots money transacciones))

(define (imbricada? lista) (if (null? lista) #f (if (pair? (car lista)) #t (imbricada? (cdr lista)))))

(define (get-last-valid-transaction results)
  (if (null? results) '()
      (if (imbricada? (car results)) (car results)
          (get-last-valid-transaction (cdr results))))
  )

(define new-money (last_element (last_element (get-last-valid-transaction (reverse resultados)))))
(define new-slot (cadr (get-last-valid-transaction (reverse resultados))))

(define (overallwins new old)
  (cond
    ((null? new) '())
    ((null? old) '())
    (else (cons (- (car (cdr (car new))) (car (cdr (car old)))) (overallwins (cdr new) (cdr old)))))
  )
  

(define (low-inv slots)
  (cond
    ((null? slots) '())
    ((if (or (and (<= (last_element (car slots)) 5) (>= (last_element (car slots)) 1)) (zero? (last_element (car slots))))
         (cons (list "Product: " (caar slots) "Q: "(last_element (car slots))) (low-inv (cdr slots)))
         (low-inv (cdr slots)))))
  )

(define (fullcoins? new old)
  (cond
    ((null? new) '())
    ((null? old) '())
    (else (if (or (and (< (car (cdr (car new))) (last_element (car old)))
                       (>= (car (cdr (car new))) (- (last_element (car old)) 10)))
                  (equal? (car (cdr (car new))) (last_element (car old))))
              (cons (list "Coin: " (caar new) "Inv: " (car (cdr (car new))) "Limit" (last_element (car old))) (fullcoins? (cdr new) (cdr old)))
              (fullcoins? (cdr new) (cdr old)))))
  )


(define (emptycoins? new)
  (cond
    ((null? new) '())
    (else (if (or (and (<= (car (cdr (car new))) 10)
                       (>= (car (cdr (car new))) 1))
                  (zero? (car (cdr (car new)))))
              (cons (list "Coin: " (caar new) "Inv: " (car (cdr (car new)))) (emptycoins? (cdr new)))
              (emptycoins? (cdr new)))))
  )

(define (review newmoney oldmoney newslots)
  (list
       "<------- Earnings ------->"(if (zero? (apply + (map (lambda (x) (if (positive? x) x 0)) (overallwins new-money money)))) "No earnings" (apply + (map (lambda (x) (if (positive? x) x 0)) (overallwins new-money money))))
       "<------- Inventory ------->" (if (empty? (low-inv newslots)) "No maintenance in inventory needed" (low-inv newslots))
       "<------- Full Coins ------->"(if (empty? (fullcoins? newmoney oldmoney)) "No full/almost full coins" (fullcoins? newmoney oldmoney))
       "<------- Empty Coins ------->" (if (empty? (emptycoins? newmoney)) "No empty/almost empty coins" (emptycoins? newmoney))
       ))

(define (filter-results resultados)
  (if (null? resultados) '()
      (if (imbricada? (car resultados)) (cons (list (caar resultados) (if (imbricada? (last_element (car resultados)))
                                                                    (car (cdr (last_element (car resultados))))
                                                                    (last_element (car resultados))))
                                              (filter-results (cdr resultados)))
          (cons (list (car resultados)) (filter-results (cdr resultados)))))
  )

(define (format-results resultados)
  (if (null? resultados) '()
      (if (null? (cdr (car resultados))) (cons (list (car (car resultados))) (format-results (cdr resultados)))
          (cons (list (car (car resultados)) "Change: " (last_element (car resultados))) (format-results (cdr resultados)))))
  )


(define (display-transactions lst)
  (for-each (lambda (what)
              (display (car what))
              (display "\n")
              (display (cdr what))
              (display "\n")
              (display "\n"))
            lst)
  (newline))

(define (display-review lst)
  (for-each (lambda (what)
              (display what)
              (display "\n")
              (display "\n"))
            lst)
  (newline))

(display "<---------- Transactions ---------->")
(display "\n")
(display-review (filter-results resultados))
(display "\n")
(display "\n")
(display "<$$$$$$$ Review $$$$$$$>")
(display "\n")
(display-review (review new-money money new-slot))
