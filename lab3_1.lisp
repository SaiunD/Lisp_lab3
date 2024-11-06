(defun left-to-right (lst k L R &optional (first nil) (result nil) (count 0))
  "Сортування бульбашкою зліва направо"
  (cond
    ((= count (1+ R)) (left-to-right lst k L R first (append result (list first)) (1+ count)))
    ((null lst) (values result k)) ; повертаємо результат, якщо список закінчився
    ((< count L) (left-to-right (cdr lst) k L R first (append result (list (car lst))) (1+ count)))
    ((> count R) (left-to-right nil k L R first (append result lst) (1+ count)))
    ((null first)                       ; визначаємо перший елемент
     (left-to-right (cdr lst) k L R (car lst) result (1+ count)))
                                        ; логіка алгоритму
    ((> first (car lst)) 
     (left-to-right (cdr lst) (1- count) L R first (append result (list (car lst))) (1+ count)))
    (t 
     (left-to-right (cdr lst) k L R (car lst) (append result (list first)) (1+ count)))))

(defun right-to-left (lst k L R &optional (last nil) (result nil) (count (1- (length lst))))
  "Сортування бульбашкою справа наліво"
  (cond
    ((= count (1- L)) (if last
                          (right-to-left lst k L R nil (append (list last) result) (1- count))
                          (right-to-left lst k L R nil result (1- count))))
    ((null lst) (values result k)) ; повертаємо результат, якщо список закінчився
    ((> count R) (right-to-left (butlast lst) k L R last (append (last lst) result) (1- count)))
    ((< count L) (right-to-left nil k L R nil (append lst result) (1- count)))
    ((null last)                        ; визначаємо останній елемент
     (right-to-left (butlast lst) k L R (car (last lst)) result (1- count)))
                                        ; логіка алгоритму
    ((< last (car (last lst)))
     (right-to-left (butlast lst) count L R last (append (list (car (last lst))) result) (1- count)))
    (t 
     (right-to-left (butlast lst) k L R (car (last lst)) (append (list last) result) (1- count)))))

(defun shaker-sort-inner (lst L R k)
  "Реалізація сортування"
                                        ; перший цикл
  (multiple-value-bind (res k) (left-to-right lst k L R)
    (setq R k)
                                        ; другий цикл
    (if (< L R)
        (multiple-value-bind (res k) (right-to-left res k L R)
          (setq L (1+ k))
          ; перевіряємо умову для рекурсії
          (if (< L R)
              (shaker-sort-inner res L R k)
              res))
        res)))

(defun shaker-sort (lst)
  (if lst
      (shaker-sort-inner lst 0 (1- (length lst)) 0)
      lst))

(defun check-shaker-sort (name input-lst expected) 
  "Execute `shaker-sort' on `input', compare result with `expected' and print comparison status" 
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (shaker-sort input-lst) expected) 
          name))

(defun test-shaker-sort ()
  (check-shaker-sort "test-1" '(3 5 2 6 1 8 4 7) '(1 2 3 4 5 6 7 8))
  (check-shaker-sort "test-2" '(6 5 4 3 2 1) '(1 2 3 4 5 6))
  (check-shaker-sort "test-3" '(1 2 3 4 5 6) '(1 2 3 4 5 6))
  (check-shaker-sort "test-4" '() '()))
