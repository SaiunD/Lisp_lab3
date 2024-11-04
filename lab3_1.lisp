(defun left-to-right (lst k count &optional (first nil) (result nil))
  "Сортування бульбашкою зліва направо частини списку"
  (cond
    ((null lst) (values (append result (list first)) k)) ; повертаємо результат, якщо список закінчився
    ((null first) ; визначаємо перший елемент
     (left-to-right (cdr lst) k (1+ count) (car lst) result))
    ; логіка алгоритму
    ((> first (car lst)) 
     (left-to-right (cdr lst) (1- count) (1+ count) first (append result (list (car lst)))))
    (t 
     (left-to-right (cdr lst) k (1+ count) (car lst) (append result (list first))))))

(defun right-to-left (lst k count &optional (last nil) (result nil))
  "Сортування бульбашкою cправа наліво частини списку"
  (cond
    ((null lst) (values (append (list last) result) k)) ; повертаємо результат, якщо список закінчився
    ((null last) ; визначаємо останній елемент
     (right-to-left (butlast lst) k (1- count) (car (last lst)) result))
    ; логіка алгоритму
    ((< last (car (last lst)))
     (right-to-left (butlast lst) count (1- count) last (append (last lst) result)))
    (t 
     (right-to-left (butlast lst) k (1- count) (car (last lst)) (append (list last) result)))))

(defun left-side (lst L &optional (res nil) (count 0))
  "Створює ліву частину списку до границі L, яка вже відсортована"
  (if (> L count)
      (left-side (cdr lst) L (append res (list (car lst))) (1+ count))
      res))

(defun right-side (lst R &optional (res nil) (count (1- (length lst))))
  "Створює праву частину списку від гранці R до кінця списку, яка вже відсортована"
  (if (< R count)
      (right-side (butlast lst) R (append (last lst) res) (1- count))
      res))

(defun shaker-sort-inner (lst L R k)
  "Реалізація сортування"
  ; перший цикл
  (multiple-value-bind (res k) (left-to-right (subseq lst L (+ 1 R)) k L)
    (let* ((left (left-side lst L))
           (right (right-side lst R))
           (input (if (null (car res))
                      (append left right)  
                      (append left res right))))
      (setq R k)
      ; другий цикл
      (multiple-value-bind (res k) (right-to-left (subseq input L (+ 1 R)) k R)
        (let* ((left (left-side input L))
               (right (right-side input R))               
               (input (if (null (car res))
                          (append left right)  
                          (append left res right))))          
          (setq L (1+ k))
          ;; перевіряємо умову для рекурсії
          (if (< L R)
              (shaker-sort-inner input L R k)
              input))))))

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
