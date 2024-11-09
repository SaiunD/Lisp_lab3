(defun recur (c lst res1 i k L R)
  (if lst
      (let* ((cond1 (< i L))
             (cond2 (< c (car lst)))
             (new-c (if (or cond1 cond2) (car lst) c))
             (new-put (if (or cond1 cond2) c (car lst)))
             (new-k (if (or cond1 cond2) k i)))
        (multiple-value-bind (znach acc bind-k bind-R) (recur new-c (cdr lst) (cons new-put res1) (1+ i) new-k L R)
          (if res1
              (let* ((cond3 (<= (car res1) znach))
                     (cond4 (> i R))
                     (new-znach (if (or cond3 cond4) (car res1) znach))
                     (new-acc-put (if (or cond3 cond4) znach (car res1)))
                     (new-k-r (if cond3 bind-k (1- i))))
                (values new-znach (cons new-acc-put acc) new-k-r bind-R))
              (values nil (cons znach acc) bind-k bind-R)))
        )
      (let* ((cond5 (> c (car res1)))
             (new-z (if cond5 (car res1) c))
             (new-acc-p (if cond5 c (car res1))))
        (values new-z (list new-acc-p) k k))))

 (defun shaker-sort-inner (lst k L R)
           (if (< L R)
               (multiple-value-bind (z new-lst new-k new-R) (recur (car lst) (cdr lst) nil 0 k L R)
                 (shaker-sort-inner new-lst new-k (1+ new-k) new-R))
               lst))

(defun shaker-sort (lst)
  (shaker-sort-inner lst 0 0 (1- (length lst))))

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
