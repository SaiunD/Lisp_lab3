 (defun shaker-sort-cycle (list)
   (let ((lst (copy-list list))
         (L 0)         
         (R (- (length list) 1))
         (k 0))
     (do () 
         ((>= L R) lst)
                                        ; проходимо зліва направо
       (do ((i L (+ 1 i)))              
           ((>= i R))
         (when (> (nth i lst) (nth (+ i 1) lst)) ; порівнюємо сусідні елементи
           (rotatef (nth i lst) (nth (+ i 1) lst)) ; обмін за допомогою rotatef
           (setf k i)))
       (setf R k)
      
                                        ; проходимо справа наліво
       (do ((i R (1- i)))              
           ((< i L))
         (when (> (nth i lst) (nth (+ i 1) lst)) ; порівнюємо сусідні елементи
           (rotatef (nth i lst) (nth (+ i 1) lst)) ; обмін за допомогою rotatef
           (setf k i)))
      
       (setf L (1+ k)))))

(defun check-shaker-sort-cycle (name input-lst expected) 
  "Execute `shaker-sort-cycle' on `input', compare result with `expected' and print comparison status" 
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (shaker-sort-cycle input-lst) expected) 
          name))

(defun test-shaker-sort-cycle ()
  (check-shaker-sort-cycle "test-1" '(3 5 2 6 1 8 4 7) '(1 2 3 4 5 6 7 8))
  (check-shaker-sort-cycle "test-2" '(6 5 4 3 2 1) '(1 2 3 4 5 6))
  (check-shaker-sort-cycle "test-3" '(1 2 3 4 5 6) '(1 2 3 4 5 6))
  (check-shaker-sort-cycle "test-4" '() '()))
