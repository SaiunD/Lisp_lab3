<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/> 
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">
    <strong>Студентка</strong>: <em><strong>Саюн Дарина Миколаївна</strong></em>
</p>
<p align="right">
    <strong>Група</strong>: <em><strong>КВ-13</strong></em>
</p>
<p align="right">
    <strong>Рік</strong>: <em><strong>2024</strong></em>
</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно.
1. *Функціональний* варіант реалізації має базуватись на використанні рекурсії і конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку. Не допускається використання: псевдо-функцій, деструктивних операцій, циклів, функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. *Імперативний* варіант реалізації має базуватись на використанні циклів і деструктивних функцій (псевдофункцій). Не допускається використання функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Тим не менш, оригінальний список цей варіант реалізації також не має змінювати, тому перед виконанням деструктивних змін варто застосувати функцію copy-list (в разі необхідності). Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).

## Варівнт 8 (16)
Алгоритм сортування обміном №4 ("шейкерне сортування") за незменшенням.

## Лістинг функції з використанням конструктивного підходу
```lisp
CL-USER>  (defun left-to-right (lst k L R &optional (first nil) (result nil) (count 0))
           "Сортування бульбашкою зліва направо"
           (cond
             ((= count (1+ R)) (left-to-right lst k L R first (append result (list first)) (1+ count)))
             ((null lst) (values result k)) ; повертаємо результат, якщо список закінчився
             ((< count L) (left-to-right (cdr lst) k L R first (append result (list (car lst))) (1+ count)))
             ((> count R) (left-to-right nil k L R first (append result lst) (1+ count)))
             ((null first)              ; визначаємо перший елемент
              (left-to-right (cdr lst) k L R (car lst) result (1+ count)))
                                        ; логіка алгоритму
             ((> first (car lst)) 
              (left-to-right (cdr lst) (1- count) L R first (append result (list (car lst))) (1+ count)))
             (t 
              (left-to-right (cdr lst) k L R (car lst) (append result (list first)) (1+ count)))))
LEFT-TO-RIGHT
CL-USER> (defun right-to-left (lst k L R &optional (last nil) (result nil) (count (1- (length lst))))
           "Сортування бульбашкою справа наліво"
           (cond
             ((= count (1- L)) (if last
                                   (right-to-left lst k L R nil (append (list last) result) (1- count))
                                   (right-to-left lst k L R nil result (1- count))))
             ((null lst) (values result k)) ; повертаємо результат, якщо список закінчився
             ((> count R) (right-to-left (butlast lst) k L R last (append (last lst) result) (1- count)))
             ((< count L) (right-to-left nil k L R nil (append lst result) (1- count)))
             ((null last)               ; визначаємо останній елемент
              (right-to-left (butlast lst) k L R (car (last lst)) result (1- count)))
                                        ; логіка алгоритму
             ((< last (car (last lst)))
              (right-to-left (butlast lst) count L R last (append (list (car (last lst))) result) (1- count)))
             (t 
              (right-to-left (butlast lst) k L R (car (last lst)) (append (list last) result) (1- count)))))
RIGHT-TO-LEFT
CL-USER> (defun shaker-sort-inner (lst L R k)
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
SHAKER-SORT-INNER
CL-USER> (defun shaker-sort (lst)
           (if lst
               (shaker-sort-inner lst 0 (1- (length lst)) 0)
               lst))
SHAKER-SORT
```

### Тестові набори та утиліти
```lisp
CL-USER> (defun check-shaker-sort (name input-lst expected) 
           "Execute `shaker-sort' on `input', compare result with `expected' and print comparison status" 
           (format t "~:[FAILED~;passed~]... ~a~%" 
                   (equal (shaker-sort input-lst) expected) 
                   name))
CHECK-SHAKER-SORT
CL-USER> (defun test-shaker-sort ()
           (check-shaker-sort "test-1" '(3 5 2 6 1 8 4 7) '(1 2 3 4 5 6 7 8))
           (check-shaker-sort "test-2" '(6 5 4 3 2 1) '(1 2 3 4 5 6))
           (check-shaker-sort "test-3" '(1 2 3 4 5 6) '(1 2 3 4 5 6))
           (check-shaker-sort "test-4" '() '()))
TEST-SHAKER-SORT
```

### Тестування
```lisp
CL-USER> (test-shaker-sort)
passed... test-1
passed... test-2
passed... test-3
passed... test-4
NIL
```

## Лістинг функції з використанням деструктивного підходу
```lisp
CL-USER>  (defun shaker-sort-cycle (list)
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
SHAKER-SORT-CYCLE
```

### Тестові набори та утиліти
```lisp
CL-USER> (defun check-shaker-sort-cycle (name input-lst expected) 
           "Execute `shaker-sort-cycle' on `input', compare result with `expected' and print comparison status" 
           (format t "~:[FAILED~;passed~]... ~a~%" 
                   (equal (shaker-sort-cycle input-lst) expected) 
                   name))
CHECK-SHAKER-SORT-CYCLE
CL-USER> (defun test-shaker-sort-cycle ()
           (check-shaker-sort-cycle "test-1" '(3 5 2 6 1 8 4 7) '(1 2 3 4 5 6 7 8))
           (check-shaker-sort-cycle "test-2" '(6 5 4 3 2 1) '(1 2 3 4 5 6))
           (check-shaker-sort-cycle "test-3" '(1 2 3 4 5 6) '(1 2 3 4 5 6))
           (check-shaker-sort-cycle "test-4" '() '()))
TEST-SHAKER-SORT-CYCLE
```

### Тестування
```lisp
CL-USER> (test-shaker-sort-cycle)
passed... test-1
passed... test-2
passed... test-3
passed... test-4
NIL
```
