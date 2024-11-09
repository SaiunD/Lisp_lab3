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
CL-USER> (defun recur (c lst res1 i k L R)
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
RECUR
CL-USER> (defun shaker-sort-inner (lst k L R)
           (if (< L R)
               (multiple-value-bind (z new-lst new-k new-R) (recur (car lst) (cdr lst) nil 0 k L R)
                 (shaker-sort-inner new-lst new-k (1+ new-k) new-R))
               lst))
; in: DEFUN SHAKER-SORT-INNER
;     (MULTIPLE-VALUE-BIND (Z NEW-LST NEW-K NEW-R)
;         (RECUR (CAR LST) (CDR LST) NIL 0 K L R)
;       (SHAKER-SORT-INNER NEW-LST NEW-K (1+ NEW-K) NEW-R))
; --> MULTIPLE-VALUE-CALL 
; ==>
;   #'(LAMBDA (&OPTIONAL (Z) (NEW-LST) (NEW-K) (NEW-R) &REST #:G0)
;       (DECLARE (IGNORE #:G0))
;       (SHAKER-SORT-INNER NEW-LST NEW-K (1+ NEW-K) NEW-R))
; 
; caught STYLE-WARNING:
;   The variable Z is defined but never used.
; 
; compilation unit finished
;   caught 1 STYLE-WARNING condition
SHAKER-SORT-INNER
CL-USER> (defun shaker-sort (lst)
           (shaker-sort-inner lst 0 0 (1- (length lst))))
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
