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
CL-USER> (defun shake-it (lst R i k)
           (if (and lst (cdr lst))
               (if (< i R)
                   (let* ((z1 (car lst))
                          (z2 (car (cdr lst)))
                          (cond1 (> z1 z2))
                          (z (if cond1 z2 z1))
                          (new-lst (if cond1 (cons z1 (cdr (cdr lst))) (cdr lst)))
                          (new-k (if cond1 i k)))
                     (multiple-value-bind (temp-lst bind-k bind-R) (shake-it new-lst R (1+ i) new-k)
                       (if (> i bind-R)
                           (values (cons z temp-lst) bind-k bind-R)
                           (let* ((cond2 (and temp-lst (<= z (car temp-lst))))
                                  (back-k (if cond2 bind-k i)))
                             (if cond2
                                 (values (cons z temp-lst) back-k bind-R)
                                 (values (cons (car temp-lst) (cons z (cdr temp-lst))) back-k bind-R))))))
                   (values lst k k))
               (values lst k k)))

SHAKE-IT
CL-USER> (defun left-side-apart (lst L &optional (i 0))
           (if (< i L)
               (multiple-value-bind (left rest) (left-side-apart (cdr lst) L (1+ i))
                 (values (cons (car lst) left) rest))
               (values nil lst)))
LEFT-SIDE-APART
CL-USER> (defun shaker-sort-inner (lst L R k)
           (if (< L R)
               (multiple-value-bind (left-side right-side) (left-side-apart lst L)
                 (multiple-value-bind (res res-k res-R) (shake-it right-side R L k)
                   (shaker-sort-inner (append left-side res) (1+ res-k) res-R res-k)))
               lst))
SHAKER-SORT-INNER
CL-USER> (defun shaker-sort (lst)
           (shaker-sort-inner lst 0 (1- (length lst)) 0))
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
