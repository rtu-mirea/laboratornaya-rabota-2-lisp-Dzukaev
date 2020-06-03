;;поиск элемента по значению.
;;Примечание: список, пожалуйста, придумайте не (list 1 2 3 4 5 6 7 8), а какие-то
;;более "случайные" значения, которые хотя бы не совпадают с их индексами
;;2 Чтение файла и вывод его содержимого (файл текстовый)
;;3 Сжатие/разжатие списка (см. примечания)

;;задание 1-1
(defun insert (X N LST)
   (cond ((NULL LST) (cons X LST))
      ((eq N 1) (cons X LST))
      (T (cons (car LST)
            (insert X (- N 1) (CDR LST))
         )
      )
   )
)

(defun Del(N L)
    (cond ((eq N 1) (cdr L))
          (T (cons (car L)
            (Del (- N 1) (cdr L)))))
)
(defun newFindL(N L position)
    (cond ((eq L nil) "No element")
          ((= N (car L)) position)
          (T (newFindL N (cdr L) (+ position 1)))
))

(print (insert 3 4 '(4 7 1 2 5 12)))
(Del 2 (list 1 3 8 10))
(newFindL 8 (list 1 3 8 10) 0)

