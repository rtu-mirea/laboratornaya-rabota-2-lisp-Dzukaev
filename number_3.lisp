;3 Сжатие/разжатие списка (см. примечания)
(defun comprsf (all i)
    (if (= (nth 1 i) (nth 1 (car all)))
        (append (list (list (+ 1 (nth 0 (car all))) (nth 1 i))) (cdr all))
        (append (list i) all)
    )
)
(defun mapf (i) 
    (list 1 i)
)
(defun unmapf (i) 
    (if (= (nth 0 i) 1)
        (nth 1 i)
        i
    )
)
;;Функция сжатия списка
(defun compress (lst)
    (map 'list #'unmapf
        (cdr
            (reverse
                (reduce
                    #'comprsf
                    (map 'list #'mapf lst)
                    :initial-value (list(list 1 -1))
                 )
             )
         )
    )
)

;; Функция расжатия списка
(defun decompress(lst)
  (loop for element in lst
    if (integerp element) collect element
    if (listp element) nconc (make-list (car element) 
  :initial-element (cadr element))))

(compress (list 7 7 7 7 0 1 0 2 2 2 0 3 3 3)) 
(decompress (list (list 4 3) 0 1 (list 6 0)))