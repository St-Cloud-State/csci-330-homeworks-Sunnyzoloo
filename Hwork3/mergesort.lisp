;; Define the partition function
(defun partition (lst)
  (labels ((split-helper (lst left right)
             (cond
               ((null lst) (list left right))  ;; If empty, return two empty lists
               ((null (cdr lst)) (list (cons (car lst) left) right))  ;; One element case
               (t (split-helper (cddr lst)   ;; Process two elements at a time
                                (cons (car lst) left)
                                (cons (cadr lst) right))))))  
    (split-helper lst '() '())))

;; Define the merge function
(defun merge-lists (left right)
  (cond
    ((null left) right)   ;; If left is empty, return right
    ((null right) left)   ;; If right is empty, return left
    ((<= (car left) (car right))  ;; Take smaller element
     (cons (car left) (merge-lists (cdr left) right)))
    (t (cons (car right) (merge-lists left (cdr right))))))  ;; Take from right list

;; Define the Mergesort function
(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst)))  ;; Base case: empty or one-element list
      lst
      (let* ((halves (partition lst))
             (left (mergesort (car halves)))
             (right (mergesort (cadr halves))))
        (merge-lists left right))))  ;; Fixed function call

;; Test the Mergesort function
(format t "Original List: ~a~%" '(2 1 8 7 6 8 4 3))
(format t "Sorted List: ~a~%" (mergesort '(2 1 8 7 6 8 4 3)))
