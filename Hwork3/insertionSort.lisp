(defun insert (element sorted)
  "Insert an element into a sorted list in ascending order."
  (cond
    ((null sorted) (list element)) ; Base case: insert into empty list
    ((<= element (first sorted)) (cons element sorted)) ; Insert at the beginning
    (t (cons (first sorted) (insert element (rest sorted)))))) ; Recursive insertion

(defun insertion-sort (lst)
  "Sort a list using insertion sort."
  (if (null lst)
      nil ; Base case: empty list is already sorted
      (insert (first lst) (insertion-sort (rest lst))))) ; Recursive sorting

;; Example usage
(setq unsorted-list '(11 2 3 2 1 4 3 66))
(setq sorted-list (insertion-sort unsorted-list))

(format t "Unsorted: ~a~%" unsorted-list)
(format t "Sorted:   ~a~%" sorted-list)
