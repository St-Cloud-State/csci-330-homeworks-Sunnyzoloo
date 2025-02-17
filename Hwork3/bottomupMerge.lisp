(defun merge-sorted-lists (list1 list2)
  "Merge two sorted lists into one sorted list."
  (cond
    ((null list1) list2) ; If the first list is empty, return the second list.
    ((null list2) list1) ; If the second list is empty, return the first list.
    ((<= (car list1) (car list2)) 
     (cons (car list1) (merge-sorted-lists (cdr list1) list2))) ; Take the smaller element and merge recursively.
    (t
     (cons (car list2) (merge-sorted-lists list1 (cdr list2)))))) ; Take the smaller element and merge recursively.

(defun pair-up (lst)
  "Partition the list into sorted pairs."
  (if (null lst)
      nil ; Base case: empty list returns nil.
      (let ((first (car lst))
            (second (cadr lst))) ; Get the first and second elements.
        (if second
            (cons (merge-sorted-lists (list first) (list second))
                  (pair-up (cddr lst))) ; Merge pairs and recursively process the rest of the list.
            (list (list first)))))) ; If an odd element is left alone, wrap it in a list.

(defun merge-pass (lst)
  "Perform one pass of merging adjacent lists."
  (if (null lst)
      nil ; Base case: return nil for an empty list.
      (let ((first (car lst))
            (second (cadr lst))) ; Get two adjacent lists.
        (if second
            (cons (merge-sorted-lists first second) (merge-pass (cddr lst))) ; Merge pairs and process the rest.
            (list first))))) ; If a single list remains, return it.

(defun bottom-up-mergesort (lst)
  "Sort a list using bottom-up merge sort."
  (let ((sorted-pairs (pair-up lst))) ; Step 1: Create initial sorted pairs.
    (loop while (> (length sorted-pairs) 1) do
          (setf sorted-pairs (merge-pass sorted-pairs))) ; Keep merging until one sorted list remains.
    (car sorted-pairs))) ; Return the final sorted list.
