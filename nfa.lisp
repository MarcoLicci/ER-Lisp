;;REMEMBER RAIL RECURSION OPTIMIZATION
;;Checkout google guidelines

(defun nfa-regexp-comp (RE)
  ())

(defun re-compile (RE initial final)
  (if (not (listp RE))
      (re-c-sym RE initial final)
      (let ((op (car RE)))
        (cond ((eql op 'seq)
               (re-c-seq (cdr RE) initial final))
              ((eql op 'or)
               (re-c-or (cdr RE) initial final))
              ((eql op 'star)
               (re-c-star (cadr RE) initial final))
              ((eql op 'plus)
               (re-c-plus (cadr RE) initial final))
              (T (re-c-sym RE initial final))))))

;; TO DO REVIEW
(defun gen-state-name ()
  (gensym))

(defun re-c-sym (RE initial final)
  (list (list initial RE final)))

(defun re-c-seq (RE-list initial final)
  (if (cdr RE-list)
      (let ((internal-final (gen-state-name)))
        (append (re-compile (car RE-list) initial internal-final)
                (re-c-seq (cdr RE-list) internal-final final)))
      (re-compile (car RE-list) initial final)))

(defun re-c-or (RE-list initial final)
  (if (cdr RE-list)
      (append (re-c-or (list (car RE-list)) initial final)
              (re-c-or (cdr RE-list) initial final))
      (let ((internal-initial (gen-state-name))
            (internal-final (gen-state-name)))
        (append (list (list initial internal-initial)
                    (list internal-final final))
              (re-compile (car RE-list) internal-initial internal-final)))))

(defun re-c-plus (RE initial final)
  (let ((internal-initial (gen-state-name))
        (internal-final (gen-state-name)))
    (append (list (list initial internal-initial)
                  (list internal-final internal-initial)
                  (list internal-final final))
            (re-compile RE internal-initial internal-final))))

(defun re-c-star (RE initial final)
  (cons (list initial final)
        (re-c-plus RE initial final)))


;; (defun is-operator-p (x)) 

;;Naming convention?
;; (defun is-symbol-p (x) (or (not (listp x)) ()))
