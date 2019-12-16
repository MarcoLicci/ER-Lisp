;;REMEMBER RAIL RECURSION OPTIMIZATION
;;Checkout google guidelines

(defun nfa-regexp-comp (RE)
  ())

(defun re-compile (RE initial final)
  (if (not (listp RE))
      (re-c-sym (list RE) initial final)
      (let ((op (car RE)))
        (cond ((eql op 'seq)
               (re-c-seq (cdr RE) initial final))
              ((eql op 'or)
               (re-c-or (cdr RE) initial final))
              ((eql op 'star)
               (re-c-star (cdr RE) initial final))
              ((eql op 'plus)
               (re-c-plus (cdr RE) initial final))
              (T (re-c-sym RE initial final))))))

;; TO DO REVIEW
(defun gen-state-name ()
  (symbol-name (gensym "q")))

(defun re-c-sym (RE initial final)
  (list (list initial (car RE) final)))

(defun re-c-seq (RE initial final)
  (if (= (list-length RE) 1)
      (re-compile RE initial final)
      (let ((internal-final (gen-state-name)))
        (append (re-compile (car RE) initial internal-final)
              (re-c-seq (cdr RE) internal-final final)))))

(defun re-c-or (RE initial final)
  (if (= (list-length RE) 1)
      (let ((internal-initial (gen-state-name))
            (internal-final (gen-state-name)))
        (cons (list initial internal-initial)
              (cons (list internal-final final)
                    (re-compile (car RE) internal-initial internal-final))))
      (append (re-c-or (list (car RE)) initial final)
              (re-c-or (cdr RE) initial final))))


;; (defun is-operator-p (x)) 

;;Naming convention?
;; (defun is-symbol-p (x) (or (not (listp x)) ()))
