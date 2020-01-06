;; TODO
;; Guidelines Antoniotti e google
;; Testare su Lispworks
;; Commenti e README
;; Rimuovi ultimi TODO (gen-state...)

(defun nfa-test (FA input)
  (if (is-nfa-p FA)
      (if (listp input)
          (nfa-accept FA (car FA) nil input))
      (error "Error: ~S is not a Finite State Automata. " FA)))

(defun nfa-regexp-comp (RE)
  (if (is-regexp RE)
      (let ((initial (gen-state-name)) (final (gen-state-name)))
        (list initial (re-compile RE initial final) final))))

(defun is-nfa-p (x)
  (and (listp x)
       (= (list-length x) 3)
       (atom (first x))
       (atom (third x))
       (is-delta-list-p (second x))))

(defun is-delta-p (x)
  (and (listp x)
       (or (and (= (list-length x) 3)
                (atom (third x)))
           (and (= (list-length x) 2)
                (atom (second x))))
       (atom (first x))))
(defun is-delta-list-p (x)
  (or (null x)
      (and (listp x)
           (is-delta-p (car x))
           (is-delta-list-p (cdr x)))))

(defun is-regexp (RE)
  (or (atom RE)
      (not (member (car RE) '(or seq plus star)))
      (and (member (car RE) '(or seq))
           (> (list-length (cdr RE)) 0)
           (is-regexp-list-p (cdr RE)))
      (and (member (car RE) '(plus star))
           (= (list-length (cdr RE)) 1)
           (is-regexp (cadr RE)))))

(defun is-regexp-list-p (RE-list)
  (and (is-regexp (car RE-list))
       (or (null (cdr RE-list))
           (is-regexp-list-p (cdr RE-list)))))

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


(defun nfa-accept (NFA state e-visit input)
  (or (and (eql state (third NFA)) (not input))
      (nfa-transition NFA state (second NFA) e-visit input)))


(defun nfa-transition (NFA state transitions e-visit input)
  (if transitions
      (or (let ((curr-transition (car transitions)))
            (if (eql (first curr-transition) state)
                (if (third curr-transition)
                    (if (equal (second curr-transition) (car input))
                        (nfa-accept
                         NFA (third curr-transition) nil (cdr input)))
                    (if (not (member (second curr-transition) e-visit))
                        (nfa-accept
                         NFA (second curr-transition)
                         (cons (second curr-transition) e-visit)
                         input)))))
          (nfa-transition NFA state (cdr transitions) e-visit input))))

