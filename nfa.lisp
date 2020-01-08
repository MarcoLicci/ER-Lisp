;; TODO
;; Guidelines Antoniotti e google
;; Testare su Lispworks
;; Commenti e README
;; Rimuovi ultimi TODO (gen-state...)

(defun nfa-test (FA input)
  (if (is-nfa-p FA)
      (if (listp input)
          (nfa-accept FA (car FA) input nil))
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
  (if (equal RE 'epsilon)
      (list (list initial final))
      (list (list initial RE final))))

(defun re-c-seq (RE-list initial final)
  (if (cdr RE-list)
      (let ((int-final (gen-state-name)))
        (append (re-compile (car RE-list) initial int-final)
                (re-c-seq (cdr RE-list) int-final final)))
      (re-compile (car RE-list) initial final)))

(defun re-c-or (RE-list initial final)
  (if (cdr RE-list)
      (append (re-c-or (list (car RE-list)) initial final)
              (re-c-or (cdr RE-list) initial final))
      (let ((int-initial (gen-state-name))
            (int-final (gen-state-name)))
        (append (list (list initial int-initial)
                      (list int-final final))
                (re-compile (car RE-list) int-initial int-final)))))

(defun re-c-plus (RE initial final)
  (let ((int-initial (gen-state-name))
        (int-final (gen-state-name)))
    (append (list (list initial int-initial)
                  (list int-final int-initial)
                  (list int-final final))
            (re-compile RE int-initial int-final))))

(defun re-c-star (RE initial final)
  (cons (list initial final)
        (re-c-plus RE initial final)))


(defun nfa-accept (NFA state input visited)
  (or (and (eql state (third NFA)) (null input))
      (nfa-accept-transitions
       NFA state (second NFA) input visited)))

(defun nfa-accept-transitions (NFA state transitions input visited)
  (if transitions
      (or (let ((tr (car transitions)))
            (if (eql (first tr) state)
                (if (third tr)
                    (if (equal (second tr) (first input))
                        (nfa-accept
                         NFA (third tr) (cdr input) nil))
                    (if (not (member (second tr) visited))
                        (nfa-accept
                         NFA (second tr)
                         input
                         (cons (second tr) visited))))))
          (nfa-accept-transitions
           NFA state (cdr transitions) input visited))))

