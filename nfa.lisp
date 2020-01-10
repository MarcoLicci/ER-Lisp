;; Licci Marco 844774

;;;; Funzioni di validazione

;; Verifica se la lista fornita e' una  espressione regolare,
;; per gli operatori or seq e' richiesto un numbero di argomenti
;; maggiore di 0, per star e plus esattamente uno.
(defun is-regexp (RE)
  (or (atom RE)
      (not (member (car RE) '(or seq plus star)))
      (and (member (car RE) '(or seq))
           (> (list-length (cdr RE)) 0)
           (every #'is-regexp (cdr RE)))
      (and (member (car RE) '(plus star))
           (= (list-length (cdr RE)) 1)
           (is-regexp (cadr RE)))))

;; Verifica che la lista fornita rispetti la struttura di un NFA
;; generato dalla funzione nfa-regex-comp
(defun is-nfa-p (x)
  (and (listp x)
       (= (list-length x) 3)
       (atom (first x))
       (atom (third x))
       (every #'is-delta-p (second x))))

;; Ritorna vero se una lista e' composta da 2 (epsilon transizioni)
;; o 3 (normale transizione) elementi e se primo ed ultimo
;; elemento sono atomi
(defun is-delta-p (x)
  (and (listp x)
       (or (and (= (list-length x) 3)
                (atom (third x)))
           (and (= (list-length x) 2)
                (atom (second x))))
       (atom (first x))))

;;;; Compilazione
;;;;
;;;; Funzioni di compilazione basate
;;;; sull'algoritmo di Thompson

;; Compila un'espressione regolare in un automa NFA,
;; cioe' una lista di lunghezza 3 composta da:
;; - (first NFA) -> stato iniziale
;; - (second NFA) -> lista di transizioni
;; - (third NFA) -> stato finale, accettazione
(defun nfa-regexp-comp (RE)
  (if (is-regexp RE)
      (let ((initial (gen-state)) (final (gen-state)))
        (list initial (re-compile RE initial final) final))))

;; Compila la lista di transizioni relativa alla RE passata
;; chiamando funzioni specifiche in base al primo elemento
;; della lista (operatore)
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

;; Gli stati sono rappresentati da simboli
;; generati progressivamente
(defun gen-state ()
  (gensym "q"))

;; Simboli
;; Ritorna una lista formata da una transizione elementare per il
;; riconoscimento di un simbolo. Nel caso il simbolo sia "epsilon",
;; viene generata una epsilon-transizione (2 elementi)
(defun re-c-sym (RE initial final)
  (if (equal RE 'epsilon)
      (list (list initial final))
      (list (list initial RE final))))

;; Operatore sequenza
;; L'operatore seq n-ario puo' essere definito sulla base del seq binario,
;; la funzione sfrutta tale proprieta' ritornando la concatenazione
;; delle transizioni ottenute dalla prima sottoespressione a quelle
;; relative ad un'espressione seq avente per argomenti il resto degli
;; argomenti iniziali
(defun re-c-seq (RE-list initial final)
  (if (cdr RE-list)
      (let ((int-final (gen-state)))
        (append (re-compile (car RE-list) initial int-final)
                (re-c-seq (cdr RE-list) int-final final)))
      (re-compile (car RE-list) initial final)))

;; Operatore or
;; Per ogni argomento genera le transizioni della sottoespressione
;; e collega gli stati iniziali e finali ai rispettivi interni ed esterni
;; attraverso epsilon transizioni
(defun re-c-or (RE-list initial final)
  (reduce #'append
          (mapcar (lambda (RE)
                    (let ((int-initial (gen-state))
                          (int-final (gen-state)))
                      (append (list (list initial int-initial)
                                    (list int-final final))
                              (re-compile RE int-initial int-final))))
                  RE-list)))

;; Operatore plus
;; Unisce alle transizioni dell'espressione interna quelle di 
;; collegamento tra stati iniziali interni ed esterni e la transizione
;; da iniziale interno ad iniziale esterno necessaria per il 
;; riconoscimento di ripetizioni
(defun re-c-plus (RE initial final)
  (let ((int-initial (gen-state))
        (int-final (gen-state)))
    (append (list (list initial int-initial)
                  (list int-final int-initial)
                  (list int-final final))
            (re-compile RE int-initial int-final))))

;; Operatore star
;; Basata sull'operatore plus aggiunge una transizione di
;; collegamento diretto tra stato iniziale e finale
(defun re-c-star (RE initial final)
  (cons (list initial final)
        (re-c-plus RE initial final)))


;;;; Test

;; Ritorna T se NFA e' valido e l'input e' una lista accettata
;; da nfa-accept, segnala un errore se NFA non e' un automa
(defun nfa-test (FA input)
  (if (is-nfa-p FA)
      (if (listp input)
          (nfa-accept FA (car FA) input nil))
      (error "Error: ~S is not a Finite State Automata. " FA)))

;; Accetta, ritorna T se lo stato corrente e' finale e l'input
;; e' una lista vuota oppure se e' possibile raggiungere lo stato
;; finale attraverso delle transizioni consumando l'intera lista
;; di input, cioe' se nfa-accept-transition ritorna true
;; 
(defun nfa-accept (NFA state input visited)
  (or (and (eql state (third NFA)) (null input))
      (nfa-accept-transitions
       NFA state (second NFA) input visited)))

;; Se sono presenti transizioni, consuma la prima verificando
;; che il relativo stato iniziale corrisponda allo stato attuale,
;; in caso contrario procede ricorsivamente sul resto della lista.
;; Se la transizione considerata e' una epsilon-transizione,
;; la funzione controlla che lo stato finale non sia gia' visitato,
;; se non e' una epsilon-transizione confronta il simbolo della
;; transizione con il primo in input. In caso di fallimento
;; della funzione nfa-accept, nfa-accept-transitions prova 
;; ricorsivamente le rimanenti
(defun nfa-accept-transitions (NFA state transitions input visited)
  (when transitions
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
