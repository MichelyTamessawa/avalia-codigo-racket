#lang racket
(require rackunit)
(require rackunit/text-ui)

;; Lista -> Número
;; Conta a quantidade de elementos da lista que tem como substring a palavra "define"
(define (count-func file_lines)
  (count identity (map (λ (x) (string-contains? x "define"))file_lines)))

;; Lista -> Número
;; Conta a quantidade de elementos da lista vazios
(define (count-blank-lines file_lines)
  (count identity (map (λ (x) (equal? x "") )file_lines)))

;; Lista -> Número
;; Conta a quantidade de elementos da lista que tem como subtring o ";"
(define (count-comments file_lines)
  (count identity (map (λ (y)(string-contains? y ";"))file_lines)))

;; Lista Número -> Número
;; Como a lista é uma lista de listas, então é contado a qtde de ";" na primeira posição para cada elemento lista
(define (count-only-comment lst n)
  (cond [(empty? lst) n]
        [(empty? (first lst)) (count-only-comment (rest lst) n)]
        [(string-prefix? (list-ref (first lst) 0) ";")
         (count-only-comment (rest lst) (add1 n))]
        [else (count-only-comment (rest lst) n) ]))

;; Lista -> Lista
;; Produz uma lista com as métricas
(define (calc-metrics file_lines)
  (let ([qtde_lines (length file_lines)]
        [qtde_func (count-func file_lines)]
        [brank_line (count-blank-lines file_lines)]
        [comments (count-comments file_lines)]
        [only_comments (count-only-comment (map (λ (x) (string-split x)) file_lines) 0)])
    (list qtde_lines qtde_func brank_line comments only_comments (- comments only_comments))))

;; Lista -> Número
;; Cálcula o resultado da função objetiva através da avaliação das métricas
(define (func-obj lst)
  (/
   (+
   (* 15 (cond
            [(< (list-ref lst 0) (* (list-ref lst 1) 15)) 100]
            [(< (list-ref lst 0) (* (list-ref lst 1) 20)) 50]
            [else 0]))
   (* 30 (cond
            [(< (list-ref lst 1) 4) 100]
            [(< (list-ref lst 1) 8) 50]
            [else 0]))
   (* 10 (cond
            [(< (list-ref lst 2) (list-ref lst 1)) 0]
            [(< (list-ref lst 2) (* 2 (list-ref lst 1))) 100]
            [else 50]))
   (* 15 (cond
           [(< (list-ref lst 3) (* 2 (list-ref lst 1))) 0]
           [(< (list-ref lst 3) (* 4 (list-ref lst 1))) 100]
           [else 50]))
   (* 10 (cond
           [(< (list-ref lst 4) (* 2 (list-ref lst 1))) 0]
           [(< (list-ref lst 4) (* 3 (list-ref lst 1))) 100]
           [else 50]))
   (* 20 (cond
           [(< (list-ref lst 5) (list-ref lst 1)) 100]
           [(< (list-ref lst 5) (* 2 (list-ref lst 1))) 50]
           [else 0])))
     100.0))
  
;; Lista Lista -> void
;; Imprime os resultados das métricas e da função objetiva
(define (return-results lst_result files_names)
  (cond
    [(empty? lst_result) (void)]
    [else
     (printf "\n===================================================== \n")
     (printf "Arquivo: ~s \n" (first files_names))
     (printf "Resultado da avaliacao das metricas: \n")
     (printf "\t 1. Total de linhas: ~a \n" (list-ref (first lst_result) 0))
     (printf "\t 2. Nro de funcoes: ~a \n" (list-ref (first lst_result) 1))
     (printf "\t 3. Linhas em branco: ~a \n" (list-ref (first lst_result) 2))
     (printf "\t 4. Linhas com comentarios: ~a \n" (list-ref (first lst_result) 3))
     (printf "\t 5. Linhas so com comentarios: ~a \n" (list-ref (first lst_result) 4))
     (printf "\t 6. Linhas de codigo com comentarios: ~a \n" (list-ref (first lst_result) 5))
     (printf "Resultado da funcao objetiva: ~a\n" (func-obj (first lst_result)))
     (return-results (rest lst_result) (rest files_names))]))

;; String Lista -> void
;; Imprime informações do diretório: nome, tamanho e a lista dos nomes dos arquivos 
(define (print-dir-info dir_name files_names)
  (printf "\n===================================================== \n")
  (printf "\nNome do diretorio: ~s\n" dir_name)
  (printf "Qtde total de arquivos no diretorio: ~a\n" (length files_names))
  (printf "Lista de arquivos: ~s\n" files_names))

;; String -> void
;; Faz a leitura dos arquivos do diretório, realiza a chamada para o cálculo das métricas e da função objetiva
(define (avalia-diretorio dir_name)
  (let ([ files_names (map path->string (directory-list (string-append dir_name "/")))])
    (print-dir-info dir_name files_names)
    (let ([ result (map (λ (x) (calc-metrics (file->lines(string-append (string-append dir_name "/") x)))) files_names)])
      (return-results result files_names))))

;; String -> void
;; Faz a leitura de um arquivo, realiza a chamada para o cálculo das métricas e da função objetiva
(define (avalia-aquivo file-name)
  (return-results (list (calc-metrics(file->lines file-name))) (list file-name)))

;;================== Testes unitários ==================
(define count-func-tests
  (test-suite
   "count-func-tests"
   (check-equal? (count-func (list "a" "b" "c") ) 0)
   (check-equal? (count-func (list "define" "a" "b") ) 1)
   (check-equal? (count-func (list "define" "a" "adefinea") ) 2)))

(define count-blank-lines-tests
  (test-suite
   "count-blank-lines-tests"
   (check-equal? (count-blank-lines (list "a" "b" "c") ) 0)
   (check-equal? (count-blank-lines (list "" "a" "b") ) 1)
   (check-equal? (count-blank-lines (list "" "a" "") ) 2)))

(define count-comments-tests
  (test-suite
   "count-comments-tests"
   (check-equal? (count-comments (list "a" "b" "c") ) 0)
   (check-equal? (count-comments (list ";" "a" "b") ) 1)
   (check-equal? (count-comments (list ";" "a" ";a" "a;") ) 3)))

(define count-only-comment-tests
  (test-suite
   "count-only-comment-tests"
   (check-equal? (count-only-comment (list (list "a") (list "b") (list "c")) 0) 0)
   (check-equal? (count-only-comment (list (list ";") (list "a") (list "a")) 0) 1)
   (check-equal? (count-only-comment (list (list ";") (list "aa") (list ";aa" ";a")) 0) 2)))

(define calc-metrics-tests
  (test-suite
   "calc-metrics-tests"
   (check-equal?
    (calc-metrics
     (list 
      "(define (buscar-elemento lst a)"
      "(cond [(empty? lst) 'não foi encontrado']"
      "[else (cond [(equal? (first lst) a) 'foi encontrado']"
      "[else (buscar-elemento (rest lst) a)])]"))
    (list 4 1 0 0 0 0))
   (check-equal?
    (calc-metrics
     (list
      ""
      ";; Lista -> Número"
      ";; Insere o número no final da lista"
      "(define (cons-fim lst a)"
      "(cond"
      "[(empty? lst) (cons a lst)]"
      "[else (cons (first lst)"
      "(cons-fim (rest lst) a))])"))
    (list 8 1 1 2 2 0))
   (check-equal?
    (calc-metrics
     (list
      ";; Lista -> Número"
      ";; Buscar o elemento na lista e retorna:"
      ";; 'foi encontrado' se o elemento for encontrado e caso contrário, 'não foi encontrado'"
      "(define (buscar-elemento lst a)"
      "(cond"
      "[(empty? lst) 'não foi encontrado']"
      "[else (cond"
      "[(equal? (first lst) a) 'foi encontrado']"
      "[else (buscar-elemento (rest lst) a)])]))"
      ""
      ";; Lista -> Número"
      ";; Insere o número no final da lista"
      "(define (insere-final lst a)"
      "(cond"
      "[(empty? lst) (cons a lst)] ;; chegou no final da lista"
      "[else (cons (first lst) (insere-final (rest lst) a))])) ;; chamada recursiva"))
    (list 16 2 1 7 5 2))))

(define func-obj-tests
  (test-suite
   "func-obj-tests"
   (check-equal? (func-obj (list 4 1 0 0 0 0)) 65.0)
   (check-equal? (func-obj (list 8 1 1 2 2 0)) 100.0)
   (check-equal? (func-obj (list 16 2 1 7 5 2)) 80.0)))

;;============ Chamada dos testes Unitarios ============
;; Teste ... -> Void
;; Executa um conjunto de testes.
(define (executa-testes)
  (run-tests (test-suite "Teste do contador de funcoes" count-func-tests))
  (run-tests (test-suite "Teste do contador de linhas vazias" count-blank-lines-tests))
  (run-tests (test-suite "Teste do contador de linhas com comentarios" count-comments-tests))
  (run-tests (test-suite "Teste do contador de linhas somente com comentarios" count-only-comment-tests))
  (run-tests (test-suite "Teste do calculo da lista de metricas" calc-metrics-tests))
  (run-tests (test-suite "Teste da funcao objetiva" func-obj-tests))
  (void))

;;================= Execução dos testes =================
(printf "======================= Testes ======================= \n")
(executa-testes)

;;================== Execução do código =================
;(avalia-aquivo "permutation/1.rkt")
;(avalia-aquivo "permutation/4.rkt")
;(avalia-diretorio "permutation")