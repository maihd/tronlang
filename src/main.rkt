#lang racket

(require "tronlang.rkt")

(define cmdarg-import-paths (make-parameter '()))
(define cmdarg-output-filename (make-parameter #false))

(define (compile-file in-file expect-out-file)
  (define out-file
    (if expect-out-file
        expect-out-file
        (let ((splitted (string-split in-file ".")))
          (string-join
           (if (equal? (last splitted) "tron")
               (list-set splitted (- (length splitted) 1) "js")
               (append splitted "js"))
           "."))))
  (with-output-to-file out-file #:exists 'replace
    (lambda ()
      (display "// Code generated from Tron Language\n")
      (display "\"use strict\";\n")
      (letrec
        ((exedir (let-values ([(result _ __) (split-path (find-system-path 'run-file))]) result))
         (import-paths (append (cmdarg-import-paths) (list (build-path exedir "lib"))))
         (loop
          (lambda (in)
            (let ([buffer (read in)])
              (if (eof-object? buffer)
                  (close-input-port in)
                  (begin
                    (tronlang buffer #:file-path in-file #:import-paths import-paths)
                    (display ";\n")
                    (loop in)))))))
        (tronlang '(import "std.tron") #:file-path (build-path exedir "lib/runtime.tron") #:import-paths import-paths)
        (loop (open-input-file in-file))))))

;; parse command line
(define-values (input-filename)
  (command-line #:program "Tron Language v1.0"
                #:multi
                [("-o" "--output") file "Output file." (cmdarg-output-filename file)]
                [("-I" "--import") path "Import path." (cmdarg-import-paths (append (cmdarg-import-paths) path))]
                #:args (input-filename)
                input-filename))

(compile-file input-filename (cmdarg-output-filename))