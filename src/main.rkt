#lang racket

(require "tronlang.rkt")

(define (compile-file in-file expect-out-file)
  ;;
  (define out-file
    (if expect-out-file
        expect-out-file
        (let ((splitted (string-split in-file ".")))
          (string-join
           (if (equal? (last splitted) "tron")
               (list-set splitted (- (length splitted) 1) "js")
               (append splitted "js"))
           "."))))

  ;; open compile file
  (with-output-to-file out-file #:exists 'replace
    (lambda ()
      (display "// Code generated from Tron Language\n")
      (display "\"use strict\";\n")
      (display "(function(){")
      (letrec
        ((loop
          (lambda (in)
            (let ([buffer (read in)])
              (if (eof-object? buffer)
                  (close-input-port in)
                  (begin
                    (tronlang buffer)
                    (display ";\n")
                    (loop in)))))))
        (let ((exedir (find-system-path 'orig-dir)))
          (loop (open-input-file (build-path exedir "lib/std.tron"))))
        (loop (open-input-file in-file)))
      (display "})();"))))

(define output-filename (make-parameter #false))

;; parse command line
(define-values (input-filename)
  (command-line #:program "Tron Language v1.0"
                #:multi
                [("-o" "--output") file "Output file." (output-filename file)]
                #:args (input-filename)
                input-filename))

(compile-file input-filename (output-filename))