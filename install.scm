#!/usr/bin/env gosh

(use file.util)
(use srfi-1)
(use srfi-37)

(define *ignore-directories* (map x->string
                                  '(.svn CVS RCS)))

(define (main args)
  (define (usage)
    (print #`"\t-b, --base=BASE\t\tBASE for install. (default \"\")")
    (print #`"\t-d, --destdir=DIR\tInstall files in DIR.")
    (print #`"\t\t\t\t(default \",(gauche-site-library-directory)\")")
    (print "\t-t, --test\t\tOnly show how to install. Don't install.")
    (print "\t-h, --help\t\tDisplay this help."))
  (define (bad-option message)
    (print message)
    (usage)
    (exit -1))
  (define options
    (list (option '(#\b "base") #t #t
                  (lambda (option name arg base dest-dir test? . others)
                    (unless arg
                      (bad-option #`"BASE is required for option ,|name|"))
                    (values arg dest-dir test?)))
          (option '(#\d "destdir") #t #t
                  (lambda (option name arg base dest-dir test? . others)
                    (unless arg
                      (bad-option #`"DIR is required for option ,|name|"))
                    (values base arg test?)))
          (option '(#\t "test") #f #f
                  (lambda (option name arg base dest-dir test? . others)
                    (values base dest-dir #t)))
          (option '(#\h "help") #f #f
                  (lambda (option name arg . others)
                    (usage)
                    (exit 0)))))
  (receive (base dest-dir test?)
      (args-fold (cdr args)
                 options
                 (lambda (option name arg . seeds) ; unrecognized
                   (bad-option #`"Unrecognized option: ,|name|"))
                 (lambda (operand test? dest-dir base) ; operand
                   (values test? dest-dir base))
                 ""
                 (gauche-site-library-directory)
                 #f)
    (install-directory "lib" (string-append base dest-dir) test?))
  0)

(define (install-directory from to test?)
  (directory-fold from
                  (lambda (file knil)
                    (let ((target (sys-dirname
                                   (string-scan file from 'after))))
                      (install-file file
                                    (string-append to target)
                                    test?)))
                  #t
                  :lister
                  (lambda (dir knil)
                    (let ((target (string-scan dir from 'after)))
                      (if (member (sys-basename target)
                                  *ignore-directories*
                                  string=?)
                          '()
                          (begin
                            (make-installed-directory
                             (string-join (list to target) "/")
                             test?)
                            (directory-list dir
                                            :children? #t
                                            :add-path? #t)))))))

(define (install-file file dir test?)
  (let ((target (string-join (list dir (sys-basename file))
                             "/")))
    (print #`"installing ,|file| => ,|target|")
    (if (not test?)
        (begin
          (copy-file file
                     target
                     :if-exists :supersede
                     :safe #t)
          (sys-chmod target #o644)))))

(define (make-installed-directory dir test?)
  (print #`"making installed directory ,|dir|")
  (if (not test?)
      (begin
        (make-directory* dir)
        (sys-chmod dir #o755))))
