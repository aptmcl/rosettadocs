#lang racket
; From http://con.racket-lang.org/pr-slides.pdf
; by Prabhakar Ragde
(require scribble/html-properties
         scribble/latex-properties
         scribble/base
         (except-in scribble/core table)
         scribble/decode
         scribble/manual)
(require (for-syntax racket/list))
(require racket/runtime-path)
(require racket/system)
;(require (only-in slideshow bitmap))
(require (only-in pict bitmap scale scale-to-fit))
(require (for-label (except-in racket random box box?)))
(provide (for-label (all-from-out racket)))
(provide (all-from-out scribble/manual))
(provide (all-defined-out))

(require scribble/eval)
(require racket/sandbox)

#|
(require (for-label rosetta/autocad))
(provide (for-label (all-from-out rosetta/autocad)))
(define incremental-evaluator-requires (make-parameter '(rosetta/autocad)))
|#

(require (for-label rosetta/tikz))
(provide (for-label (all-from-out rosetta/tikz)))

(define incremental-evaluator-requires (make-parameter '(rosetta/tikz)))

(define mathjax-source
  "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  ;"http://c328740.r40.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=default"
  )

(define (tex-math . strs)
  (tex (string-append "$" (apply string-append strs) "$")))

(define (photo pathname)
  (scale-to-fit
   (bitmap pathname)
   600.0 500.0))

(define (tikz-image pathname)
  (image-element #f "image" pathname '() 2.0))

;;Heavily inspired by stchang slideshow-tex
(define-runtime-path tex-dir "rosetta-texfiles")

(define (get-filenames [str #f])
  (define preprefix "texfile")
  (define prefix
    (if str
        (string-append preprefix (number->string (equal-hash-code str)))
        (symbol->string (gensym preprefix))))
  (values prefix
          (string-append prefix ".tex")
          (string-append prefix ".pdf")
          (string-append prefix ".svg")))

(define (tex #:scale [scale 2] . strs)
  (define str (apply string-append strs))
  (define tex-str (string-append
                   "\\documentclass[a4paper,border=5pt,multi=false]{standalone}[2012/04/13]\n"
                   ;;"\\documentclass[a4paper]{standalone}\n"
                   "\\usepackage[noanswers]{racket}\n"
                   "\\usetikzlibrary{matrix}\n"
                   "\\def\\pause{\\relax}"
                   "\\def\\visible<#1>#2{#2}"
                   "\\colorlet{fg}{black}"
                   "\\colorlet{bg}{white}"
                   "\\begin{document}\n"
                   str "\n"
                   "\\end{document}\n"))
  (tex->image tex-str scale))


(define (tex->image str [scale 1.0])
  (define-values (prefix texfile pdffile svgfile) (get-filenames str))
  (define (dirfile file) (build-path tex-dir file))
  (if (file-exists? (dirfile svgfile))
      (image-element #f "image" (dirfile svgfile) '() scale)
      (begin
        (unless (directory-exists? tex-dir) (make-directory tex-dir))
        (with-output-to-file (dirfile texfile)
          (lambda () (display str))
          #:mode 'binary #:exists 'replace)
        (parameterize ((current-directory tex-dir))
          (system (format "pdflatex ~a" texfile)))
        (system (format "pdf2svg ~a ~a" (dirfile pdffile) (dirfile svgfile)))
        (for-each delete-file
           (list (dirfile (string-append prefix ".aux"))
                 (dirfile (string-append prefix ".log"))
                 (dirfile pdffile)
                 (dirfile texfile)))
        (image-element #f "image" (dirfile svgfile) '() scale))))

(define setup-math
  (paragraph
   (style
    #f (list (alt-tag "script")
             (attributes `((type . "text/javascript")
                           (src . ,mathjax-source )))))
   '()))

;; (define (mymath start end . strs)
;;   (make-element (make-style "relax" '(exact-chars)) `(,start ,@strs ,end)))

(define (mymath start end . strs)
  (make-element rosetta-style `(,start ,@strs ,end)))

(define (math-in . strs)
  (apply mymath "\\(" "\\)" strs))

(define (math-disp . strs)
  (apply mymath "\\[" "\\]" strs))

(define $ math-in)
(define $$ math-disp)


(define (lispf . strs)
  (apply tt strs))

(define-syntax-rule
  (lispemphcode code ...)
  (racketblock code ...))

(define-syntax-rule
  (lispcode code ...)
  (racketblock code ...))

(define (pascal . strs)
  (apply tt strs))

(define (chapter . strs)
  (apply section strs))

(define (footnote . strs)
  (apply margin-note strs))

;;TO BE FINISHED

(define questions-counter 0)

(define (questions . args)
  (set! questions-counter (+ 1 questions-counter))
  (cons (subsection #:tag (format "Questions~A" questions-counter)
                    (format "Exercises ~A" questions-counter))
        args))
;Śhould we use this?

#|

To summarize for the benefit of those who find this thread later:

A style of 'unnumbered does the trick. Thus

  @section[#:style 'unnumbered]{My Section}

will list it without giving it a number.

As for avoiding duplicate tags, this doesn't appear to be quite right:
 
If you go the latter route, you'll also need to use `#:tag-prefix` in a
cross-reference to a subsection (if you have any besides "Handin
Instructions") in one of the N sections.

It appears that the actual keyword argument is #:tag-prefixes [note extra "es"], and it takes a list of tags.

Thus, with only one prefix, a source document with

  @section[#:tag "uniq" #:tag-prefix "uniq"]{My Section}

can be referenced as

  (secref #:tag-prefixes (list "uniq") "uniq")

[But of course there's no reason for the tag and the tag-prefix to be the same string.]

And of course you can combine these:

  @section[#:style 'unnumbered #:tag "uniq" #:tag-prefix "uniq"]{My Section}

|#

(define question-counter 0)

(define (question #:tag [tag #f] . args)
  (set! question-counter (+ 1 question-counter))
  (cons (subsubsection #:tag (or tag (format "Question~A" question-counter))
                       (format "Question ~A" question-counter))
        args))

(define (answer . args)
  "THE ANSWER NEEDS TO BE FINISHED")

(define-syntax-rule
  (ref tag)
  (secref tag))

(define (verb . strs)
  (apply tt strs))

(define (ltxtable . strs)
  "THIS TABLE NEEDS TO BE FINISHED")

(define ldots "...")

(define-runtime-path extras.css "rosettaExtras.css")
(define-runtime-path extras.tex "rosettaExtras.tex")

(define rosetta-style
  (make-style "Rosetta"
              (list 'exact-chars
                    (make-css-addition extras.css)
                    (make-tex-addition extras.tex))))

(define incremental-evaluator
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f]
                 [sandbox-eval-limits '(#f #f)]
                 [sandbox-path-permissions '((execute "/")
                                             (read-bytecode "/"))]
#;                 [sandbox-gui-available #t]
#;                 [print-as-expression #f])
    (make-evaluator 'racket
                    #:requires (incremental-evaluator-requires))))

(interaction-eval
 #:eval incremental-evaluator
 (current-print pretty-print-handler))

#;
(interaction-eval
 #:eval incremental-evaluator
 (immediate-mode? #f))

(define-syntax-rule
  (incremental . args)
  (interaction #:eval incremental-evaluator . args))

(define-syntax-rule
  (def . args)
  (interaction/no-prompt #:eval incremental-evaluator . args))

(define-syntax-rule
  (def/no-show def ...)
  (interaction-eval #:eval incremental-evaluator (begin def ...)))

(define-syntax-rule
  (def/no-results . args)
  (racketblock+eval #:eval incremental-evaluator . args))

(define-syntax-rule
  (def/only-results def ...)
  (interaction-eval-show #:eval incremental-evaluator (begin def ...)))

(define-syntax (evaluation-steps stx)
  (syntax-case stx ()
    ((_ expr ...)
     (quasisyntax/loc stx
       (centered #,@(add-between (map (lambda (expr)
                                        #`(para (racket #,expr)))
                                      (syntax->list #'(expr ...)))
                                 #'(para (math-in "\\downarrow"))))))))


(define-syntax-rule
  (stx id)
  (racket id))

(define-syntax-rule
  (fn id)
  (racket id))

(define-syntax-rule
  (lit e)
  (racket e))

(define-syntax-rule
  (lisp expr ...)
  (racket expr ...))

(define-syntax-rule
  (lispemph expr)
  (italic (racket expr)))

(define-syntax-rule
  (lispemphi id sub)
  (italic (racket id) (subscript sub)))

(define (fig . body)
  (centered (apply elem body)))

(define (quotation . body)
  (nested #:style 'inset (apply elem body)))

(define tikz-style
  (make-style "Tikz" '(exact-chars)))

#;(define (tikz . body)
  (make-element tikz-style body))

;(require (prefix-in fig: scriblib/figure))

;;Implement the tag and caption for latex and find something suitable for html
(define (figure #:tag [tag #f] #:caption [caption #f] . body)
  ;(apply fig:figure tag caption body)
  (nested
   (margin-note caption)
   (centered
    (if tag
        (apply elemtag tag body)
        (apply elem body)))))

(define (table #:tag [tag #f] #:caption [caption #f] body)
  (nested
   (margin-note caption)
   (centered
    (if tag
        (list (make-target-element #f "" `(elem ,tag))
              body)
        (make-element #f body)))))

(define-runtime-path authorized-photos-folder
  (build-path 'up 'up "Teaching" "ElementosPedagogicos" "Arquitectura" "authorizedPhotos")
  #;
  (build-path 'up "authorizedPhotos"))


;;HACK: what should we do with the scale?
(define (authorizedPhoto #:scale [scale 1] name)
  (let ((filename (build-path authorized-photos-folder name)))
    (or (for/or ((suffix (in-list '(".png" ".jpg" ".jpeg"))))
          (let ((full-name (path-add-suffix filename suffix)))            
            (and (file-exists? full-name)
                 (photo full-name))))
        (error "Couldn't find photo" filename))))

;;HACK: this could load the render directly but we need to preprocess it first
(define (autoimage #:scale [scale 0.9] name)
  (tex (format "\\autoimage[~a]{~a}\n" scale name)))


;; \newcommand{\autoimage}[2][0.9]{%
;;   \pgfimage[width=#1\textwidth]{/home/aml/Dropbox/Renders/\imagesFolderSuffix/#2}}

;; \newcommand{\screenshot}[2][0.9]{%
;;   \pgfimage[width=#1\textwidth]{/home/aml/Dropbox/Renders/screenShots/#2}}




(define (figref ref)
  (elemref ref "this figure"))

(define (Figref ref)
  (elemref ref "This figure"))

(define (tabref ref)
  (elemref ref "this table"))

(define (Tabref ref)
  (elemref ref "This table"))

(define (tikz . body)
  (tex (string-append
                   "\\begin{tikzpicture}\n"
                   (foldr string-append "" body) "\n"
                   "\\end{tikzpicture}\n")))

(define (def/tikz)
  (tikz (incremental-evaluator '(tikz-output))))

(define (show-tikz [scale 1.0] [thickness "thick"])
  (tex->image (string-append
               "\\documentclass[a4paper,border=5pt,multi=false]{standalone}\n"
               "\\usepackage{tikz}\n"
               "\\usetikzlibrary{matrix}\n"
               "\\usetikzlibrary{hobby}\n"
               "\\begin{document}\n"
               "\\begin{tikzpicture}[" thickness "]\n"
               (incremental-evaluator '(tikz-output))
               "\n"
               "\\end{tikzpicture}\n"
               "\\end{document}\n")
              scale))


(define (clear-tikz)
  (incremental-evaluator '(tikz-output))
  "")

;;To be finished
(define (hfill . nothing)
  "")

  
