#!/usr/bin/env sh
:; ( echo "$EMACS" | grep -q "term" ) && EMACS=emacs || EMACS=${EMACS:-emacs} # -*-emacs-lisp-*-
:; command -v $EMACS >/dev/null || { >&2 echo "Can't find emacs in your PATH"; exit 1; }
:; exec emacs -l ~/.emacs.d/init.el --script "$0" -- "$@"
:; exit 0
;; -*- lexical-binding: t -*-
;;; Code:

(defun alphabet (int)
  ""
  (nth int '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(defun sort-tree (width stage text)
  ""
  (if (equal stage 0)
      (progn
        (setq text (concat text "digraph {\n\n  {rank=same; a0 "))
        (dotimes (i width)
          (if (> i 0)
              (setq text (concat text (format "-> %s%d" (alphabet stage) i )))))
        (setq text (concat text " [style = invis]}\n"))
        ))

  (setq text (concat text (format "\n  //width=%d\n" width)))

  (cond

   ;; special case 1
   ((equal width 1)
    (setq text (concat text "}\n"))
    text)

   ;; special case 3
   ((equal width 3)
    (dotimes (i width)
      (setq text (concat text (format "  %s%d -> %s%d\n" (alphabet stage) i (alphabet (+ stage 1)) (/ i 3))))
      )
    (setq text (concat text "}\n"))
    text)

   ;; every other width
   (t (dotimes (i width)
        (setq text (concat text (format "  %s%d -> %s%d\n" (alphabet stage) i (alphabet (+ stage 1)) (/ i 2)))))
      (sort-tree (ceiling (/ width 2.0)) (+ stage 1) text)))
  )

;;(princ (sort-tree 8 0 ""))
(shell-command-to-string (format "echo '%s' > output.gv" (sort-tree 9 0 "")))
(shell-command-to-string (format "dot -Tpng -O output.gv"))
