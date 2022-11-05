
;;;; defglobal.lisp -- Define "global lexical variables" in Common Lisp.

;;; Copyright (c) 2003-2007, 2011 Rob Warnock <rpw3@rpw3.org>.
;;; Lightly modified for cl-islisp by John Cowan <cowan@ccil.org>
;;; All Rights Reserved.
;;; 
;;; Permission to use, copy, modify, and/or distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;

;;; [Extracted from my personal "utils.lisp" (available someday, someday...)]

;;; DEFGLOBAL -- Define "global lexical variables", that is, top-level
;;; variables (convenient when debugging) that are lexical in scope,
;;; and thus don't pollute either the special or lexical variable spaces
;;; [except for the names of the "shadow" variables (c.f.), which are
;;; hopefully non-conflicting in most cases]. Thanks to the denizens
;;; of the "comp.lang.lisp" newsgroup for many useful discussions (and
;;; flames!) on this topic, and for the suggestion for the simple and
;;; efficient (albeit inelegant) "shadow" variable approach used here.
;;; [Note: Like several others, I had previously used a single global
;;; adjustable vector of shadow values, with complicated compile-time
;;; allocation of indices so that symbol-macro FOO expanded into something
;;; like this: (AREF *LEXICAL-STORE* (LOAD-TIME-VALUE {index-for-FOO})).
;;; But the following approach is much simpler and more maintainable.]
;;;
;;; 2005-06-12 -- Package bugfix thanks to Adam Warner <adam@consulting.net.nz>
;;;

(cl:in-package :islisp-impl)

(defmacro defglobal (var val &optional (doc nil docp))    
  "Define a top level (global) lexical VAR with initial value VAL,
  which is assigned unconditionally as with DEFPARAMETER. If a DOC
  string is provided, it is attached to both the name |VAR| and the
  name *STORAGE-FOR-DEFGLOBAL-VAR-|VAR|* as a documentation string of
  kind 'VARIABLE. The new VAR will have lexical scope and thus may be
  shadowed by LET bindings without affecting its dynamic (global) value."
  (let* ((s0 (symbol-name '#:*storage-for-defglobal-var-))
	 (s1 (symbol-name var))
	 (s2 (symbol-name '#:*))
	 (s3 (symbol-package var))	; BUGFIX [see above]
	 (backing-var (intern (concatenate 'string s0 s1 s2) s3)))
    ;; Note: The DEFINE-SYMBOL-MACRO must be the last thing we do so
    ;; that the value of the form is the symbol VAR.
    (if docp
      `(progn
	 (defparameter ,backing-var ,val ,doc)
	 (setf (documentation ',var 'variable) ,doc)
	 (define-symbol-macro ,var ,backing-var))
      `(progn
	 (defparameter ,backing-var ,val)
	 (define-symbol-macro ,var ,backing-var)))))
