;;; plsense-direx.el --- Perl Package Explorer

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: perl, convenience
;; URL: https://github.com/aki2o/plsense-direx
;; Version: 0.1.0
;; Package-Requires: ((direx "0.1alpha") (plsense "0.3.2") (log4e "0.2.0") (yaxception "0.3.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; see <https://github.com/aki2o/plsense-direx/blob/master/README.md>

;;; Dependency:
;; 
;; - direx.el ( see <https://github.com/m2ym/direx-el> )
;; - plsense.el ( see <https://github.com/aki2o/emacs-plsense> )
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )
;; - log4e.el ( see <https://github.com/aki2o/log4e> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'plsense-direx)

;;; Configuration:
;; 
;; ;; Key Binding
;; (setq plsense-direx:open-explorer-key "C-x j")
;; (setq plsense-direx:open-explorer-other-window-key "C-x J")
;; 
;; (plsense-direx:config-default)

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "plsense-direx:[^:]" :docstring t)
;; `plsense-direx:open-explorer-key'
;; Keystroke for `plsense-direx:open-explorer'.
;; `plsense-direx:open-explorer-other-window-key'
;; Keystroke for `plsense-direx:open-explorer-other-window'.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "plsense-direx:[^:]" :docstring t)
;; `plsense-direx:open-explorer'
;; Open perl package explorer.
;; `plsense-direx:open-explorer-other-window'
;; Open perl package explorer in other window.
;; `plsense-direx:display-help'
;; Display help buffer about the current node/leaf.
;; `plsense-direx:update-current-package'
;; Update the package of current point.
;; `plsense-direx:setup-current-buffer'
;; Do setup for using plsense-direx in current buffer.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2013-08-22 on chindi02, modified by Debian
;; - direx.el ... Version 0.1alpha
;; - plsense.el ... Version 0.3.2
;; - yaxception.el ... Version 0.3.2
;; - log4e.el ... Version 0.2.0


;; Enjoy!!!


(require 'direx)
(require 'plsense)
(require 'dired)
(require 'log4e)
(require 'yaxception)

(defgroup plsense-direx nil
  "Perl Package Explorer."
  :group 'convenience
  :prefix "plsense-direx")

(defcustom plsense-direx:open-explorer-key nil
  "Keystroke for `plsense-direx:open-explorer'."
  :type 'string
  :group 'plsense-direx)

(defcustom plsense-direx:open-explorer-other-window-key nil
  "Keystroke for `plsense-direx:open-explorer-other-window'."
  :type 'string
  :group 'plsense-direx)

(defface plsense-direx:package-face
  '((t (:inherit dired-directory)))
  "Face for the package part of explorer."
  :group 'plsense-direx)

(defface plsense-direx:regular-method-face
  nil
  "Face for the regular method part of explorer."
  :group 'plsense-direx)


(log4e:deflogger "plsense-direx" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                           (error . "error")
                                                           (warn  . "warn")
                                                           (info  . "info")
                                                           (debug . "debug")
                                                           (trace . "trace")))
(plsense-direx--log-set-level 'trace)


;;;;;;;;;;;;;;;
;; Utilities

(defun* plsense-direx::show-message (msg &rest args)
  (apply 'message (concat "[PLSENSE-DIREX] " msg) args)
  nil)

(defsubst plsense-direx::get-root-package-name (pkgnm)
  (when (and pkgnm
             (string-match "\\`\\([^: ]+\\)" pkgnm))
    (match-string-no-properties 1 pkgnm)))

(defsubst plsense-direx::get-parent-package-name (pkgnm)
  (let ((ret (replace-regexp-in-string "::[^:]+\\'" "" pkgnm)))
    (when (and (not (string= ret ""))
               (not (string= ret pkgnm)))
      ret)))

(defsubst plsense-direx::build-address (pkgnm mtdnm)
  (cond ((and pkgnm mtdnm) (concat "&" pkgnm "::" mtdnm))
        (pkgnm             pkgnm)
        (mtdnm             (concat "&" mtdnm))))

(defsubst plsense-direx::pick-method-name (addr)
  (when (string-match "\\`&" addr)
    (replace-regexp-in-string "\\`&"
                              ""
                              (replace-regexp-in-string "::[^:]+\\'"
                                                        ""
                                                        addr))))

(defun plsense-direx::get-current-pkg/mtd ()
  (plsense-direx--trace "start get current pkg/mtd.")
  (let* ((locret (plsense--get-server-response "location" :waitsec 2 :ignore-done t))
         (pkgnm (when (string-match "^Module: \\([^\n]+\\)" locret)
                  (match-string-no-properties 1 locret)))
         (mtdnm (when (string-match "^Sub: \\([^\n]+\\)" locret)
                  (match-string-no-properties 1 locret))))
    (plsense-direx--trace "got current pkg/mtd. pkgnm[%s] mtdnm[%s]" pkgnm mtdnm)
    (list pkgnm mtdnm)))

(defun plsense-direx::select-package ()
  (plsense-direx--trace "start select package.")
  (let* ((readyret (plsense--get-server-response "ready" :waitsec 2 :ignore-done t))
         (pkgs (loop for s in (split-string readyret "\r?\n")
                     if (string-match "\\`[^ ]+\\'" s)
                     collect s)))
    (when pkgs
      (completing-read "Select package: " pkgs nil t nil nil nil))))


;;;;;;;;;;;;;
;; Explore

(defvar plsense-direx::hash-explore-cache (make-hash-table :test 'equal))
(defvar plsense-direx::use-root-cache nil)

(defun plsense-direx::clear-explore-cache (&optional pkgnm)
  (let ((reqpkgnm (if plsense-direx::use-root-cache
                      (plsense-direx::get-root-package-name pkgnm)
                    pkgnm)))
    (plsense-direx--trace "start clear explore cache of [%s]" reqpkgnm)
    (if reqpkgnm
        (remhash reqpkgnm plsense-direx::hash-explore-cache)
      (setq plsense-direx::hash-explore-cache (make-hash-table :test 'equal)))))

(defun plsense-direx::clear-explore-cache-by-regexp (regexp)
  (plsense-direx--trace "start clear explore cache by regexp : %s" regexp)
  (when regexp
    (loop for k being the hash-key in plsense-direx::hash-explore-cache
          if (string-match regexp k)
          do (remhash k plsense-direx::hash-explore-cache))))

(yaxception:deferror 'plsense-direx:explore-error nil "[PLSENSE-DIREX] Faild get the explore result of %s" 'pkg)
(defun plsense-direx::get-explore-result (pkgnm)
  (plsense-direx--trace "start get explore result of [%s]. use-root-cache[%s]"
                        pkgnm plsense-direx::use-root-cache)
  (let* ((reqpkgnm pkgnm)
         (ret (when pkgnm
               (or (gethash pkgnm plsense-direx::hash-explore-cache)
                   (direx:awhen (and plsense-direx::use-root-cache
                                     (plsense-direx::get-root-package-name pkgnm))
                     (setq reqpkgnm it)
                     (gethash it plsense-direx::hash-explore-cache))
                   (puthash reqpkgnm
                            (plsense--get-server-response (concat "explore " reqpkgnm)
                                                          :waitsec 3
                                                          :force t
                                                          :ignore-done t)
                            plsense-direx::hash-explore-cache)))))
    (when (or (not ret)
              (string= ret ""))
      (remhash reqpkgnm plsense-direx::hash-explore-cache)
      (plsense-direx--error "failed get explore result of [%s]" reqpkgnm)
      (yaxception:throw 'plsense-direx:explore-error :pkg reqpkgnm))
    ret))

(defun plsense-direx::get-matched-package-region (pkgre)
  (let* ((startpt (when (re-search-forward pkgre nil t)
                    (point-at-bol)))
         (endpt (when startpt
                  (goto-char (or (when (re-search-forward "^[^ ]" nil t)
                                   (point-at-bol))
                                 (point-max)))
                  (point))))
    (when (and startpt endpt)
      (buffer-substring-no-properties startpt endpt))))

(defun plsense-direx::get-own-explore-results (pkgnm)
  (plsense-direx--trace "start get own explore results of [%s]" pkgnm)
  (let ((re (rx-to-string `(and bol ,pkgnm " ")))
        (exret (plsense-direx::get-explore-result pkgnm)))
    (with-temp-buffer
      (insert exret)
      (goto-char (point-min))
      (direx:awhen (plsense-direx::get-matched-package-region re)
        (plsense-direx--trace "got own explore region.\n%s" it)
        (split-string it "\r?\n")))))

(defun plsense-direx::get-child-explore-results (pkgnm)
  (plsense-direx--trace "start get child explore results of [%s]" pkgnm)
  (let ((childre (rx-to-string `(and bol ,pkgnm "::" (+ (not (any ": \t\r\n"))) " ")))
        (exchildre (rx-to-string `(and bol (group ,pkgnm "::" (+ (not (any ": \t\r\n")))) "::" (not (any ": \t\r\n")))))
        (exret (plsense-direx::get-explore-result pkgnm)))
    (with-temp-buffer
      (insert exret)
      (let* ((founds)
             (childs (loop initially (goto-char (point-min))
                           for child = (plsense-direx::get-matched-package-region childre)
                           while child
                           if (string-match "\\`\\([^ ]+\\) " child)
                           do (pushnew (match-string-no-properties 1 child) founds :test 'equal)
                           collect child))
             (exchilds (loop initially (goto-char (point-min))
                             while (re-search-forward exchildre nil t)
                             for child = (match-string-no-properties 1)
                             if (not (member child founds))
                             collect (progn (push child founds)
                                            (concat child "\n"))
                             do (forward-line 1)
                             do (beginning-of-line))))
        (direx:awhen (mapconcat 'identity (sort (append childs exchilds) 'string<) "")
          (plsense-direx--trace "got child explore region.\n%s" it)
          (split-string it "\r?\n"))))))


;;;;;;;;;;;;
;; Mirror

(defvar plsense-direx::mirror-buffer-name " *PlSense DireX Mirror*")
(defvar plsense-direx::mirror-value "")
(defvar plsense-direx::mirror-entries nil)

(defun plsense-direx::mirror-clear-entry ()
  (setq plsense-direx::mirror-entries nil)
  (plsense-direx--trace "cleared mirror entry"))

(defun plsense-direx::mirror-start ()
  (plsense-direx--trace "start mirror restore")
  (with-current-buffer (get-buffer-create plsense-direx::mirror-buffer-name)
    (erase-buffer)
    (insert plsense-direx::mirror-value))
  (plsense-direx::mirror-clear-entry))

(defun plsense-direx::mirror-stop (&optional nostore)
  (plsense-direx--trace "start mirror store. nostore[%s]" nostore)
  (let ((buff (get-buffer plsense-direx::mirror-buffer-name)))
    (if (not (buffer-live-p buff))
        (plsense-direx--info "abort mirror store because buffer not exists")
      (when (not nostore)
        (with-current-buffer buff
          (setq plsense-direx::mirror-value (buffer-string))))
      (kill-buffer buff))))

(defun plsense-direx::mirror-init ()
  (plsense-direx--trace "start mirror init")
  (setq plsense-direx::mirror-value "")
  (let ((buff (get-buffer plsense-direx::mirror-buffer-name)))
    (when (buffer-live-p buff)
      (kill-buffer buff))))

(defun plsense-direx::mirror-search-location (addr)
  (let ((buff (get-buffer plsense-direx::mirror-buffer-name)))
    (plsense-direx--trace "start mirror search location : %s" addr)
    (if (not (buffer-live-p buff))
        (plsense-direx--info "abort mirror search location because buffer not exists")
      (with-current-buffer buff
        (goto-char (point-min))
        (direx:awhen (and (re-search-forward (concat "^" (regexp-quote addr) "$") nil t)
                          (count-lines (point-min) (point)))
          (plsense-direx--info "found mirrored location at %s : %s" it addr)
          it)))))

(defun plsense-direx::mirror-search-parent (addr)
  (let ((buff (get-buffer plsense-direx::mirror-buffer-name)))
    (plsense-direx--trace "start mirror search parent : %s" addr)
    (if (not (buffer-live-p buff))
        (plsense-direx--info "abort mirror search parent because buffer not exists")
      (with-current-buffer buff
        (loop with pkgnm = (or (plsense-direx::pick-method-name addr)
                               addr)
              while pkgnm
              do (goto-char (point-min))
              if (re-search-forward (concat "^" (regexp-quote pkgnm) "$") nil t)
              return (let ((line (count-lines (point-min) (point))))
                       (plsense-direx--info "found mirrored parent at %s : %s" line pkgnm)
                       line)
              else
              do (setq pkgnm (plsense-direx::get-parent-package-name pkgnm)))))))

(defun plsense-direx::mirror-insert (line &optional entries)
  (let ((buff (get-buffer plsense-direx::mirror-buffer-name))
        (entries (or entries plsense-direx::mirror-entries)))
    (plsense-direx--trace "start mirror insert at %s : %s" line entries)
    (when entries
      (if (not (buffer-live-p buff))
          (plsense-direx--info "abort mirror insert because buffer not exists")
        (with-current-buffer buff
          (goto-char (point-min))
          (when (> line 1) (forward-line (- line 1)))
          (beginning-of-line)
          (insert (mapconcat 'identity entries "\n") "\n")))))
  (when (not entries)
    (plsense-direx::mirror-clear-entry)))

(defun plsense-direx::mirror-delete (&optional entries)
  (let ((buff (get-buffer plsense-direx::mirror-buffer-name))
        (entries (or entries plsense-direx::mirror-entries)))
    (plsense-direx--trace "start mirror delete : %s" entries)
    (when entries
      (if (not (buffer-live-p buff))
          (plsense-direx--info "abort mirror delete because buffer not exists")
        (with-current-buffer buff
          (dolist (entry entries)
            (goto-char (point-min))
            (when (re-search-forward (concat "^" (regexp-quote entry) "$") nil t)
              (delete-region (point-at-bol) (+ (point-at-eol) 1))))))))
  (when (not entries)
    (plsense-direx::mirror-clear-entry)))

(defun plsense-direx::mirror-delete-matched (regexp)
  (let ((buff (get-buffer plsense-direx::mirror-buffer-name)))
    (plsense-direx--trace "start mirror delete matched : %s" regexp)
    (if (not (buffer-live-p buff))
        (plsense-direx--info "abort mirror delete matched because buffer not exists")
      (with-current-buffer buff
        (while (progn (goto-char (point-min))
                      (re-search-forward regexp nil t))
          (delete-region (point-at-bol) (+ (point-at-eol) 1)))))))


;;;;;;;;;;;
;; Trees

(defgeneric plsense-direx::get-location-address (location)
  "Return the address of LOCATION.")

(defgeneric plsense-direx::get-location-line (location)
  "Return the line number of LOCATION.")

(defgeneric plsense-direx::get-location-column (location)
  "Return the column number of LOCATION.")

(defgeneric plsense-direx::get-location-package (location)
  "Return the package string of LOCATION.")

(defgeneric plsense-direx::get-location-file (location)
  "Return the file path of LOCATION.")

(defgeneric plsense-direx::update-location (location)
  "update LOCATION by using cache.")

(defun plsense-direx::get-best-location (loc)
  (let ((file (plsense-direx::get-location-file loc))
        (line (plsense-direx::get-location-line loc))
        (col (plsense-direx::get-location-column loc)))
    (cond ((or (not file) (not line) (not col))
           (plsense-direx--warn ""))
          ((and (file-exists-p file) (> line 0) (> col 0))
           (list file line col))
          (t
           (plsense-direx::update-location loc)
           (list (plsense-direx::get-location-file loc)
                 (plsense-direx::get-location-line loc)
                 (plsense-direx::get-location-column loc))))))

(defun plsense-direx::goto-location (loc)
  (multiple-value-bind (file line col) (plsense-direx::get-best-location loc)
    (plsense-direx--trace "start goto location. file[%s] line[%s] col[%s]" file line col)
    (if (not (file-exists-p file))
        (plsense-direx::show-message "Not exist file of %s" (plsense-direx::get-location-package loc))
      (with-current-buffer (find-file-noselect file)
        (if (or (eq line 0) (eq col 0))
            (progn
              (plsense-direx::show-message "This package might be not yet ready.")
              (sleep-for 1)
              (plsense-direx::clear-explore-cache (plsense-direx::get-location-package loc)))
          (goto-char 1)
          (forward-line (- line 1))
          (forward-char (- col 1)))
        (current-buffer)))))


(defclass plsense-direx::method (direx:leaf)
  ((package :initarg :package
            :accessor plsense-direx::method-package)
   (line :initarg :line
         :accessor plsense-direx::method-line)
   (column :initarg :column
           :accessor plsense-direx::method-column)))

(defclass plsense-direx::package (direx:node)
  ((string :initarg :string
           :accessor plsense-direx::package-string)
   (file :initarg :file
         :accessor plsense-direx::package-file)
   (line :initarg :line
         :accessor plsense-direx::package-line)
   (column :initarg :column
           :accessor plsense-direx::package-column)))


(defsubst plsense-direx::make-method-by-explore (str pkg pkgnm &optional target-mtdnm)
  (when (string-match "\\`  &\\([^ ]+\\) +\\([0-9]+\\):\\([0-9]+\\)" str)
    (let* ((mtdnm (match-string-no-properties 1 str))
           (line (string-to-number (match-string-no-properties 2 str)))
           (col (string-to-number (match-string-no-properties 3 str)))
           (addr (plsense-direx::build-address pkgnm mtdnm)))
      (when (or (not target-mtdnm)
                (string= mtdnm target-mtdnm))
        (plsense-direx--trace "make method[%s] by explore : %s" mtdnm str)
        `(,addr . ,(make-instance 'plsense-direx::method
                                  :name mtdnm
                                  :package pkg
                                  :line line
                                  :column col))))))

(defun plsense-direx::make-method-by-name (mtdnm pkg pkgnm &optional explore-results)
  (plsense-direx--trace "start make method by name. mtdnm[%s] pkg[%s] explore-results[%s]"
                        mtdnm pkgnm explore-results)
  (loop for str in (or explore-results
                       (plsense-direx::get-own-explore-results pkgnm))
        for mtd-alist = (plsense-direx::make-method-by-explore str pkg pkgnm mtdnm)
        if mtd-alist return (cdr mtd-alist)))

(defmethod direx:tree-equals ((x plsense-direx::method) y)
  (or (eq x y)
      (and (typep y 'plsense-direx::method)
           (let* ((pkg1 (plsense-direx::method-package x))
                  (pkgnm1 (when pkg1 (plsense-direx::package-string pkg1)))
                  (pkg2 (plsense-direx::method-package y))
                  (pkgnm2 (when pkg2 (plsense-direx::package-string pkg2))))
             (equal (plsense-direx::build-address pkgnm1 (direx:tree-name x))
                    (plsense-direx::build-address pkgnm2 (direx:tree-name y)))))))

(defmethod plsense-direx::get-location-address ((mtd plsense-direx::method))
  (let* ((pkg (plsense-direx::method-package mtd))
         (pkgnm (when pkg (plsense-direx::package-string pkg))))
    (plsense-direx::build-address pkgnm (direx:tree-name mtd))))

(defmethod plsense-direx::get-location-line ((mtd plsense-direx::method))
  (plsense-direx::method-line mtd))

(defmethod plsense-direx::get-location-column ((mtd plsense-direx::method))
  (plsense-direx::method-column mtd))

(defmethod plsense-direx::get-location-package ((mtd plsense-direx::method))
  (let ((pkg (plsense-direx::method-package mtd)))
    (when pkg (plsense-direx::package-string pkg))))

(defmethod plsense-direx::get-location-file ((mtd plsense-direx::method))
  (let ((pkg (plsense-direx::method-package mtd)))
    (when pkg (plsense-direx::package-file pkg))))

(defmethod plsense-direx::update-location ((mtd plsense-direx::method))
  (let* ((mtdnm (direx:tree-name mtd))
         (pkg (plsense-direx::method-package mtd))
         (pkgnm (plsense-direx::package-string pkg))
         (umtd (progn (plsense-direx--trace "start update location of %s::%s" pkgnm mtdnm)
                      (plsense-direx::update-location pkg)
                      (plsense-direx::make-method-by-name mtdnm pkg pkgnm))))
    (setf (plsense-direx::method-line mtd) (plsense-direx::method-line umtd))
    (setf (plsense-direx::method-column mtd) (plsense-direx::method-column umtd))))


(defsubst* plsense-direx::make-package (&key name file line column)
  (make-instance 'plsense-direx::package
                 :name (or (when (string-match "\\`.+::\\([^:]+\\)\\'" name)
                             (match-string-no-properties 1 name))
                           name)
                 :string name
                 :file file
                 :line line
                 :column column))

(defsubst plsense-direx::make-package-by-explore (str)
  (when (string-match "\\`[^ \t\r\n]" str)
    (let* ((e (split-string str " +"))
           (pkgnm (when e (pop e)))
           (filepath (loop with ret = ""
                           while (> (length e) 1)
                           do (setq ret (concat ret (pop e)))
                           finally return ret))
           (e (when e (split-string (pop e) ":")))
           (line (when e (string-to-number (pop e))))
           (col (when e (string-to-number (pop e)))))
      (when pkgnm
        (plsense-direx--trace "make package[%s] by explore : %s" pkgnm str)
        `(,pkgnm . ,(plsense-direx::make-package :name pkgnm
                                                 :file filepath
                                                 :line line
                                                 :column col))))))

(defun plsense-direx::make-package-by-name (pkgnm &optional explore-results)
  (plsense-direx--trace "start make package by name. pkgnm[%s] explore-results[%s]" pkgnm explore-results)
  (if (or (not (stringp pkgnm))
          (string= pkgnm ""))
      (plsense-direx--warn "called plsense-direx::make-package-by-name by illegal value : pkgnm[%s]" pkgnm)
    (loop for str in (or explore-results
                         (plsense-direx::get-own-explore-results pkgnm))
          for pkg-alist = (plsense-direx::make-package-by-explore str)
          if pkg-alist return (cdr pkg-alist)
          finally return (plsense-direx::make-package :name pkgnm))))

(defmethod direx:tree-equals ((x plsense-direx::package) y)
  (or (eq x y)
      (and (typep y 'plsense-direx::package)
           (equal (plsense-direx::package-string x) (plsense-direx::package-string y)))))

(defmethod direx:node-children ((pkg plsense-direx::package))
  (let* ((pkgnm (plsense-direx::package-string pkg))
         (loc-alist (append (loop for str in (plsense-direx::get-own-explore-results pkgnm)
                                  for mtd-alist = (plsense-direx::make-method-by-explore str pkg pkgnm)
                                  if mtd-alist collect mtd-alist)
                            (loop for str in (plsense-direx::get-child-explore-results pkgnm)
                                  for child-alist = (plsense-direx::make-package-by-explore str)
                                  if child-alist collect child-alist)))
         (addrs (loop for e in loc-alist collect (car e)))
         (ret (loop for e in loc-alist collect (cdr e))))
    (when addrs
      (setq plsense-direx::mirror-entries (append plsense-direx::mirror-entries addrs))
      (plsense-direx--trace "added mirror entries\n%s" (mapconcat 'identity addrs "\n")))
    ret))

(defmethod direx:node-contains ((pkg plsense-direx::package) loc)
  (let ((pkgnm1 (plsense-direx::package-string pkg))
        (pkgnm2 (plsense-direx::get-location-package loc)))
    (and pkgnm1
         pkgnm2
         (direx:starts-with pkgnm2 pkgnm1))))

(defmethod plsense-direx::get-location-address ((pkg plsense-direx::package))
  (plsense-direx::package-string pkg))

(defmethod plsense-direx::get-location-line ((pkg plsense-direx::package))
  (plsense-direx::package-line pkg))

(defmethod plsense-direx::get-location-column ((pkg plsense-direx::package))
  (plsense-direx::package-column pkg))

(defmethod plsense-direx::get-location-package ((pkg plsense-direx::package))
  (plsense-direx::package-string pkg))

(defmethod plsense-direx::get-location-file ((pkg plsense-direx::package))
  (plsense-direx::package-file pkg))

(defmethod plsense-direx::update-location ((pkg plsense-direx::package))
  (let* ((pkgnm (plsense-direx::package-string pkg))
         (upkg (progn (plsense-direx--trace "start update location of %s" pkgnm)
                      (plsense-direx::clear-explore-cache pkgnm)
                      (plsense-direx::make-package-by-name pkgnm))))
    (setf (plsense-direx::package-file pkg)   (plsense-direx::package-file upkg))
    (setf (plsense-direx::package-line pkg)   (plsense-direx::package-line upkg))
    (setf (plsense-direx::package-column pkg) (plsense-direx::package-column upkg))))


;;;;;;;;;;;;;;;;
;; Tree Items

(defun plsense-direx::find-package-file (loc not-this-window)
  (let* ((addr (plsense-direx::get-location-address loc))
         (buff (progn (plsense-direx--trace "start find location : %s" addr)
                      (plsense-direx::goto-location loc))))
    (when buff
      (if not-this-window
          (switch-to-buffer-other-window buff)
        (switch-to-buffer buff)))))

(defun plsense-direx::display-package-file (loc)
  (let* ((addr (plsense-direx::get-location-address loc))
         (buff (progn (plsense-direx--trace "start display location : %s" addr)
                      (plsense-direx::goto-location loc))))
    (when buff
      (display-buffer buff))))


(defclass plsense-direx::item (direx:item) ())

(defmethod direx:generic-find-item ((item plsense-direx::item) not-this-window)
  (plsense-direx::find-package-file (direx:item-tree item) not-this-window))

(defmethod direx:generic-display-item ((item plsense-direx::item))
  (plsense-direx::display-package-file (direx:item-tree item)))

(defmethod direx:make-item ((pkg plsense-direx::package) parent)
  (make-instance 'plsense-direx::item
                 :tree pkg
                 :parent parent
                 :face 'plsense-direx:package-face))

(defmethod direx:make-item ((mtd plsense-direx::method) parent)
  (make-instance 'plsense-direx::item
                 :tree mtd
                 :parent parent
                 :face 'plsense-direx:regular-method-face))

(defmethod direx:item-refresh ((item plsense-direx::item))
  (yaxception:$~
    (yaxception:try
      (let* ((pt (direx:item-start item))
             (parent (direx:item-parent item))
             (loc (direx:item-tree item))
             (pkgnm (plsense-direx::get-location-package loc))
             (addr (plsense-direx::get-location-address loc))
             (line (+ (count-lines 1 pt) 1)))
        (plsense-direx--trace "start direx:item-refresh : %s" addr)
        (plsense-direx::clear-explore-cache-by-regexp (concat "\\`" pkgnm "\\(\\'\\|::\\)"))
        (direx:item-delete-recursively item)
        (plsense-direx::update-location loc)
        (goto-char pt)
        (direx:item-insert (direx:make-item loc parent))
        (plsense-direx::mirror-start)
        (plsense-direx::mirror-delete-matched (concat "^" addr "\\($\\|::\\)"))
        (when (not (string-match "\\`&" addr))
          (plsense-direx::mirror-delete-matched (concat "^&" addr "::")))
        (plsense-direx::mirror-insert line (list addr))
        (plsense-direx::mirror-stop)
        (plsense-direx--trace "finished direx:item-refresh : %s" addr)))
    (yaxception:catch 'error e
      (plsense-direx::mirror-init)
      (plsense-direx--error "failed direx:item-refresh : %s" (yaxception:get-text e))
      (kill-buffer)
      (yaxception:throw e))))


;;;;;;;;;;;;;;;
;; Fix Direx

(defvar plsense-direx::active-p nil)
(defvar plsense-direx::mirror-active-p nil)

(defadvice direx:item-expand (around plsense-direx:fix activate)
  (if (not plsense-direx::mirror-active-p)
      ad-do-it
    (yaxception:$~
      (yaxception:try
        (plsense-direx--trace "start direx:item-expand")
        (lexical-let ((line (+ (count-lines 1 (direx:item-end (ad-get-arg 0))) 1)))
          (plsense-direx::mirror-start)
          ad-do-it
          (if (not plsense-direx::mirror-entries)
              (plsense-direx::mirror-stop t)
            (plsense-direx::mirror-insert line)
            (plsense-direx::mirror-stop))))
      (yaxception:catch 'error e
        (plsense-direx::mirror-init)
        (plsense-direx--error "failed direx:item-expand : %s" (yaxception:get-text e))
        (kill-buffer)
        (yaxception:throw e)))))

(defadvice direx:item-refresh-recursively (around plsense-direx:fix activate)
  (if (not plsense-direx::active-p)
      ad-do-it
    (direx:item-refresh (ad-get-arg 0))))

(defadvice direx:refresh-whole-tree (around plsense-direx:fix activate)
  (if (not plsense-direx::active-p)
      ad-do-it
    (yaxception:$~
      (yaxception:try
        (lexical-let* ((item (or (ad-get-arg 0)
                                (direx:item-at-point)))
                       (pt (direx:item-start item))
                       (loc (direx:item-tree item))
                       (addr (plsense-direx::get-location-address loc)))
          (plsense-direx--trace "start direx:refresh-whole-tree : %s" addr)
          ad-do-it
          (setq plsense-direx::mirror-active-p nil)
          (plsense-direx::mirror-start)
          (direx:awhen (or (plsense-direx::mirror-search-location addr)
                           (plsense-direx::mirror-search-parent addr))
            (goto-char 1)
            (forward-line (- it 1))
            (plsense-direx::down-to-location loc))
          (plsense-direx::mirror-stop)
          (setq plsense-direx::mirror-active-p t)
          (plsense-direx--trace "finished direx:refresh-whole-tree : %s" addr)))
      (yaxception:catch 'error e
        (plsense-direx::mirror-init)
        (plsense-direx--error "failed direx:refresh-whole-tree : %s" (yaxception:get-text e))
        (ignore-errors (kill-buffer))
        (yaxception:throw e)))))


;;;;;;;;;;;;;;;;;;
;; Setup Buffer

(defvar plsense-direx:direx-mode-map
  (let ((map (copy-keymap direx:direx-mode-map)))
    (define-key map (kbd "?") 'plsense-direx:display-help)
    map))

(defun plsense-direx::sort-root-item (i1 i2)
  (let ((t1 (direx:item-tree i1))
        (t2 (direx:item-tree i2)))
    (string< (direx:tree-name t1) (direx:tree-name t2))))

(defvar plsense-direx::root-items nil)
(defun plsense-direx::add-root-into-buffer (root buff)
  (with-current-buffer buff
    (let ((rootitem (direx:make-item root nil))
          (buffer-read-only nil))
      (setq direx:root-item rootitem)
      ;; Update the list of root item
      (push rootitem plsense-direx::root-items)
      (setq plsense-direx::root-items
            (sort plsense-direx::root-items 'plsense-direx::sort-root-item))
      ;; Go to insert point
      (goto-char (loop with pt = 1
                       for item in plsense-direx::root-items
                       for currloc = (direx:item-tree item)
                       if (direx:tree-equals currloc root)
                       return pt
                       else
                       do (setq pt (or (direx:item-end item)
                                       (point-max)))))
      ;; Insert with mirroring
      (direx:item-insert rootitem)
      (direx:move-to-item-name-part rootitem)
      (plsense-direx::mirror-insert (count-lines 1 (point))
                                    (list (plsense-direx::get-location-address root))))))

(defun plsense-direx::down-to-location (loc)
  (loop with depth = -1
        with locaddr = (plsense-direx::get-location-address loc)
        for item = (direx:item-at-point)
        for currloc = (when item (direx:item-tree item))
        for currlocaddr = (if currloc (plsense-direx::get-location-address currloc) "")
        for currdepth = (length (split-string currlocaddr "::"))
        while currloc
        ;; Initialize depth
        do (when (= depth -1)
             (plsense-direx--trace "start down to location[%s] from [%s]" locaddr currlocaddr)
             (setq depth (- currdepth 1)))
        ;; The location is found
        if (direx:tree-equals currloc loc)
        return (loop with target = item
                     for item = (when item (direx:item-parent item))
                     while item
                     ;; The tree of the location might be hidden
                     do (direx:item-ensure-open item)
                     finally
                     (plsense-direx--info "finished down to location[%s]" locaddr)
                     (direx:move-to-item-name-part target))
        ;; The parent is found
        else if (and (typep currloc 'direx:node)
                     (direx:node-contains currloc loc))
        do (progn
             (plsense-direx--debug "down into [%s]" currlocaddr)
             ;; Update depth
             (setq depth currdepth)
             ;; Open with mirroring
             (direx:item-ensure-open item)
             (forward-line 1)
             (beginning-of-line)
             (plsense-direx::mirror-insert (+ (count-lines 1 (point)) 1)))
        ;; Non-increase of depth means the location is not found
        else if (<= currdepth depth)
        return (plsense-direx::show-message "Item not found")
        ;; Go next
        else
        do (forward-line 1)
        finally (plsense-direx::show-message "Item not found")))

(defvar plsense-direx::buffer-name "*PlSense DireX*")
(defun plsense-direx::get-buffer (&optional create)
  (direx:awhen (and (string= plsense-direx::mirror-value "")
                    (get-buffer plsense-direx::buffer-name))
    (plsense-direx--info "kill buffer because failed mirroring.")
    (kill-buffer it))
  (or (when (not create)
        (get-buffer plsense-direx::buffer-name))
      (with-current-buffer (generate-new-buffer plsense-direx::buffer-name)
        (plsense-direx--trace "generated new buffer : %s" (buffer-name))
        (direx:direx-mode)
        (use-local-map plsense-direx:direx-mode-map)
        (set (make-local-variable 'plsense-direx::active-p) t)
        (set (make-local-variable 'plsense-direx::mirror-active-p) nil)
        (plsense-direx::mirror-init)
        (setq plsense-direx::root-items nil)
        (current-buffer))))

(yaxception:deferror 'plsense-direx:illegal-pkg-error nil "'%s' is illegal package" 'pkg)

(defun plsense-direx::open-buffer (pkgnm &optional mtdnm)
  (plsense-direx--trace "start open buffer. pkgnm[%s] mtdnm[%s]" pkgnm mtdnm)
  (let* ((plsense-direx::use-root-cache t)
         (buff (plsense-direx::get-buffer))
         (exrets (plsense-direx::get-own-explore-results pkgnm))
         (pkg (plsense-direx::make-package-by-name pkgnm exrets))
         (mtd (when mtdnm (plsense-direx::make-method-by-name mtdnm pkg pkgnm exrets)))
         (loc (or mtd pkg))
         (addr (plsense-direx::build-address pkgnm mtdnm)))
    (yaxception:$~
      (yaxception:try
        (when (not loc)
          (yaxception:throw 'plsense-direx:illegal-pkg-error :pkg pkgnm))
        (with-current-buffer buff
          (plsense-direx--trace "start setup buffer")
          (goto-char 1)
          (setq plsense-direx::mirror-active-p nil)
          (plsense-direx::mirror-start)
          (direx:aif (or (plsense-direx::mirror-search-location addr)
                         (plsense-direx::mirror-search-parent addr))
              (forward-line (- it 1))
            (let* ((rootnm (plsense-direx::get-root-package-name pkgnm))
                   (root (when rootnm
                           (plsense-direx::make-package-by-name rootnm))))
              (plsense-direx::add-root-into-buffer root (current-buffer))))
          (plsense-direx::down-to-location loc)
          (plsense-direx::mirror-stop)
          (setq plsense-direx::mirror-active-p t))
        (plsense-direx--trace "opened buffer for pkgnm[%s] mtdnm[%s]" pkgnm mtdnm)
        buff)
      (yaxception:catch 'error e
        (plsense-direx::mirror-init)
        (plsense-direx--error "failed open buffer : %s" (yaxception:get-text e))
        (when (buffer-live-p buff)
          (kill-buffer buff))
        (plsense-direx::show-message "Failed open explorer : %s" (yaxception:get-text e))))))

;;;###autoload
(defun plsense-direx:config-default ()
  "Do setting recommemded configuration."
  (loop for mode in plsense-enable-modes
        for hook = (intern-soft (concat (symbol-name mode) "-hook"))
        do (add-to-list 'ac-modes mode)
        if (and hook
                (symbolp hook))
        do (add-hook hook 'plsense-direx:setup-current-buffer t))
  (add-hook 'after-save-hook 'plsense-direx:update-current-package t))


;;;;;;;;;;;;;;;;;;
;; User Command

;;;###autoload
(defun plsense-direx:open-explorer (&optional other-window)
  "Open perl package explorer."
  (interactive "P")
  (yaxception:$~
    (yaxception:try
      (if (not (plsense--active-p))
          (plsense-direx::show-message "Not available buffer : %s" (buffer-name))
        (plsense--try-to-ready)
        (if (not (plsense--ready-p))
            (plsense-direx::show-message "Not yet ready current buffer")
          (when (and (plsense--set-current-file)
                     (plsense--set-current-package)
                     (plsense--set-current-method))
            (multiple-value-bind (pkgnm mtdnm) (plsense-direx::get-current-pkg/mtd)
              (when (or (not pkgnm)
                        (string= pkgnm "main"))
                (setq pkgnm (plsense-direx::select-package))
                (setq mtdnm nil))
              (direx:awhen (plsense-direx::open-buffer pkgnm mtdnm)
                (if other-window
                    (switch-to-buffer-other-window it)
                  (switch-to-buffer it))))))))
    (yaxception:catch 'plsense-direx:explore-error e
      (message (yaxception:get-text e)))
    (yaxception:catch 'error e
      (plsense-direx::show-message "Failed open explorer" (yaxception:get-text e))
      (plsense-direx--error "failed open explorer : %s" (yaxception:get-text e)))))

;;;###autoload
(defun plsense-direx:open-explorer-other-window ()
  "Open perl package explorer in other window."
  (interactive)
  (plsense-direx:open-explorer t))

;;;###autoload
(defun plsense-direx:display-help ()
  "Display help buffer about the current node/leaf."
  (interactive)
  (yaxception:$~
    (yaxception:try
      (let* ((item (or (direx:item-at-point)
                       (plsense-direx::show-message "Item not found from current point.")))
             (loc (when item (direx:item-tree item)))
             (pkgnm (when loc (plsense-direx::get-location-package loc)))
             (mtdnm (when (typep loc 'plsense-direx::method) (direx:tree-name loc)))
             (cmdstr (cond (mtdnm (concat "subhelp " mtdnm " " pkgnm))
                           (pkgnm (concat "modhelp " pkgnm))))
             (doc (when cmdstr
                    (plsense--get-server-response cmdstr :waitsec 4 :force t :ignore-done t)))
             (buff (plsense--get-help-buffer doc)))
        (when (buffer-live-p buff)
          (display-buffer buff))))
    (yaxception:catch 'error e
      (plsense-direx::show-message "Failed display help : %s" (yaxception:get-text e))
      (plsense-direx--error "failed display help : %s" (yaxception:get-text e)))))

;;;###autoload
(defun plsense-direx:update-current-package ()
  "Update the package of current point."
  (interactive)
  (yaxception:$~
    (yaxception:try
      (when (and (plsense--active-p)
                 (plsense--ready-p)
                 (plsense--set-current-file)
                 (plsense--set-current-package)
                 (plsense--set-current-method))
        (plsense-direx--trace "start update current package")
        (multiple-value-bind (pkgnm mtdnm) (plsense-direx::get-current-pkg/mtd)
          (let ((buff (get-buffer plsense-direx::buffer-name)))
            (when (and pkgnm
                       (not (string= pkgnm "main"))
                       (buffer-live-p buff))
              (with-current-buffer buff
                (plsense-direx::mirror-start)
                (let* ((addr (plsense-direx::build-address pkgnm nil))
                       (item (direx:awhen (plsense-direx::mirror-search-location addr)
                               (save-excursion
                                 (goto-char 1)
                                 (forward-line (- it 1))
                                 (direx:item-at-point))))
                       (locs (when item
                               (append (direx:item-tree item)
                                       (loop for child in (direx:item-children item)
                                             collect (direx:item-tree child))))))
                  (loop for loc in locs
                        if loc do (plsense-direx::update-location loc))
                  (plsense-direx::mirror-stop)
                  (plsense-direx--trace "updated current package : %s" addr))))))))
    (yaxception:catch 'error e
      (plsense-direx::show-message "Failed update current package : %s" (yaxception:get-text e))
      (plsense-direx--error "failed update current package : %s" (yaxception:get-text e)))))

;;;###autoload
(defun plsense-direx:setup-current-buffer ()
  "Do setup for using plsense-direx in current buffer."
  (interactive)
  (yaxception:$~
    (yaxception:try
      (when (plsense--active-p)
        ;; Key binding
        (when (and (stringp plsense-direx:open-explorer-key)
                   (not (string= plsense-direx:open-explorer-key "")))
          (local-set-key (read-kbd-macro plsense-direx:open-explorer-key)
                         'plsense-direx:open-explorer))
        (when (and (stringp plsense-direx:open-explorer-other-window-key)
                   (not (string= plsense-direx:open-explorer-other-window-key "")))
          (local-set-key (read-kbd-macro plsense-direx:open-explorer-other-window-key)
                         'plsense-direx:open-explorer-other-window))
        (plsense-direx--info "finished setup for %s" (current-buffer))))
    (yaxception:catch 'error e
      (plsense-direx::show-message "Failed setup current buffer : %s" (yaxception:get-text e))
      (plsense-direx--error "failed setup current buffer : %s" (yaxception:get-text e)))))


(provide 'plsense-direx)
;;; plsense-direx.el ends here
