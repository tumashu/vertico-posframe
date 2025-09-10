;;; vertico-posframe.el --- Using posframe to show Vertico  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/vertico-posframe
;; Version: 0.9.0
;; Keywords: abbrev, convenience, matching, vertico
;; Package-Requires: ((emacs "26.0") (posframe "1.4.0") (vertico "2.5"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; * vertico-posframe README                                :README:

;; ** What is vertico-posframe

;; vertico-posframe is an vertico extension, which lets vertico use posframe to show
;; its candidate menu.

;; NOTE: vertico-posframe requires Emacs 26 and do not support mouse
;; click.

;; ** How to let vertico-posframe work well with vertico-multiform.
;; #+BEGIN_EXAMPLE
;; (setq vertico-multiform-commands
;;       '((consult-line
;;          posframe
;;          (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
;;          (vertico-posframe-border-width . 10)
;;          ;; NOTE: This is useful when emacs is used in both in X and
;;          ;; terminal, for posframe do not work well in terminal, so
;;          ;; vertico-buffer-mode will be used as fallback at the
;;          ;; moment.
;;          (vertico-posframe-fallback-mode . vertico-buffer-mode))
;;         (t posframe)))
;; (vertico-multiform-mode 1)
;; #+END_EXAMPLE

;; ** How to enable vertico-posframe
;; #+BEGIN_EXAMPLE
;; (require 'vertico-posframe)
;; (vertico-posframe-mode 1)
;; #+END_EXAMPLE

;; ** Tips

;; *** How to show fringe to vertico-posframe
;; #+BEGIN_EXAMPLE
;; (setq vertico-posframe-parameters
;;       '((left-fringe . 8)
;;         (right-fringe . 8)))
;; #+END_EXAMPLE

;; By the way, User can set *any* parameters of vertico-posframe with
;; the help of `vertico-posframe-parameters'.

;;; Code:
;; * vertico-posframe's code
(require 'posframe)
(require 'vertico)
(require 'vertico-multiform)

(defgroup vertico-posframe nil
  "Using posframe to show vertico."
  :group 'vertico)

(defcustom vertico-posframe-fallback-mode #'ignore
  "When posframe is not workable, this mode will be used as fallback."
  :type 'function)

(defcustom vertico-posframe-font nil
  "The font used by vertico-posframe.
When nil, Using current frame's font as fallback."
  :type 'string)

(defcustom vertico-posframe-width nil
  "The width of vertico-posframe."
  :type 'number)

(defcustom vertico-posframe-height nil
  "The height of vertico-posframe."
  :type 'number)

(defcustom vertico-posframe-min-width nil
  "The min width of vertico-posframe."
  :type 'number)

(defcustom vertico-posframe-min-height nil
  "The min height of vertico-posframe."
  :type 'number)

(defcustom vertico-posframe-truncate-lines t
  "Non-nil means truncate lines in vertico-posframe."
  :type 'boolean)

(defcustom vertico-posframe-poshandler #'posframe-poshandler-frame-center
  "The posframe poshandler used by vertico-posframe."
  :type 'function)

(defcustom vertico-posframe-refposhandler #'vertico-posframe-refposhandler-default
  "The refposhandler used by vertico-posframe.

NOTE: This variable is very useful to EXWM users."
  :type 'function)

(defcustom vertico-posframe-size-function #'vertico-posframe-get-size
  "The function which is used to deal with posframe's size."
  :type 'function)

(defcustom vertico-posframe-border-width 2
  "The border width used by vertico-posframe.
When 0, no border is showed."
  :type 'number)

(defcustom vertico-posframe-parameters nil
  "The frame parameters used by vertico-posframe."
  :type '(alist :key-type symbol :value-type sexp))

(defcustom vertico-posframe-show-minibuffer-rules
  (list "^eval-*")
  "A list of rule showed minibuffer.

a rule can be a regexp or a function.

1. when rule is a regexp and it match `this-command'.
2. when rule is a function and it return t.
3. when rule is a symbol, its value is t.

minibuffer will not be hided by minibuffer-cover."
  :type '(repeat (choice string function)))

(defcustom vertico-posframe-vertico-multiform-key "M-P"
  "The vertico-posframe keybinding used in vertico-multiform."
  :type '(choice (const nil) string))

(defface vertico-posframe
  '((t (:inherit default)))
  "Face used by the vertico-posframe."
  :group 'vertico-posframe)

(defface vertico-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the vertico-posframe's border when minibuffer-depth = 1."
  :group 'vertico-posframe)

(defface vertico-posframe-border-2
  '((t (:inherit default :background "red")))
  "Face used by the vertico-posframe's border when minibuffer-depth = 2."
  :group 'vertico-posframe)

(defface vertico-posframe-border-3
  '((t (:inherit default :background "green")))
  "Face used by the vertico-posframe's border when minibuffer-depth = 3."
  :group 'vertico-posframe)

(defface vertico-posframe-border-4
  '((t (:inherit default :background "blue")))
  "Face used by the vertico-posframe's border when minibuffer-depth = 4."
  :group 'vertico-posframe)

(defface vertico-posframe-border-fallback
  '((t (:inherit default :background "yellow")))
  "Face used by the vertico-posframe's border when find no face."
  :group 'vertico-posframe)

(defvar vertico-posframe--buffer nil)
(defvar vertico-posframe--use-auto-hscroll-mode-p nil)

;; Fix warn
(defvar exwm--connection)
(defvar exwm-workspace--workareas)
(defvar exwm-workspace-current-index)

;;;###autoload
(define-minor-mode vertico-posframe-mode
  "Display Vertico in posframe instead of the minibuffer."
  :global t
  (if vertico-posframe-mode
      (unless (posframe-workable-p)
        (funcall (buffer-local-value 'vertico-posframe-fallback-mode (current-buffer)) 1))
    (if (posframe-workable-p)
        (vertico-posframe--multiform-function)
      (funcall (buffer-local-value 'vertico-posframe-fallback-mode (current-buffer)) -1))))

(defun vertico-posframe--multiform-function ()
  "Function work with `'vertico-multiform-mode'.
When `vertico-posframe-mode' is disabled, hide posframe and let
the contents of minibuffer show again, this approach let
vertico-posframe works with vertico multiform toggle."
  (set-window-vscroll (active-minibuffer-window) 0)
  (posframe-hide vertico-posframe--buffer))

;; Support vertico-multiform
(defun vertico-posframe-vertico-multiform-setup ()
  (let* ((name 'posframe)
         (mode (intern (format "vertico-%s-mode" name)))
         (toggle (intern (format "vertico-multiform-%s" name))))
    (defalias toggle
      (lambda ()
        (interactive)
        (vertico-multiform-vertical mode))
      (format "Toggle the %s display." name))
    (push mode vertico-multiform--display-modes)
    (put toggle 'completion-predicate #'vertico--command-p)
    (when vertico-posframe-vertico-multiform-key
      (define-key vertico-multiform-map
                  (kbd vertico-posframe-vertico-multiform-key)
                  #'vertico-multiform-posframe))))

(vertico-posframe-vertico-multiform-setup)

(cl-defmethod vertico--setup
  :after (&context ((vertico-posframe-mode-workable-p) (eql t)))
  "Setup minibuffer overlay, which pushes the minibuffer content down."
  (add-hook 'minibuffer-exit-hook #'vertico-posframe--minibuffer-exit-hook nil 'local))

(defun vertico-posframe-mode-workable-p ()
  "Test `vertico-posframe-mode' is actived and can work or not."
  (and vertico-posframe-mode
       (posframe-workable-p)))

(defun vertico-posframe--minibuffer-exit-hook ()
  "The function used by `minibuffer-exit-hook'."
  ;; `vertico--resize-window' have set `max-mini-window-height' to
  ;; 1.0, so I think setting it to 1.0 here is safe :-).
  (setq-local max-mini-window-height 1.0)
  ;; Before hide vertico-posframe, we should clean window hscroll
  ;; effect, to handle long path issue:
  ;; https://github.com/tumashu/vertico-posframe/issues/37
  (posframe-funcall
   vertico-posframe--buffer
   (lambda ()
     (setf (window-hscroll) 0)
     (when vertico-posframe--use-auto-hscroll-mode-p
       (kill-local-variable 'auto-hscroll-mode)
       (setq-local vertico-posframe--use-auto-hscroll-mode-p nil))))
  (posframe-hide vertico-posframe--buffer))

(cl-defmethod vertico--resize-window
  (_height &context ((vertico-posframe-mode-workable-p) (eql t))))

(cl-defmethod vertico--display-candidates
  :after (_candidates &context ((vertico-posframe-mode-workable-p) (eql t)))
  "Display candidates in posframe.

1. Let minibuffer-window's height = 1
2. Hide the context of minibuffer-window by vscroll 100.
3. Show minibuffer with the help of posframe-show."
  (let ((buffer (current-buffer))
        (point (point)))
    ;; NOTE: buffer is minibuffer.
    (setq vertico-posframe--buffer buffer)
    (vertico-posframe--handle-minibuffer-window)
    (vertico-posframe--show buffer point)))

(defun vertico-posframe--handle-minibuffer-window ()
  "Handle minibuffer window."
  (let ((show-minibuffer-p (vertico-posframe--show-minibuffer-p))
        (minibuffer-window (active-minibuffer-window)))
    (setq-local max-mini-window-height 1)
    ;; Let minibuffer-window's height = 1
    (window-resize minibuffer-window
                   (- (window-pixel-height minibuffer-window))
                   nil nil 'pixelwise)
    ;; Hide the context showed in minibuffer-window.
    (set-window-vscroll minibuffer-window 100)
    (when show-minibuffer-p
      (set-window-vscroll minibuffer-window 0))))

(defun vertico-posframe--show-minibuffer-p ()
  "Test show minibuffer or not."
  (cl-some
   (lambda (rule)
     (cond ((functionp rule)
            (funcall rule))
           ((and rule
                 (stringp rule)
                 (symbolp this-command))
            (string-match-p rule (symbol-name this-command)))
           ((symbolp rule)
            (symbol-value rule))
           (t nil)))
   vertico-posframe-show-minibuffer-rules))

(defun vertico-posframe--show (buffer window-point)
  "`posframe-show' of vertico-posframe."
  (with-selected-window (vertico-posframe-last-window) ;Some posframe poshandlers need infos of last-window.
    (with-current-buffer buffer
      (unless (local-variable-p 'auto-hscroll-mode)
        (setq-local auto-hscroll-mode 'current-line)
        (setq-local vertico-posframe--use-auto-hscroll-mode-p t)))
    (apply #'posframe-show
           buffer
           :cursor (if (eq cursor-type t)
                       ;; For vertico-posframe special, when
                       ;; cursor-type is t, hollow box will be showed,
                       ;; which is not expected by user, so we should
                       ;; force show it with box.
                       'box
                     cursor-type)
           :tty-non-selected-cursor t
           :window-point window-point
           :font (buffer-local-value 'vertico-posframe-font buffer)
           ;; Variable settings in `vertico-multiform-commands' will
           ;; save to BUFFER as buffer-local variables, so we need to
           ;; get buffer local value from BUFFER, for example:
           ;;
           ;; (setq vertico-multiform-commands
           ;;       '((consult-line
           ;;          posframe
           ;;          (vertico-posframe-poshandler . posframe-poshandler-frame-top-center))
           ;;         (t buffer)))
           ;;
           :poshandler (buffer-local-value 'vertico-posframe-poshandler buffer)
           :background-color (face-attribute 'vertico-posframe :background nil t)
           :foreground-color (face-attribute 'vertico-posframe :foreground nil t)
           :border-width (buffer-local-value 'vertico-posframe-border-width buffer)
           :border-color (vertico-posframe--get-border-color)
           :override-parameters (buffer-local-value 'vertico-posframe-parameters buffer)
           :refposhandler (buffer-local-value 'vertico-posframe-refposhandler buffer)
           :hidehandler #'vertico-posframe-hidehandler
           :lines-truncate (buffer-local-value 'vertico-posframe-truncate-lines buffer)
           (funcall (buffer-local-value 'vertico-posframe-size-function buffer) buffer))))

(defun vertico-posframe-last-window ()
  "Get the last actived window before active minibuffer."
  (let ((window (minibuffer-selected-window)))
    (or (if (window-live-p window)
            window
          (next-window))
        (selected-window))))

(defun vertico-posframe--get-border-color ()
  "Get color of vertico-posframe border."
  (face-attribute
   (let* ((n (minibuffer-depth))
          (face (intern (format "vertico-posframe-border-%s" n)))
          (face-fallback 'vertico-posframe-border-fallback))
     (if (= n 1)
         'vertico-posframe-border
       (if (facep face)
           face
         face-fallback)))
   :background nil t))

(defun vertico-posframe-refposhandler-default (&optional frame)
  "The default posframe refposhandler used by vertico-posframe.
Optional argument FRAME ."
  (cond
   ;; EXWM environment
   ((bound-and-true-p exwm--connection)
    (or (ignore-errors
          (let ((info (elt exwm-workspace--workareas
                           exwm-workspace-current-index)))
            (cons (elt info 0)
                  (elt info 1))))
        ;; Need user install xwininfo.
        (ignore-errors
          (posframe-refposhandler-xwininfo frame))
        ;; Fallback, this value will incorrect sometime, for example: user
        ;; have panel.
        (cons 0 0)))
   (t nil)))

(defun vertico-posframe-hidehandler (_)
  "Hidehandler used by vertico-posframe."
  (not (minibufferp)))

(defun vertico-posframe-get-size (buffer)
  "The default functon used by `vertico-posframe-size-function'."
  (list
   :height (buffer-local-value 'vertico-posframe-height buffer)
   :width (buffer-local-value 'vertico-posframe-width buffer)
   :min-height (or (buffer-local-value 'vertico-posframe-min-height buffer)
                   (let ((height (+ vertico-count 1)))
                     (min height (or (buffer-local-value 'vertico-posframe-height buffer) height))))
   :min-width (or (buffer-local-value 'vertico-posframe-min-width buffer)
                  (let ((width (round (* (frame-width) 0.62))))
                    (min width (or (buffer-local-value 'vertico-posframe-width buffer) width))))))

;;;###autoload
(defun vertico-posframe-cleanup ()
  "Remove frames and buffers used for vertico-posframe."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (minibufferp buffer)
      (posframe-delete-frame buffer))))

(provide 'vertico-posframe)
;;; vertico-posframe.el ends here
