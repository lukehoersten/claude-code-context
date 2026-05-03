;;; claude-code-context.el --- Share buffer context with Claude Code -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Luke Hoersten

;; Author: Luke Hoersten <Luke@Hoersten.org>
;; Maintainer: Luke Hoersten <Luke@Hoersten.org>
;; URL: https://github.com/lukehoersten/claude-code-context
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, convenience
;; Assisted-by: Claude:claude-opus-4-7

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package automatically shares your current buffer context
;; (file, line, column, selection, and diagnostics) with Claude Code
;; via a context file that Claude Code hooks can read.
;;
;; Setup:
;; 1. Add to your init.el:
;;    (require 'claude-code-context)
;;    (claude-code-context-mode 1)
;;
;; 2. Add this hook to your ~/.claude/settings.json:
;;    {
;;      "hooks": {
;;        "UserPromptSubmit": [
;;          {
;;            "hooks": [
;;              {
;;                "type": "command",
;;                "command": "CONTEXT_FILE=\"${XDG_CONFIG_HOME:-$HOME/.config}/emacs/claude-code-context.json\"; [ -f \"$CONTEXT_FILE\" ] || CONTEXT_FILE=\"$HOME/.emacs.d/claude-code-context.json\"; if [ -f \"$CONTEXT_FILE\" ]; then echo \"\\n---\\n## Emacs Context\\n\"; cat \"$CONTEXT_FILE\"; echo \"\\n---\"; fi"
;;              }
;;            ]
;;          }
;;        ]
;;      }
;;    }
;;
;; Usage:
;; - C-c C-l u : Manually update context
;; - C-c C-l d : Add flymake diagnostics to context
;; - C-c C-l c : Clear context
;; - C-c C-l m : Toggle automatic context mode

;;; Code:

(require 'json)

(declare-function flymake-diagnostics "flymake" (&optional beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-backend "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))

(defgroup claude-code-context nil
  "Share buffer context with Claude Code."
  :group 'tools
  :prefix "claude-code-context-")

(defcustom claude-code-context-file
  (expand-file-name "claude-code-context.json" user-emacs-directory)
  "File where Claude Code context is written."
  :type 'file
  :group 'claude-code-context)

(defcustom claude-code-context-update-interval 2
  "Seconds between context updates (to avoid excessive writes)."
  :type 'integer
  :group 'claude-code-context)

(defvar claude-code-context-timer nil
  "Timer for updating Claude Code context.")

(defvar claude-code-context--last-written nil
  "Last JSON string written to context file, used to skip redundant writes.")

(defun claude-code-context--get-current-context ()
  "Get current buffer context as an alist."
  (unless (minibufferp)
    (let* ((file (or (buffer-file-name) (buffer-name)))
           (line (line-number-at-pos))
           (col (current-column))
           (modified (and (buffer-file-name) (buffer-modified-p)))
           (selection (when (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end)))))
      `((buffer . ,file)
        (line . ,line)
        (column . ,col)
        (modified . ,(if modified t :json-false))
        ,@(when selection `((selection . ,selection)))))))

(defun claude-code-context--get-flymake-diagnostics ()
  "Get flymake diagnostics for current buffer as a list."
  (when (and (bound-and-true-p flymake-mode)
             (buffer-file-name))
    (let ((diags (flymake-diagnostics)))
      (when diags
        (mapcar
         (lambda (diag)
           `((line . ,(line-number-at-pos (flymake-diagnostic-beg diag)))
             (type . ,(symbol-name (flymake-diagnostic-type diag)))
             (backend . ,(symbol-name (flymake-diagnostic-backend diag)))
             (text . ,(flymake-diagnostic-text diag))))
         diags)))))

(defun claude-code-context-update-context ()
  "Update Claude Code context file with current buffer state."
  (interactive)
  (let ((context (claude-code-context--get-current-context)))
    (when context
      (let ((json (json-encode context)))
        (unless (equal json claude-code-context--last-written)
          (setq claude-code-context--last-written json)
          (with-temp-file claude-code-context-file
            (insert json)))
        (when (called-interactively-p 'interactive)
          (message "Claude Code context updated"))))))

(defun claude-code-context-add-diagnostics ()
  "Add flymake diagnostics to Claude Code context file."
  (interactive)
  (let ((context (claude-code-context--get-current-context))
        (diags (claude-code-context--get-flymake-diagnostics)))
    (when context
      (when diags
        (setq context (append context `((diagnostics . ,diags)))))
      (with-temp-file claude-code-context-file
        (insert (json-encode context)))
      (message "Claude Code context updated with diagnostics"))))

(defun claude-code-context-clear-context ()
  "Clear the Claude Code context file."
  (interactive)
  (setq claude-code-context--last-written nil)
  (when (file-exists-p claude-code-context-file)
    (delete-file claude-code-context-file)
    (message "Claude Code context cleared")))

(defun claude-code-context--update-context-timer ()
  "Timer function to update context periodically."
  (with-current-buffer (window-buffer (selected-window))
    (unless (minibufferp)
      (claude-code-context-update-context))))

(defun claude-code-context-mode-enable ()
  "Enable automatic context update timer."
  (unless claude-code-context-timer
    (setq claude-code-context-timer
          (run-with-idle-timer claude-code-context-update-interval t
                               #'claude-code-context--update-context-timer))
    (message "Claude Code context mode enabled")))

(defun claude-code-context-mode-disable ()
  "Disable automatic context update timer."
  (when claude-code-context-timer
    (cancel-timer claude-code-context-timer)
    (setq claude-code-context-timer nil)
    (message "Claude Code context mode disabled")))

(defvar claude-code-context-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'claude-code-context-update-context)
    (define-key map (kbd "d") #'claude-code-context-add-diagnostics)
    (define-key map (kbd "c") #'claude-code-context-clear-context)
    (define-key map (kbd "m") #'claude-code-context-mode)
    map)
  "Keymap for claude-code-context commands.")

(defvar claude-code-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") claude-code-context-command-map)
    map)
  "Keymap for `claude-code-context-mode'.")

;;;###autoload
(define-minor-mode claude-code-context-mode
  "Minor mode for automatic Claude Code context updates."
  :global t
  :lighter " CC"
  :keymap claude-code-context-mode-map
  :group 'claude-code-context
  (if claude-code-context-mode
      (claude-code-context-mode-enable)
    (claude-code-context-mode-disable)))

(provide 'claude-code-context)
;;; claude-code-context.el ends here
