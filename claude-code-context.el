;;; claude-code-context.el --- Share Emacs context with Claude Code -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Luke Hoersten

;; Author: Luke Hoersten <Luke@Hoersten.org>
;; URL: https://github.com/lhoersten/claude-code-context
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, convenience

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

;; This package automatically shares your current Emacs buffer context
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
;;                "command": "CONTEXT_FILE=\"$HOME/.emacs.d/claude-context.txt\"; if [ -f \"$CONTEXT_FILE\" ]; then echo \"\\n---\\n## Emacs Context\\n\"; cat \"$CONTEXT_FILE\"; echo \"\\n---\"; fi"
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

(defgroup claude-code-context nil
  "Share Emacs context with Claude Code."
  :group 'tools
  :prefix "claude-code-context-")

(defcustom claude-code-context-file
  (expand-file-name "claude-context.txt" user-emacs-directory)
  "File where Claude Code context is written."
  :type 'file
  :group 'claude-code-context)

(defcustom claude-code-context-update-interval 2
  "Seconds between context updates (to avoid excessive writes)."
  :type 'integer
  :group 'claude-code-context)

(defvar claude-code-context-timer nil
  "Timer for updating Claude Code context.")

(defun claude-code--get-current-context ()
  "Get current buffer context as a string."
  (when (buffer-file-name)
    (let* ((file (buffer-file-name))
           (line (line-number-at-pos))
           (col (current-column))
           (selection (when (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end)))))
      (concat
       (format "File: %s\n" file)
       (format "Line: %d, Column: %d\n" line col)
       (when selection
         (format "Selection:\n```\n%s\n```\n" selection))))))

(defun claude-code--get-flymake-diagnostics ()
  "Get flymake diagnostics for current buffer."
  (when (and (bound-and-true-p flymake-mode)
             (buffer-file-name))
    (let ((diags (flymake-diagnostics)))
      (when diags
        (concat
         "\nFlymake Diagnostics:\n"
         (mapconcat
          (lambda (diag)
            (format "  %4d %8s  %-8s  %s"
                    (line-number-at-pos (flymake-diagnostic-beg diag))
                    (flymake-diagnostic-type diag)
                    (flymake-diagnostic-backend diag)
                    (flymake-diagnostic-text diag)))
          diags
          "\n"))))))

(defun claude-code-update-context ()
  "Update Claude Code context file with current buffer state."
  (interactive)
  (let ((context (claude-code--get-current-context)))
    (when context
      (with-temp-file claude-code-context-file
        (insert "# Emacs Context for Claude Code\n")
        (insert "# This file is automatically updated by Emacs\n\n")
        (insert context))
      (message "Claude Code context updated"))))

(defun claude-code-add-diagnostics ()
  "Add flymake diagnostics to Claude Code context file."
  (interactive)
  (let ((context (claude-code--get-current-context))
        (diags (claude-code--get-flymake-diagnostics)))
    (when context
      (with-temp-file claude-code-context-file
        (insert "# Emacs Context for Claude Code\n")
        (insert "# This file is automatically updated by Emacs\n\n")
        (insert context)
        (when diags
          (insert diags)))
      (message "Claude Code context updated with diagnostics"))))

(defun claude-code-clear-context ()
  "Clear the Claude Code context file."
  (interactive)
  (when (file-exists-p claude-code-context-file)
    (delete-file claude-code-context-file)
    (message "Claude Code context cleared")))

(defun claude-code--update-context-timer ()
  "Timer function to update context periodically."
  (when (and (buffer-file-name)
             (not (minibufferp)))
    (claude-code-update-context)))

(defun claude-code-context-mode-enable ()
  "Enable automatic context updates."
  (unless claude-code-context-timer
    (setq claude-code-context-timer
          (run-with-idle-timer claude-code-context-update-interval t
                               #'claude-code--update-context-timer))
    (message "Claude Code context mode enabled")))

(defun claude-code-context-mode-disable ()
  "Disable automatic context updates."
  (when claude-code-context-timer
    (cancel-timer claude-code-context-timer)
    (setq claude-code-context-timer nil)
    (message "Claude Code context mode disabled")))

;;;###autoload
(define-minor-mode claude-code-context-mode
  "Minor mode for automatic Claude Code context updates."
  :global t
  :lighter " CC"
  :group 'claude-code-context
  (if claude-code-context-mode
      (claude-code-context-mode-enable)
    (claude-code-context-mode-disable)))

;; Keybindings
(defvar claude-code-context-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") 'claude-code-update-context)
    (define-key map (kbd "d") 'claude-code-add-diagnostics)
    (define-key map (kbd "c") 'claude-code-clear-context)
    (define-key map (kbd "m") 'claude-code-context-mode)
    map)
  "Keymap for claude-code-context commands.")

(global-set-key (kbd "C-c C-l") claude-code-context-command-map)

(provide 'claude-code-context)
;;; claude-code-context.el ends here
