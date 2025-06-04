;;; amber-mode.el --- Major mode for editing Amber source files -*- lexical-binding: t; -*-
;;
;; Filename: amber-mode.el
;; Description:
;; Author: Slava Barinov
;; Maintainer: rayslava@gmail.com
;; Created: Wed May 28 14:26:29 2025 (+0900)
;; Version: 0.2
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/rayslava/amber-mode
;; Keywords: languages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This package provides a major mode for editing Amber source files.
;; https://amber-lang.com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'compile)
(require 'ansi-color)

(defgroup amber nil
  "Major mode for editing Amber source code."
  :group 'languages)

(defcustom amber-indent-offset 4
  "Number of spaces for each indentation step in `amber-mode'."
  :type 'integer
  :safe 'integerp
  :group 'amber)

(defcustom amber-executable "amber"
  "Path to the Amber executable."
  :type 'string
  :group 'amber)

(defcustom amber-enable-flycheck t
  "Enable flycheck integration with Amber check."
  :type 'boolean
  :group 'amber)

(defcustom amber-strip-ansi-colors t
  "Strip ANSI color codes from amber command output."
  :type 'boolean
  :group 'amber)

(defcustom amber-mode-hook nil
  "Hook run when entering Amber Mode."
  :type 'hook
  :group 'amber)

;; Define keywords for syntax highlighting
(defconst amber-keywords
  '("and" "as" "break" "cd" "const" "continue" "echo" "else" "exit" "fail"
    "failed" "for" "from" "fun" "if" "import" "in" "is" "len" "let"
    "lines" "loop" "main" "mv" "nameof" "not" "or" "pub" "ref" "return"
    "silent" "status" "then" "trust" "unsafe"))

(defconst amber-types
  '("Text" "Num" "Bool" "Null"))

(defconst amber-constants
  '("true" "false" "null"))

(defvar amber-font-lock-keywords
  (let ((kw-re (regexp-opt amber-keywords 'words))
        (ty-re (regexp-opt amber-types 'words))
        (co-re (regexp-opt amber-constants 'words)))
    `(
      (,kw-re . font-lock-keyword-face)
      (,ty-re . font-lock-type-face)
      (,co-re . font-lock-constant-face)
      ("\\$\\([^$\n]+\\)\\$" . font-lock-string-face)  ; Command syntax $...$
      ("\\(fun\\s-+\\)\\([a-zA-Z_][a-zA-Z0-9_]*\\)" (2 font-lock-function-name-face)) ; Function definitions
      ("\\(\"\\)\\([^\"]*\\)\\(\"\\)" 0 font-lock-string-face)                       ; String literals
      ("\\`#!.*" . font-lock-comment-face)                                           ; Shebang lines at start of file
      )))

;; Syntax table
(defvar amber-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments start with // and end with newline
    (modify-syntax-entry ?/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; _ is part of a word
    (modify-syntax-entry ?_ "w" st)

    ;; String quotes
    (modify-syntax-entry ?\" "\"" st)

    ;; Brackets and parentheses
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    st))

;; Indent function
(defun amber-indent-line ()
  "Indent current line as Amber code."
  (interactive)
  (let ((indent-col 0)
        (pos (point))
        (beg (line-beginning-position)))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while (not (bobp))
            (forward-line -1)
            (beginning-of-line)
            (if (looking-at "^[ \t]*\\({\\|fun\\|if\\|else\\|for\\|loop\\)")
                (progn
                  (setq indent-col (+ (current-indentation) amber-indent-offset))
                  (throw 'break nil))
              (if (and (looking-at "^[ \t]*}") (not (looking-at "^[ \t]*}.*$")))
                  (progn
                    (setq indent-col (current-indentation))
                    (throw 'break nil))
                (if (not (looking-at "^[ \t]*$"))
                    (progn
                      (setq indent-col (current-indentation))
                      (throw 'break nil))))))
        (error nil)))

    ;; Reduce indentation for closing brackets
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^[ \t]*\\(}\\|]\\|)\\)")
        (setq indent-col (max 0 (- indent-col amber-indent-offset)))))

    ;; Apply indentation
    (if (not (= (current-indentation) indent-col))
        (progn
          (delete-horizontal-space)
          (indent-to indent-col)))))

;; Amber command-line integration for compilation-mode
(defun amber--project-root ()
  "Find the root directory of the current Amber project."
  (let ((file (buffer-file-name)))
    (when file
      (let ((dir (file-name-directory file)))
        (locate-dominating-file dir "amber.toml")))))

;; ANSI color handling
(defun amber--strip-ansi-from-command (command)
  "Add env variables to strip ANSI codes from COMMAND if needed."
  (if amber-strip-ansi-colors
      (concat "NO_COLOR=1 AMBER_NO_COLOR=1 " command)
    command))

;; Compilation commands
(defun amber-compile-file ()
  "Compile the current Amber file."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (compile (amber--strip-ansi-from-command
               (concat amber-executable " run " (shell-quote-argument file-name)))))))

(defun amber-compile-project ()
  "Compile the current Amber project."
  (interactive)
  (let ((root (or (amber--project-root) default-directory)))
    (let ((default-directory root))
      (compile (amber--strip-ansi-from-command
               (concat amber-executable " build"))))))

(defun amber-run-file ()
  "Run the current Amber file."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (compile (amber--strip-ansi-from-command
               (concat amber-executable " run " (shell-quote-argument file-name)))))))

(defun amber-check-file ()
  "Check the current Amber file for errors."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (compile (amber--strip-ansi-from-command
               (concat amber-executable " check " (shell-quote-argument file-name)))))))

;; Add amber commands to compilation-command completions
(with-eval-after-load 'compile
  ;; Add support for ansi-color in compilation buffer
  (add-hook 'compilation-filter-hook 'amber--colorize-compilation-buffer)

  ;; Add error patterns
  (add-to-list 'compilation-error-regexp-alist-alist
               '(amber "^\\s-*ERROR\\s-+\\(.*\\)[\r\n]+at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                       2 3 4 2))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(amber-ansi "ERROR\\s-+\\(.*\\)[\r\n]+at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                       2 3 4 2))
  (add-to-list 'compilation-error-regexp-alist 'amber)
  (add-to-list 'compilation-error-regexp-alist 'amber-ansi))

;; Handle ANSI color codes in compilation buffer
(defun amber--colorize-compilation-buffer ()
  "Remove ANSI escape sequences from compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

;; Custom error parsing function for flycheck
(defun amber-flycheck-parse-errors (output checker buffer)
  "Parse Amber errors from OUTPUT.
CHECKER and BUFFER denote the checker and buffer for which the errors are parsed."
  (let ((errors nil)
        (file nil)
        (line nil)
        (column nil)
        (msg nil)
        (in-error nil)
        (raw-lines (split-string output "\n")))

    ;; Process the output line by line
    (dolist (line-text raw-lines)
      (let ((clean-line (ansi-color-filter-apply line-text)))
        (cond
         ;; Starting a new error - try both with and without ANSI color codes
         ((or (string-match "^ ERROR\\s-+\\(.*\\)$" clean-line)
              (string-match "ERROR\\s-+\\(.*\\)$" clean-line))
          (setq in-error t
                msg (match-string 1 clean-line)
                file nil
                line nil
                column nil))

         ;; Location line
         ((and in-error (string-match "^at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" clean-line))
          (setq file (match-string 1 clean-line)
                line (string-to-number (match-string 2 clean-line))
                column (string-to-number (match-string 3 clean-line)))

          ;; Create error object now that we have all the info
          (when (and file line column msg)
            (push (flycheck-error-new-at
                   line column 'error msg
                   :checker 'amber-check
                   :buffer buffer
                   :filename file)
                  errors)
            (setq in-error nil)))

         ;; Additional context lines - append to message if we're processing an error
         ((and in-error msg)
          (setq msg (concat msg "\n" clean-line))))))

    ;; Return the errors in the order they were found
    (nreverse errors)))

;; Register the Amber flycheck syntax checker
(defun amber-register-flycheck-checker ()
  "Register the amber-check syntax checker with flycheck."
  (flycheck-define-checker amber-check
    "An Amber syntax checker using the amber check command."
    :command ("env" "NO_COLOR=1" "AMBER_NO_COLOR=1" "amber" "check" source)
    :error-parser amber-flycheck-parse-errors
    :modes amber-mode
    :standard-input nil)

  (add-to-list 'flycheck-checkers 'amber-check))

;; Flycheck integration (if available)
(defun amber-setup-flycheck ()
  "Set up flycheck for Amber code."
  (when (and amber-enable-flycheck (featurep 'flycheck))
    ;; Enable flycheck
    (when (fboundp 'flycheck-mode)
      (flycheck-mode 1))))

;; Initialize flycheck if available
(with-eval-after-load 'flycheck
  (amber-register-flycheck-checker))

;; Key bindings
(defvar amber-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Compilation commands
    (define-key map (kbd "C-c C-c") 'amber-compile-file)
    (define-key map (kbd "C-c C-p") 'amber-compile-project)
    (define-key map (kbd "C-c C-r") 'amber-run-file)
    (define-key map (kbd "C-c C-k") 'amber-check-file)
    map)
  "Keymap for Amber major mode.")

;;;###autoload
(define-derived-mode amber-mode prog-mode "Amber"
  "Major mode for editing Amber language source files."
  :syntax-table amber-mode-syntax-table

  ;; Font lock
  (setq font-lock-defaults '(amber-font-lock-keywords))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")

  ;; Indentation
  (setq-local indent-line-function 'amber-indent-line)

  ;; Set compilation-mode-hook to handle file paths
  ;; Make compilation work with amber files in non-project directories
  (with-eval-after-load 'compile
    (add-hook 'compilation-filter-hook 'amber--parse-compilation-output))

  ;; Flycheck integration (if available)
  (when (featurep 'flycheck)
    (amber-setup-flycheck)))

;; Function to process compilation output to handle non-project file paths
(defun amber--parse-compilation-output ()
  "Parse the compilation output to handle Amber error locations."
  (save-excursion
    (goto-char compilation-filter-start)
    ;; Handle error lines with file paths that compilation-mode didn't catch
    (while (re-search-forward "^at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" nil t)
      (let ((file (match-string 1))
            (line (string-to-number (match-string 2)))
            (column (string-to-number (match-string 3))))
        (when (and file line column)
          (let ((map (make-sparse-keymap)))
            (define-key map [mouse-2]
              #'(lambda (event)
                  (interactive "e")
                  (let ((window (posn-window (event-end event))))
                    (with-selected-window window
                      (find-file-other-window file)
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (forward-char (1- column))))))
            (let ((end-pos (point)))
              (put-text-property (match-beginning 0) end-pos 'mouse-face 'highlight)
              (put-text-property (match-beginning 0) end-pos 'keymap map))))))))

;; Add hook to setup flycheck when it's loaded
(eval-after-load 'flycheck
  '(add-hook 'amber-mode-hook 'amber-setup-flycheck))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ab\\'" . amber-mode))

(provide 'amber-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; amber-mode.el ends here
