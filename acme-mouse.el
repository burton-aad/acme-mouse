;;; acme-mouse.el --- mouse-button chording

;; Author: Alexandre Adolphe, Alex Kritikos (my gmail.com username is alex.kritikos)

;; Description:

;; The Acme editor from Plan 9 has very nice two-button mouse chords
;; for cut, copy and paste, and a handy right-click "find"
;; function. Please see http://swtch.com/plan9port/man/man1/acme.html
;; and http://plan9.bell-labs.com/sys/doc/acme/acme.pdf for more
;; information on how the chords are used. This package aims to work
;; the same as much as makes sense for Emacs.

;; Terms:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; acme-mouse-org.el

;; Todo:

;; Allow drag-highlighting with the right button (secondary selection?)

;; Make this a proper minor mode without clobbering keymaps and global
;; settings

;; Clicks in the gutter/margin don't seem to work right

;; Sometimes the region is active after the command, when we don't
;; want it to be. Leads to large highlighted regions. Goes away after
;; a click.

(require 'cl)

(defgroup acme-mouse nil
  "Acme mouse chording mode for Emacs"
  :group 'mouse)

(defcustom acme-mouse-select-button (kbd "<mouse-1>")
  "Define the mouse button used to select region and place the cursor (acme left click)."
  :type 'key-sequence
  :group 'acme-mouse)

(defcustom acme-mouse-exec-button (kbd "<mouse-2>")
  "Define the mouse button to evaluate text as code (acme middle click)."
  :type 'key-sequence
  :group 'acme-mouse)

(defcustom acme-mouse-search-button (kbd "<mouse-3>")
  "Define the mouse button to search for text (acme right click)."
  :type 'key-sequence
  :group 'acme-mouse)


;; Acme mouse chording doesn't make much sense without
;; delete-selection mode
(delete-selection-mode t)
;; Acme doesn't set the selection until you explicitly copy
(setq mouse-drag-copy-region nil)
(setq acme-mouse-state 'none)
(setq acme-last-command 'none)

;; everything starts with this function, so set state accordingly
(defun acme-down-mouse-1 (click)
  (interactive "e")
  ;; which buttons are currently pressed?
  (setq acme-mouse-state 'left)
  (setq acme-last-command 'none)
  (mouse-set-mark click)
  ;; pass to regular click handler
  (mouse-drag-region click))

;; called if mouse doesn't move between button down and up
(defun acme-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'none)
  (setq deactivate-mark nil)
  (if (eq acme-last-command 'none)
      (mouse-set-point click))
  (setq transient-mark-mode (cons 'only t))
  (setq acme-last-command 'none))

(defun acme-double-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'none)
  (setq deactivate-mark nil)
  ;; if we were just selecting, and not chording, pass to the regular click handler
  (if (eq acme-last-command 'none)
      (progn (mouse-set-point click)
             (acme-select-region)))
  (setq transient-mark-mode (cons 'only t))
  (setq acme-last-command 'none))

;; called if mouse moves between button down and up
(defun acme-drag-mouse-1 (click)
  (interactive "e")
  (if (eq acme-last-command 'none)
      (mouse-set-region click))
  (acme-mouse-1 click))

(defun acme-select-region ()
  (let ((range (mouse-start-end (mark) (point) mouse-selection-click-count)))
    (setq acme-start (car range))
    (setq acme-end (nth 1 range))
    (set-mark acme-start)
    (goto-char acme-end)))

(defun acme-down-mouse-2 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left)
      (progn (setq acme-mouse-state 'left-middle)
             (if (eq acme-last-command 'paste)
                 (undo)
               (mouse-set-point click)
               (acme-select-region)
               (kill-region (mark) (point)))
             (setq acme-last-command 'cut))
    (popup-menu (mouse-menu-major-mode-map) click)))

(defun acme-mouse-2 ()
  (interactive)
  (if (eq acme-mouse-state 'left-middle)
      (setq acme-mouse-state 'left)))

(defun acme-down-mouse-3 (click arg)
  (interactive "e\nP")
  (if (eq acme-mouse-state 'left)
      (progn
        (setq acme-mouse-state 'left-right)
        (case acme-last-command
          ('none (mouse-set-point click)
                 (acme-select-region)
                 (delete-region (mark) (point))
                 (yank arg))
          ('cut (undo)
                (set-mark acme-start)
                (goto-char acme-end)))
        (setq acme-last-command 'paste)
        (setq deactivate-mark nil)
        (activate-mark))))

(defun acme-mouse-3 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left-right)
      (setq acme-mouse-state 'left)
    (acme-search click)))

;; Search - modified from Dan McCarthy's acme-search.el

(defun header-line-active-p ()
  (not (null header-line-format)))

(defun move-mouse-to-point ()
  "Move the mouse pointer to point in the current window."
  (let* ((coords (posn-col-row (posn-at-point)))
	 (window-coords (window-inside-edges))
	 (x (+ (car coords) (car window-coords) -1)) ;the fringe is 0
	 (y (+ (cdr coords) (cadr window-coords)
	       (if (header-line-active-p)
		   -1
		 0))))
    (set-mouse-position (selected-frame) x y)))

(defun acme-highlight-search (sym)
  "Set the region to the current search result. Assume point is
at the end of the result."
  (set-mark (point))
  (search-backward sym nil t)
  (exchange-point-and-mark))

(defun acme-search (posn)
  "Search forward for the symbol under mouse, moving mouse and point forward.
This is inspired by Rob Pike's Acme."
  (let ((sym (if (region-active-p)
                 (buffer-substring (mark) (point))
               (mouse-set-point posn)
               (thing-at-point 'filename))))
    (if (file-readable-p sym)
        (special-display-popup-frame (find-file-noselect sym nil nil nil))
      (if (search-forward sym nil t)
          (acme-highlight-search sym)
        (let ((saved-point (point)))
          (message "Wrapped search")
          (goto-char (point-min))
          (if (search-forward sym nil t)
              (acme-highlight-search sym)
            (goto-char saved-point)))))
    ;;Redisplay the screen if we search off the bottom of the window.
    (unless (posn-at-point)
      (universal-argument)
      (recenter))
    (move-mouse-to-point)))


;; (global-set-key [(double-drag-mouse-1)] 'acme-double-mouse-1)
;; (global-set-key [(triple-drag-mouse-1)] 'acme-double-mouse-1)

(defvar acme-mouse-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<down-mouse-1>") 'acme-down-mouse-1)
    (define-key keymap (kbd "<down-mouse-2>") 'acme-down-mouse-2)
    (define-key keymap (kbd "<down-mouse-3>") 'acme-down-mouse-3)
    (define-key keymap acme-mouse-select-button 'acme-mouse-1)
    (define-key keymap acme-mouse-exec-button 'acme-mouse-2)
    (define-key keymap acme-mouse-search-button 'acme-mouse-3)
    (define-key keymap (kbd "<double-mouse-1>") 'acme-double-mouse-1)
    ;; (define-key keymap (kbd "<double-mouse-2>") (acme-mouse-make-transition up middle))
    ;; (define-key keymap (kbd "<double-mouse-3>") (acme-mouse-make-transition up right))
    (define-key keymap (kbd "<triple-mouse-1>") 'acme-double-mouse-1)
    ;; (define-key keymap (kbd "<triple-mouse-2>") (acme-mouse-make-transition up middle))
    ;; (define-key keymap (kbd "<triple-mouse-3>") (acme-mouse-make-transition up right))
    (define-key keymap (kbd "<drag-mouse-1>") 'acme-drag-mouse-1)
    ;; (define-key keymap (kbd "<drag-mouse-2>") (acme-mouse-make-transition drag middle))
    ;; (define-key keymap (kbd "<drag-mouse-3>") (acme-mouse-make-transition drag right))
    keymap)
  "Keymap for `acme-mouse` mode.")

;;;###autoload
(define-minor-mode acme-mouse
  "Acme mouse mode enables the button actions of Acme:
  * Chording left and middle cuts the region
  * Chording left and middle pastes at point
  * Clicking with middle evaluates code at point
  * Clicking with right searches word at point
  * Dragging with middle evaluates selected region
  * Dragging with right searches region"
  nil
  " Acme-Mouse"
  acme-mouse-map
  :group 'acme-mouse
  :global t)

(provide 'acme-mouse)
