- [Description](#orgcde4888)
- [History](#orgc5f2d31)
- [Missing features](#org08beae9)
- [Bugs](#orgee615b2)
- [License](#org81e242b)
  - [GPLv2+](#org8535ec3)
- [Implementation](#org6a3d144)
  - [Acme-mouse library](#org84afcf4)
    - [Global variables](#orgc21c0e5)
    - [Basic state-machine](#org30facd4)
    - [State-machine driver](#org5a1c80d)
    - [Extending the state-machine](#orgb8aac47)
    - [Selection faces](#orgc633530)
    - [Library](#orgbaddb5e)
  - [Fundamental acme-mouse](#org1c6fd41)
    - [State machine](#orga366467)
    - [Acme functionality](#orgc99935d)
    - [Keymap](#orgb7a14ce)
    - [Minor mode](#orgb3cd269)


<a id="orgcde4888"></a>

# Description

This implements the Acme-style chording features, left-middle to cut; left-right to paste; middle click/sweep to execute; and right click/sweep to search, or open a file.

To compile this file: open in Emacs in org-mode, and tangle (C-c C-v C-t), then `(load-file "acme-mouse-new.el")` it. This defines the minor-mode `acme-mouse`. Or perhaps just do `M-: (org-babel-execute-buffer)`, but you might want to `M-: (setq org-confirm-babel-evaluate nil)` first.


<a id="orgc5f2d31"></a>

# History

This is a newly written software, but heavily inspired by <https://github.com/akrito/acme-mouse> by Alex Kritikos. That in turn cites Dan McCarthy's acme-search.el, which is perhaps the least different feature of this too, except I use pixel positions. This is because I use variable-width fonts (like Acme does, Google Noto in case you are wondering).


<a id="org08beae9"></a>

# Missing features

-   **Default left-mouse up binding:** The left-mouse button click should be taken from the major mode, e.g. at the moment when we click on a link in an info page.
    
    Perhaps this could also be used for right-button clicks, when they didn't click in a region, or dragged a region? (And similarly for middle-mouse button). The macro could possibly grab the current binding, if it is evaluated before its result is assigned.

-   **Mode-specific functions:** We should change eval based on the mode (e.g. Scheme/Standard ML/etc for the current REPL). Also, loading files based on library path/#include/(require 'feature) would be sweet! And these should be split out into a different file, as they are irrelevant to the central mechanism. And don't forget the paredit cut.

-   **Don't copy on look select/paste:** This is a major bug at the moment.

-   **Working with evil-mode:** I'd like to give evil-mode a try, but I know at the moment this doesn't work with evil-mode.

-   **Pipe-eval:** Having something selected, then chording middle-left pipes the selection into the chorded (selected or at point) word. For shell, the STDIN is probably good enough, but not sure what is the best action to take for Elisp. Perhaps if it is `commandp`, call-interactively it, and hope it uses the region, if it is `functionp` call it with string/sexp/start/end?
    
    Also, consider Acme's use of >cmd, |cmd and <cmd.

-   **Middle-mouse emulation:** Not sure if this should be separate, but emulate the middle-mouse button with shift-right click. My acme actually uses a shift-press as a right-mouse click, and a control-press as a middle-mouse click, but apparently this might require Emacs C source changes, as plain modifier presses cannot be bound.

-   **Emulate editable Tag:** In acme, there is an editable tag window (which contains the file name, what I suppose would be called the mode line in Emacs). It would be useful to have something similar here too.

-   **More features?:** Re-read <https://research.swtch.com/acme.pdf>

-   **Exchange primary and secondary selection:** This is not something that Acme actually has, but I would find this extremely useful.


<a id="orgee615b2"></a>

# Bugs

-   **Bug with undo-tree:** When using undo-tree, sometimes the undo breaks, when you try to undo past a left-middle; left-right chord. This only happens if you haven't opened the undo-tree buffer before starting to undo past the acme-mouse's undo. Probably just not using undo would fix it. Or possibly using undo-tree-undo when you have undo-tree, which is what is implemented currently, but needs more testing.


<a id="org81e242b"></a>

# License


<a id="org8535ec3"></a>

## GPLv2+

Copyright (C) 2018 Robert Kovacsics

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


<a id="org6a3d144"></a>

# Implementation


<a id="org84afcf4"></a>

## Acme-mouse library


<a id="orgc21c0e5"></a>

### Global variables

```elisp
;; -*- lexical-binding: t -*-

(defvar acme-mouse-state 'none
  "The state of the chording state machine.")

(defvar acme-mouse-remapping nil
  "The remapping applied to the region face.")

(defvar acme-mouse-start-click nil
  "The start click of the transition from 'none state, set to
  nil on a transition back to 'none state.")

(defvar acme-mouse-saved-mark-point-active nil
  "The saved mark, point and mark-active variables, so that they
  can be restored when the user finishes dragging the eval/search
  selection. Set on a transition from a 'none state just like
  acme-mouse-start-click.")
```


<a id="org30facd4"></a>

### Basic state-machine

These are the default bindings.

```elisp
(setq acme-mouse-transition-table
      `((none (down left) left mouse-drag-region)
        (none (down middle) middle)
        (none (down right) right)

        (left (down middle) left-middle)
        (left (down right) left-right)

        (middle (down left) middle-left)

        ;; Mouse up
        (left (up left) none mouse-set-point)
        (middle (up middle) none mouse-yank-primary)
        (right (up right) none mouse-save-then-kill)

        (left-middle (up middle) left)
        (left-right (up right) left)

        (middle-left (up left) middle)

        ;; Mouse drag up
        (left (drag left) none mouse-set-region)
        (middle (drag middle) none)
        (right (drag right) none)

        (left-middle (drag middle) left)
        (left-right (drag right) left)

        (middle-left (drag left) middle)))
```


<a id="org5a1c80d"></a>

### State-machine driver

If we only change the state, then we want to do that transparently, hence setting the `last-command`.

```elisp
(defmacro acme-mouse-make-transition (&rest input)
  `(lambda (click)
     (interactive "e")
     (setq this-command last-command)   ; Transparent
     (when (eq acme-mouse-state 'none)
       (setq acme-mouse-start-click click)
       (acme-mouse-save-mark-point-active))
     (cond
      ,@(cl-reduce (lambda (acc transition)
                     (pcase transition
                       (`(,start ,(pred (equal input)) ,end . ,funs)
                        (cons
                         `((equal acme-mouse-state ',start)
                           (setq acme-mouse-state ',end)
                           ,@(apply 'append
                                    (cl-mapcar
                                     (lambda (f)
                                       (if (commandp f t)
                                           `((setq this-command ',f)
                                             (call-interactively ',f))
                                         `(,f)))
                                     funs)))
                         acc))
                       (_ acc)))
                   acme-mouse-transition-table
                   :initial-value nil)
      (t (setq acme-mouse-state 'none)))
     (when (eq acme-mouse-state 'none)
       (acme-mouse-face-unmap)
       (setq acme-mouse-start-click nil))))

(defun acme-mouse-save-mark-point-active ()
  (setq acme-mouse-saved-mark-point-active (list (mark) (point) mark-active)))

(defun acme-mouse-restore-mark-point-active ()
  (seq-let [mark point active] acme-mouse-saved-mark-point-active
    (when mark (set-mark mark))
    (when point (goto-char point))
    (setq mark-active active)))
```


<a id="orgb8aac47"></a>

### Extending the state-machine

```elisp
(defun acme-mouse-lookup (start transition end)
  (let (ret)
    (dolist (el acme-mouse-transition-table
                (list start transition end))
      (setq ret el))))

(defun acme-mouse-add (start transition end &rest actions)
  (let ((entry (acme-mouse-lookup start transition end)))
    (setcdr (cddr entry)
            (append (cdddr entry) actions))))

(defun acme-mouse-del (start transition end &rest actions)
  (let ((entry (acme-mouse-lookup start transition end)))
    (if actions
        ;; remove-actions
        (seq-map
         (lambda (action)
           (setcdr (cddr entry)
                   (seq-filter (lambda (action) (equal actions))
                               (cdddr entry))))
         actions)
      ;; remove-all
      (setcdr (cddr entry) nil))))
```


<a id="orgc633530"></a>

### Selection faces

```elisp
(defun acme-mouse-face-unmap ()
  (when acme-mouse-remapping
    (face-remap-remove-relative acme-mouse-remapping)
    (setq acme-mouse-remapping nil)))

(defun acme-mouse-face-remap (face)
  (acme-mouse-face-unmap)
  (setq acme-mouse-remapping (face-remap-add-relative 'region face)))

(defun acme-mouse-selection (click)
  (let* ((start (posn-point (event-start acme-mouse-start-click)))
         (end (posn-point (event-start click)))
         (clicks (event-click-count acme-mouse-start-click)))
    (mouse-start-end start end (1- clicks))))

(defface acme-mouse-face-eval
  '((((class color) (min-colors 8))
     :inverse-video t :foreground "dark red")
    (t :inverse-video t))
  "Face for selecting with the middle mouse button."
  :group 'acme-mouse
  :group 'faces)

(defface acme-mouse-face-search
  '((((class color) (min-colors 8))
     :inverse-video t :foreground "dark green")
    (t :inverse-video t))
  "Face for selecting with the right mouse button."
  :group 'acme-mouse
  :group 'faces)
```


<a id="orgbaddb5e"></a>

### Library

```elisp
(defgroup acme-mouse nil
  "Acme mouse chording mode for Emacs"
  :group 'mouse)

(provide 'acme-mouse)
```


<a id="org1c6fd41"></a>

## Fundamental acme-mouse


<a id="orga366467"></a>

### State machine

The format of the state machine is `(start symbol next statements...)` where the statements are either a variable mapping to a command (e.g. `mouse-drag-region`), in which case they get `call-interactively`'d otherwise they are executed as a statement.

```elisp
;; -*- lexical-binding: t -*-

(require 'acme-mouse)

(defmacro region-or-click (region-cmd click-cmd)
  `(lambda (click)
     (interactive "e")
     (seq-let [mark point active] acme-mouse-saved-mark-point-active
       (let ((lo (min (or mark point) point)) (hi (max (or mark point) point)))
         (if (and active (<= lo (posn-point (event-end click)) hi))
             (progn
               (acme-mouse-restore-mark-point-active)
               (funcall-interactively ,region-cmd click lo hi))
           (call-interactively ,click-cmd))))))

;; Transitions
(acme-mouse-add 'none '(down middle) 'middle
                (acme-mouse-face-remap 'acme-mouse-face-eval) ;; TODO: remap guard!
                mouse-drag-region)
(acme-mouse-add 'none '(down right) 'right
                (acme-mouse-face-remap 'acme-mouse-face-search)
                mouse-drag-region)

(acme-mouse-add 'left '(down middle) 'left-middle
                acme-mouse-cut)
(acme-mouse-add 'left '(down right) 'left-right
                acme-mouse-paste)

(acme-mouse-add 'middle '(up middle) 'none
                mouse-set-point
                (region-or-click 'acme-mouse-eval-region 'acme-mouse-eval-click))
(acme-mouse-add 'right '(up right) 'none
                mouse-set-point
                (region-or-click 'acme-mouse-search-region 'acme-mouse-search-click))

(acme-mouse-add 'middle-left '(up left) 'middle
                acme-mouse-eval-pipe-region)

(acme-mouse-add 'middle '(drag middle) 'none
                acme-mouse-eval-region)
(acme-mouse-add 'right '(drag right) 'none
                acme-mouse-search-region)
```


<a id="orgc99935d"></a>

### Acme functionality

1.  Cut/paste

    ```elisp
    (defun acme-mouse-cut (click)
      (interactive "e")
      (if (eq last-command 'yank)
          (undo)
        (apply 'kill-region (acme-mouse-selection click))))
    
    (defun acme-mouse-paste (click)
      (interactive "e")
      (if (eq last-command 'kill-region)
          (undo)
        (yank)))
    ```

2.  Eval

    ```elisp
    (defun acme-mouse-eval-region (click lo hi)
      (interactive "e\nr")
      (eval-expression (read (buffer-substring (mark) (point))))
      (acme-mouse-restore-mark-point-active))
    
    (defun acme-mouse-eval-click (click)
      (interactive "e")
      (let ((sexp-at-point (thing-at-point 'sexp)))
        (if (sexp-at-point)
            (eval-expression (sexp-at-point))
          (elisp--eval-last-sexp nil)))
      (acme-mouse-restore-mark-point-active))
    ```

3.  Search

    ```elisp
    (defun acme-mouse-search-region (click lo hi)
      (interactive "e\nr")
      (let ((word (buffer-substring lo hi)))
        (unless (search-forward word nil t)
          (goto-char (point-min))
          (search-forward word nil t)
          (acme-mouse-highlight-search word))
        (acme-mouse-highlight-search word)))
    
    (defun acme-mouse-search-click (click)
      (interactive "e")
      (let ((file (thing-at-point 'filename))
            (word (thing-at-point 'word))
            (saved-point (point)))
        (cond ((and file (file-readable-p file))
               (special-display-popup-frame
                (find-file-noselect file)))
              ((and word (search-forward word nil t))
               (acme-mouse-highlight-search word))
              ((and word
                    (goto-char (point-min))
                    (search-forward word nil t))
               (message "Wrapped search")
               (acme-mouse-highlight-search word)))))
    
    (defun acme-mouse-highlight-search (str)
      "Set the region to the current search result. Assumes point is
    at the end of the result."
      (set-mark (point))
      (search-backward str nil t)
      (exchange-point-and-mark)
      (unless (posn-at-point)
        (recenter))
      (let* ((x-y (posn-x-y (posn-at-point)))
             (edges (window-inside-pixel-edges))
             (width (default-font-width))
             (height (default-font-height))
             (x (+ (car x-y) (nth 0 edges) (- (/ width 2))))
             (y (+ (cdr x-y) (nth 1 edges) (/ height 2))))
        (set-mouse-pixel-position (selected-frame) x y)))
    ```


<a id="orgb7a14ce"></a>

### Keymap

```elisp
;; TODO: This could be generated automatically
(defvar acme-mouse-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [(down-mouse-1)] (acme-mouse-make-transition down left))
    (define-key keymap [(down-mouse-2)] (acme-mouse-make-transition down middle))
    (define-key keymap [(down-mouse-3)] (acme-mouse-make-transition down right))
    (define-key keymap [(mouse-1)] (acme-mouse-make-transition up left))
    (define-key keymap [(mouse-2)] (acme-mouse-make-transition up middle))
    (define-key keymap [(mouse-3)] (acme-mouse-make-transition up right))
    (define-key keymap [(double-mouse-1)] (acme-mouse-make-transition up left))
    (define-key keymap [(double-mouse-2)] (acme-mouse-make-transition up middle))
    (define-key keymap [(double-mouse-3)] (acme-mouse-make-transition up right))
    (define-key keymap [(triple-mouse-1)] (acme-mouse-make-transition up left))
    (define-key keymap [(triple-mouse-2)] (acme-mouse-make-transition up middle))
    (define-key keymap [(triple-mouse-3)] (acme-mouse-make-transition up right))
    (define-key keymap [(drag-mouse-1)] (acme-mouse-make-transition drag left))
    (define-key keymap [(drag-mouse-2)] (acme-mouse-make-transition drag middle))
    (define-key keymap [(drag-mouse-3)] (acme-mouse-make-transition drag right))
    keymap)
  "Keymap for `acme-mouse` mode.")
;; By something like
(acme-mouse-generate-keymap
 "Keymap for 'acme-mouse-fundamental' mode.")
```


<a id="orgb3cd269"></a>

### Minor mode

```elisp
;;;###autoload
(define-minor-mode acme-mouse-fundamental
  "Acme mouse mode enables the button actions of Acme:
  * Chording left and middle cuts the region
  * Chording left and middle pastes at point
  * Clicking with middle evaluates elisp sexp before point
  * Clicking with right searches word at point
  * Dragging with middle evaluates selected region as elisp expression
  * Dragging with right searches region"
  nil
  " Acme-Mouse"
  acme-mouse-fundamental-map
  :group 'acme-mouse
  :global t)

(provide 'acme-mouse-fundamental)
```