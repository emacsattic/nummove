;;; NumMove --- A simple number-move-game

;; Copyright (C) 2007  Kristian Rumberg (kristianrumberg@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(setq nummove-path-to-images "~/source/nummove/")

(defun nummove-nada()(interactive))

(defun nummove-wincheck()
  (interactive)

  (when (not nummove-is-automode)

  (let (x y done)
    (setq x 0)
    (setq y 0)
    (setq done t)

    (while (< y 4)
      (while (< x 4)

	(when (not (eq (car (nummove-get-boardentry x y)) (+ (* 4 y) x 1)))
	  (setq done nil)
	  )

	(setq x (+ x 1))
	)
      (setq y (+ y 1))
      (setq x 0)
      )

    (when done
      (define-key nummove-mode-map [left]    'nummove-nada)
      (define-key nummove-mode-map [right]   'nummove-nada)
      (define-key nummove-mode-map [up]      'nummove-nada)
      (define-key nummove-mode-map [down]    'nummove-nada)
      (message (concat "You made it in " (number-to-string nummove-moves) " moves :) Press 'r' to play again"))
    )
  )
)
)

(defun nummove-try-inc-move-counter()
  (when (not nummove-is-automode)
    (setq nummove-moves (+ nummove-moves 1))
    (message (concat (number-to-string nummove-moves) " moves performed"))
  )
)

(defun nummove-get-boardentry(x y)
  (aref (aref nummove-numlist y) x)
  )

(defun nummove-set-boardentry(x y value)
  (aset (aref nummove-numlist y) x value)
  )

(defun nummove-swap-boardentry(xfrom yfrom xto yto) 

  (let (tmp) 
    (setq tmp (nummove-get-boardentry xto yto))
    (nummove-set-boardentry xto yto (nummove-get-boardentry xfrom yfrom))
    (nummove-set-boardentry xfrom yfrom tmp)
    )
  )

(defun nummove-move-right()
  (interactive)
  (nummove-try-inc-move-counter)
  (unless (eq nummove-empty-x 0)
    (nummove-swap-boardentry nummove-empty-x nummove-empty-y (- nummove-empty-x 1) nummove-empty-y)
    (setq nummove-empty-x (- nummove-empty-x 1))
    (nummove-draw-board)
   )
  (nummove-wincheck)
  )
  
(defun nummove-move-left()
  (interactive)
  (nummove-try-inc-move-counter)

  (unless (eq nummove-empty-x 3)
    (nummove-swap-boardentry nummove-empty-x nummove-empty-y (+ nummove-empty-x 1) nummove-empty-y)
    (setq nummove-empty-x (+ nummove-empty-x 1))
    (nummove-draw-board)
   )
  (nummove-wincheck)
  )

(defun nummove-move-down()
  (interactive)
  (nummove-try-inc-move-counter)

  (unless (eq nummove-empty-y 0)
    (nummove-swap-boardentry nummove-empty-x nummove-empty-y nummove-empty-x (- nummove-empty-y 1) )
    (setq nummove-empty-y (- nummove-empty-y 1))
    (nummove-draw-board)
   )
  (nummove-wincheck)
  )

(defun nummove-move-up()
  (interactive)
  (nummove-try-inc-move-counter)

  (unless (eq nummove-empty-y 3)
    (nummove-swap-boardentry nummove-empty-x nummove-empty-y nummove-empty-x (+ nummove-empty-y 1) )
    (setq nummove-empty-y (+ nummove-empty-y 1))
    (nummove-draw-board)
   )
  (nummove-wincheck)
  )
  
(defun nummove-init-buffer()
  (set-buffer (get-buffer-create "*NumMove*"))
  (switch-to-buffer "*NumMove*")
  )

(defun nummove-init-keys()
  
  (defvar nummove-mode-map (make-sparse-keymap 'nummove-mode-map))
  
  (define-key nummove-mode-map [left]    'nummove-move-left)
  (define-key nummove-mode-map [right]   'nummove-move-right)
  (define-key nummove-mode-map [up]      'nummove-move-up)
  (define-key nummove-mode-map [down]    'nummove-move-down)
  (define-key nummove-mode-map "r"    'nummove-restart)
  
  (use-local-map nummove-mode-map)  
  (buffer-disable-undo (current-buffer))
  )

(defun nummove-restart()
  (interactive)
  (nummove-init-board)

  (setq nummove-is-automode t)

  (setq nummove-moves 0)
  (nummove-draw-board)
  (nummove-shuffle-board)
  (nummove-init-keys)

  (setq nummove-is-automode nil)
)

(defun nummove-init-board()

  (setq nummove-numlist [[nil nil nil nil][nil nil nil nil][nil nil nil nil][nil nil nil nil]])
  
  (let (x y)
    (setq x 0)
    (setq y 0)

    (while (< y 4)
      (while (< x 4)
	(aset (aref nummove-numlist y) x (cons (+ (* 4 y) x 1) (create-image (concat nummove-path-to-images (number-to-string (+ (* 4 y) x 1)) ".png"))))
	(setq x (+ x 1))
	)
      (setq y (+ y 1))
      (setq x 0)
      )

    )
    (setq nummove-empty-x 3)
    (setq nummove-empty-y 3)
  )

(defun nummove-shuffle-board()
  
  (let (i r)
    (setq i 0)

    (while (< i 200) ;(not (eq (car (nummove-get-boardentry 2 2)) 9)) )

      (setq r (random 4))
      (cond 
       ((eq r 0) (nummove-move-left)) 
       ((eq r 1) (nummove-move-right)) 
       ((eq r 2) (nummove-move-up)) 
       ((eq r 3) (nummove-move-down)) 
       )

      (setq i (+ i 1))
      )
    )
  )

(defun nummove-draw-board()

  (let (x y)
    (setq x 0)
    (setq y 0)

    (erase-buffer)
    (insert "**** NumMove 0.1 ****")
    (newline)

    (while (< y 4)
      (while (< x 4)

	(insert-image (cdr (aref (aref nummove-numlist y) x)))
	
	(setq x (+ x 1))
	)
      (setq y (+ y 1))
      (setq x 0)
      (newline)
      )
    )
  )

(defun nummove()
  (interactive)
  (message "Press 'r' to restart the game")

  (nummove-init-buffer)
  (nummove-restart)
  )

