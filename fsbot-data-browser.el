;;; -*- lexical-binding: t -*-

(defvar fsbot-data '())

(defun fsbot-download-data ()
  (interactive)
  (url-retrieve
   "http://gnufans.net/~fsbot/data/botbbdb"
   (lambda (&rest _)
     (write-region nil nil "~/.emacs.d/.fsbot-data-raw" nil)
     (message "fsbot data finished downloading"))))

(defun fsbot-parse-data ()
  (goto-char 0)
  (let ((ret '()))
    (while (search-forward "[" nil t)
      (push (read-from-string (thing-at-point 'line t)) ret))
    ret))

(defun fsbot-process-notes (notes)
  (if notes
      (let ((real-notes (car (read-from-string notes))))
        (cond ((not real-notes) "nil")
              ((stringp real-notes) real-notes)
              ((and (listp real-notes)
                    (= 1 (length real-notes))
                    (stringp (car real-notes)))
               (car real-notes))
              (t (format "%S" real-notes))))
    "nil"))

(defun fsbot-load-data ()
  (let ((fsbot-parsed-data (with-current-buffer (find-file-noselect "~/.emacs.d/.fsbot-data-raw")
                             (fsbot-parse-data))))
    (-map (lambda (entry)
            (let ((key (aref (car entry) 0))
                  (notes (fsbot-process-notes
                          (cdr (car (cdr (aref (car entry) 7)))))))
              `(,key [,key ,notes])))
          fsbot-parsed-data)))

(define-derived-mode fsbot-data-browser-mode tabulated-list-mode
  "Fsbot Data Browser" "Tabulated-list-mode browser for fsbot data."
  (setq tabulated-list-format [("Key" 30 t)
                               ("Notes" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Key" nil))
  (tabulated-list-init-header))

(defun fsbot-list-data (data)
  (interactive)
  (pop-to-buffer "*fsbot data*" nil)
  (fsbot-data-browser-mode)
  (setq truncate-lines t)
  (setq tabulated-list-entries data)
  (tabulated-list-print t))

(defun fsbot-view-data ()
  (interactive)
  (fsbot-list-data (fsbot-load-data)))
