;;; Tempo template definitions

(use-package uuidgen
  :commands (uuidgen-1
             uuidgen-4
             uuidgen))

(defun my-template-handler (arg)
  "Handle custom symbol as ARG in my tempo templates."
  (pcase arg
    ('date (format-time-string "%F" (current-time)))
    ('year (format-time-string "%Y" (current-time)))
    ('datetime (format-time-string "%FT%T%z" (current-time)))
    ('uuid (uuidgen-4))))

(defvar my-tempo-match-finder "\\(%[[:word:]]+\\)\\="
  "Tempo match finder for my.")

(use-package tempo
  :commands (tempo-define-template
             tempo-use-tag-list
             tempo-complete-tag)
  :config
  (setq tempo-interactive t)
  (setq tempo-user-elements
        (cons #'my-template-handler tempo-user-elements))

  ;; note: don't use any taglist as it needs to be added by `tempo-use-tag-list',
  ;; which changes a local variable and is hard to add it everywhere
  (tempo-define-template
   "my-date"
   '(date)
   "%date"
   "Insert date")

  (tempo-define-template
   "my-datetime"
   '(datetime)
   "%datetime"
   "Insert datetime")

  (tempo-define-template
   "my-uuid"
   '(uuid)
   "%uuid"
   "Insert uuid v4")

  (tempo-define-template
   "my-agpl"
   '((p "Description: " desc t)
     (p "Author: " author t)
     (s desc) n
     "Copyright (C) " year "  " (s author) n
     "
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
")
   "%agpl"
   "Insert AGPL notice"))

