;;-----------------------=={ Update Attributes }==----------------------;;
;;                                                                      ;;
;;  Reads a CSV file containing attribute data, and, should the drawing ;;
;;  name of the current drawing appear in the first column of the CSV   ;;
;;  Drawing List, the program will proceed to update block attributes.  ;;
;;                                                                      ;;
;;  The attributes updated will be those with tags corresponding to the ;;
;;  CSV column headings. These will be updated with values from the     ;;
;;  row in which the drawing file is listed.                            ;;
;;                                                                      ;;
;;  -------------------------------------                               ;;
;;  Example of CSV format:                                              ;;
;;  -------------------------------------                               ;;
;;                                                                      ;;
;;  +------------+-----------+-----------+----------+-----+----------+  ;;
;;  |    DWG     |  Layout*  |   Block*  |   TAG1   | ... |   TAGN   |  ;;
;;  +------------+-----------+-----------+----------+-----+----------+  ;;
;;  |  Drawing1  |  Layout1  |   Block1  |  Value1  | ... |  ValueN  |  ;;
;;  +------------+-----------+-----------+----------+-----+----------+  ;;
;;  |  Drawing1  |  Layout2  |   Block1  |  Value1  | ... |  ValueN  |  ;;
;;  +------------+-----------+-----------+----------+-----+----------+  ;;
;;  |  Drawing2  |  Layout1  |   Block2  |  Value1  | ... |  ValueN  |  ;;
;;  +------------+-----------+-----------+----------+-----+----------+  ;;
;;  |    ...     |    ...    |    ...    |    ...   | ... |    ...   |  ;;
;;  +------------+-----------+-----------+----------+-----+----------+  ;;
;;                                                                      ;;
;;  *Layout & Block Name columns are optional.                          ;;
;;                                                                      ;;
;;  -------------------------------------                               ;;
;;  Possible Warnings:                                                  ;;
;;  -------------------------------------                               ;;
;;  -  Without a block filter or block name column the code will        ;;
;;     update ALL attributed blocks with tags listed in the CSV         ;;
;;     headings.                                                        ;;
;;                                                                      ;;
;;  -  Currently designed to run on startup - add to either             ;;
;;     Startup-Suite or ACADDOC.lsp to update blocks when drawing is    ;;
;;     opened. To disable code running when loaded, remove (c:utb)      ;;
;;     from the bottom of the code.                                     ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
(defun c:ut

    (
        /
        *error*
        ano
        bln bno
        csv
        ent
        flg fnb:fun
        inc
        lst
        sel str
        tag
        utb:blk utb:csv utb:ftr utb:lay
        val
    )

;;----------------------------------------------------------------------;;
;; Location of CSV Drawing List (set to nil for prompt)                 ;;
;;                                                                      ;;
;; If the CSV file resides in the same directory as the drawing, omit   ;;
;; the filepath from the location of the CSV file, and only include     ;;
;; the filename, e.g. "myfile.csv"                                      ;;
;;                                                                      ;;
;; If only a filename is specified, AutoCAD will first search the       ;;
;; working directory for this file before searching the Support Paths.  ;;
;;----------------------------------------------------------------------;;
  
    (setq utb:csv nil) ;; e.g. (setq utb:csv "C:/myfolder/myfile.csv")

;;----------------------------------------------------------------------;;
;; Block Filter (may use wildcards and may be nil)                      ;;
;;----------------------------------------------------------------------;;

    (setq utb:ftr nil)  ;; e.g. (setq utb:ftr "*BORDER")

;;----------------------------------------------------------------------;;
;; Layout Column (t or nil)                                             ;;
;;----------------------------------------------------------------------;;

    (setq utb:lay t)    ;; set to t if CSV file contains Layout Column

;;----------------------------------------------------------------------;;
;; Block Name Column (t or nil)                                         ;;
;;----------------------------------------------------------------------;;

    (setq utb:blk nil)  ;; set to t if CSV file contains Block Name Column

;;----------------------------------------------------------------------;;

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq fnb:fun
        (lambda ( s )
            (if (wcmatch (strcase s t) "*.dwg,*.dxf,*.dwt,*.dws")
                (vl-filename-base s) s
            )
        )
    )
    (cond
        (   (not (setq sel (ssget "_X" (vl-list* '(0 . "INSERT") '(66 . 1) (if utb:ftr (list (cons 2 (strcat "`*U*," utb:ftr))))))))
            (princ "\nNo Attributed Blocks found in drawing.")
        )
        (   (and utb:csv (not (setq csv (findfile utb:csv))))
            (princ
                (strcat
                    "\n"
                    (vl-filename-base utb:csv)
                    (vl-filename-extension utb:csv)
                    " not found."
                )
            )
        )
        (   (and csv (/= ".CSV" (strcase (vl-filename-extension csv))))
            (princ "\nAttribute data file must be in CSV format.")
        )
        (   (not (or csv (setq csv (getfiled "Select CSV File" "" "csv" 16))))
            (princ "\n*Cancel*")
        )
        (   (not (setq lst (mapcar '(lambda ( x ) (cons (strcase (fnb:fun (car x))) (cdr x))) (LM:readcsv csv))))
            (princ
                (strcat
                    "\nNo data found in "
                    (vl-filename-base csv)
                    ".csv file."
                )
            )
        )
        (   (not
                (setq tag (mapcar 'strcase (cdar lst))
                      lst (LM:massoc (strcase (fnb:fun (getvar 'dwgname))) lst)
                )
            )
            (princ (strcat "\n" (fnb:fun (getvar 'dwgname)) " not found in first column of CSV file."))
        )
        (   t
            (setq lst (mapcar '(lambda ( x ) (mapcar 'cons tag x)) lst)
                  ano 0
                  bno 0
            )
            (LM:startundo (LM:acdoc))
            (repeat (setq inc (sslength sel))
                (setq ent (ssname sel (setq inc (1- inc)))
                      bln (strcase (LM:al-effectivename ent))
                      val lst
                      flg nil
                )
                (if (or (null utb:ftr) (wcmatch bln (strcase utb:ftr)))
                    (progn
                        (if utb:lay
                            (setq val (mapcar '(lambda ( x ) (cons (strcase (cdar x)) (cdr x))) val)
                                  val (LM:massoc (strcase (cdr (assoc 410 (entget ent)))) val)
                            )
                        )
                        (if utb:blk
                            (setq val (mapcar '(lambda ( x ) (cons (strcase (cdar x)) (cdr x))) val)
                                  val (cdr (assoc bln val))
                            )
                            (setq val (car val))
                        )
                        (if val
                            (foreach att (vlax-invoke (vlax-ename->vla-object ent) 'getattributes)
                                (if
                                    (and
                                        (setq str (assoc (strcase (vla-get-tagstring att)) val))
                                        (progn
                                            (setq val (LM:remove1st str val))
                                            (/= (vla-get-textstring att) (cdr str))
                                        )
                                    )
                                    (progn
                                        (vla-put-textstring att (cdr str))
                                        (setq flg t
                                              ano (1+ ano)
                                        )
                                    ) 
                                )
                            )
                        )
                        (if flg (setq bno (1+ bno)))
                    )
                )
            )
            (if (zerop ano)
                (princ "\nAll attributes are up-to-date.")
                (princ
                    (strcat
                        "\n"           (itoa ano) " attribute" (if (= 1 ano) "" "s")
                        " updated in " (itoa bno) " block"     (if (= 1 bno) "" "s") "."
                    )
                )
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Effective Block Name  -  Lee Mac
;; ent - [ent] Block Reference entity

(defun LM:al-effectivename ( ent / blk rep )
    (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
        (if
            (and
                (setq rep
                    (cdadr
                        (assoc -3
                            (entget
                                (cdr
                                    (assoc 330
                                        (entget
                                            (tblobjname "block" blk)
                                        )
                                    )
                                )
                               '("AcDbBlockRepBTag")
                            )
                        )
                    )
                )
                (setq rep (handent (cdr (assoc 1005 rep))))
            )
            (setq blk (cdr (assoc 2 (entget rep))))
        )
    )
    blk
)

;; Read CSV  -  Lee Mac
;; Parses a CSV file into a matrix list of cell values.
;; csv - [str] filename of CSV file to read
 
(defun LM:readcsv ( csv / des lst sep str )
    (if (setq des (open csv "r"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
            (while (setq str (read-line des))
                (setq lst (cons (LM:csv->lst str sep 0) lst))
            )
            (close des)
        )
    )
    (reverse lst)
)

;; CSV -> List  -  Lee Mac
;; Parses a line from a CSV file into a list of cell values.
;; str - [str] string read from CSV file
;; sep - [str] CSV separator token
;; pos - [int] initial position index (always zero)
 
(defun LM:csv->lst ( str sep pos / s )
    (cond
        (   (not (setq pos (vl-string-search sep str pos)))
            (if (wcmatch str "\"*\"")
                (list (LM:csv-replacequotes (substr str 2 (- (strlen str) 2))))
                (list str)
            )
        )
        (   (or (wcmatch (setq s (substr str 1 pos)) "\"*[~\"]")
                (and (wcmatch s "~*[~\"]*") (= 1 (logand 1 pos)))
            )
            (LM:csv->lst str sep (+ pos 2))
        )
        (   (wcmatch s "\"*\"")
            (cons
                (LM:csv-replacequotes (substr str 2 (- pos 2)))
                (LM:csv->lst (substr str (+ pos 2)) sep 0)
            )
        )
        (   (cons s (LM:csv->lst (substr str (+ pos 2)) sep 0)))
    )
)

(defun LM:csv-replacequotes ( str / pos )
    (setq pos 0)
    (while (setq pos (vl-string-search  "\"\"" str pos))
        (setq str (vl-string-subst "\"" "\"\"" str pos)
              pos (1+ pos)
        )
    )
    str
)

;; MAssoc  -  Lee Mac
;; Returns all associations of a key in an association list

(defun LM:massoc ( key lst / item )
    (if (setq item (assoc key lst))
        (cons (cdr item) (LM:massoc key (cdr (member item lst))))
    )
)

;; Remove 1st  -  Lee Mac
;; Removes the first occurrence of an item from a list

(defun LM:remove1st ( itm lst / f )
    (setq f equal)
    (vl-remove-if '(lambda ( a ) (if (f a itm) (setq f (lambda ( a b ) nil)))) lst)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)

(princ)
