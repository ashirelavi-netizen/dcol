;;; ============================================================
;;;  DCOL.lsp  -  תוסף ציור צורה כפולה עם האצ'
;;;  פקודות: DCOL , DCOLUP , DCOLSET
;;;
;;;  קובץ זה מכיל את כל קוד התוסף.
;;;  נטען ע"י acaddoc.lsp (loader) שיושב בתיקיית Support.
;;;  ראה DCOL_summary_v7.md לפרטי הפרויקט המלאים.
;;; ============================================================

(vl-load-com)

;;; ----- שמות קבועים -----
(setq *COL-DICT*  "COL_SETTINGS")   ; שם המילון לשמירת הגדרות
(setq *COL-XAPP*  "COL_PAIR")       ; שם אפליקציית XData לזיהוי זוגות
(setq *COL-VER*   "1")              ; גרסת מבנה XData

;;; ============================================================
;;;  util:filter-list — סינון רשימה לפי טקסט (מינימום 3 תווים)
;;;  lst  = רשימת מחרוזות לסינון
;;;  term = טקסט חיפוש; אם פחות מ-3 תווים — מחזיר את כל הרשימה
;;; ============================================================
(defun util:filter-list (lst term)
  (if (>= (strlen term) 3)
    (vl-remove-if-not
      '(lambda (x) (vl-string-search (strcase term) (strcase x)))
      lst)
    lst)
)

;;; ============================================================
;;;  עזר: רישום אפליקציית XData
;;; ============================================================
(defun col:regapp ()
  (if (not (tblsearch "APPID" *COL-XAPP*))
    (regapp *COL-XAPP*))
)

;;; ============================================================
;;;  הגדרות: קריאה / כתיבה במילון של הקובץ
;;;  סדר השדות:
;;;   0 lineLayerSrc   1 hatchLayerSrc
;;;   2 lineLayerCpy   3 hatchLayerCpy
;;;   4 patSrc 5 scaleSrc 6 angSrc
;;;   7 patCpy 8 scaleCpy 9 angCpy
;;;  10 dX 11 dY 12 dZ
;;;  13 activeShape ("P"/"R"/"C"/"E"/"M")
;;;  14 axisLayer
;;;  15 axRect 16 axCirc 17 axEll
;;;  18 nestedOuterLayer  19 nestedHatchLayer
;;;  20 nestedPattern     21 nestedScale  22 nestedAngle
;;;  23 nestedInnerLayer
;;; ============================================================

(defun col:settings-path ()
  (strcat (getenv "USERPROFILE") "\\dcol_settings.dat"))

(defun col:get-settings ( / path f line lst )
  (setq path (col:settings-path))
  (if (findfile path)
    (progn
      (setq lst '() f (open path "r"))
      (while (setq line (read-line f))
        (setq lst (append lst (list line))))
      (close f)
      (if lst lst nil))
    nil))

(defun col:put-settings ( lst / f )
  (setq f (open (col:settings-path) "w"))
  (foreach s lst (write-line s f))
  (close f)
  lst)

(defun col:default-settings ()
  (list
    "0" "0"      ; שכבות מקור
    "0" "0"      ; שכבות עותק
    "ANSI31" "1.0" "0.0"   ; האצ' מקור
    "ANSI31" "1.0" "0.0"   ; האצ' עותק
    "500.0" "0.0" "0.0"    ; הזחה dX dY dZ
    "P"          ; צורה פעילה  [13]
    "0"          ; שכבת קו ציר [14]
    "0" "0" "0"  ; ax rect/circ/ell [15][16][17]
    "0"          ; שכבת קו חיצוני Geometry [18]
    "0"          ; שכבת האצ' Geometry [19]
    "ANSI31"     ; Pattern Geometry [20]
    "1.0"        ; Scale Geometry [21]
    "0.0"        ; Angle Geometry [22]
    "0")         ; שכבת קו פנימי Geometry [23]
)

;;; ============================================================
;;;  רשימת שכבות קיימות בקובץ
;;; ============================================================
(defun col:layer-list ( / l name )
  (setq l '())
  (setq name (tblnext "LAYER" t))
  (while name
    (setq l (cons (cdr (assoc 2 name)) l))
    (setq name (tblnext "LAYER" nil)))
  (acad_strlsort l)
)

;;; ============================================================
;;;  חלון הגדרות (DCL) — כולל שדות חיפוש מעל כל popup_list שכבה
;;; ============================================================
(defun col:write-dcl ( / f path )
  (setq path (vl-filename-mktemp "col" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "col_dlg : dialog {" f)
  (write-line "  label = \"הגדרות COL/WALL & Geometry\";" f)
  ;; ===== קטע COL/WALL =====
  (write-line "  : boxed_column { label = \"COL / WALL\";" f)
  (write-line "    : row {" f)
  (write-line "      : boxed_column { label = \"מקור\";" f)
  (write-line "        : edit_box { key=\"f_ll_src\"; label=\"\"; edit_width=12; }" f)
  (write-line "        : popup_list { key=\"ll_src\"; label=\"שכבת קו\"; }" f)
  (write-line "        : edit_box { key=\"f_hl_src\"; label=\"\"; edit_width=12; }" f)
  (write-line "        : popup_list { key=\"hl_src\"; label=\"שכבת האצ'\"; }" f)
  (write-line "        : popup_list { key=\"pat_src\"; label=\"Pattern\"; }" f)
  (write-line "        : edit_box { key=\"sc_src\"; label=\"Scale\"; edit_width=8; }" f)
  (write-line "        : edit_box { key=\"an_src\"; label=\"Angle\"; edit_width=8; } }" f)
  (write-line "      : boxed_column { label = \"עותק\";" f)
  (write-line "        : edit_box { key=\"f_ll_cpy\"; label=\"\"; edit_width=12; }" f)
  (write-line "        : popup_list { key=\"ll_cpy\"; label=\"שכבת קו\"; }" f)
  (write-line "        : edit_box { key=\"f_hl_cpy\"; label=\"\"; edit_width=12; }" f)
  (write-line "        : popup_list { key=\"hl_cpy\"; label=\"שכבת האצ'\"; }" f)
  (write-line "        : popup_list { key=\"pat_cpy\"; label=\"Pattern\"; }" f)
  (write-line "        : edit_box { key=\"sc_cpy\"; label=\"Scale\"; edit_width=8; }" f)
  (write-line "        : edit_box { key=\"an_cpy\"; label=\"Angle\"; edit_width=8; } }" f)
  (write-line "    }" f)
  (write-line "    : boxed_row { label = \"הזחת עותק\";" f)
  (write-line "      : edit_box { key=\"dx\"; label=\"dX\"; edit_width=8; }" f)
  (write-line "      : edit_box { key=\"dy\"; label=\"dY\"; edit_width=8; }" f)
  (write-line "      : edit_box { key=\"dz\"; label=\"dZ\"; edit_width=8; } }" f)
  (write-line "    : boxed_column { label = \"קו ציר\";" f)
  (write-line "      : edit_box { key=\"f_ax_lay\"; label=\"\"; edit_width=12; }" f)
  (write-line "      : popup_list { key=\"ax_lay\"; label=\"שכבה\"; }" f)
  (write-line "      : toggle { key=\"ax_rect\"; label=\"Rectangle\"; }" f)
  (write-line "      : toggle { key=\"ax_circ\"; label=\"Circle\"; }" f)
  (write-line "      : toggle { key=\"ax_ell\";  label=\"Ellipse\"; } }" f)
  (write-line "  }" f)
  ;; ===== קטע Geometry =====
  (write-line "  : boxed_column { label = \"Geometry\";" f)
  (write-line "    : row {" f)
  (write-line "      : column {" f)
  (write-line "        : edit_box { key=\"f_nl_lay\"; label=\"\"; edit_width=12; }" f)
  (write-line "        : popup_list { key=\"nl_lay\"; label=\"קו חיצוני\"; } }" f)
  (write-line "      : column {" f)
  (write-line "        : edit_box { key=\"f_nl_in_lay\"; label=\"\"; edit_width=12; }" f)
  (write-line "        : popup_list { key=\"nl_in_lay\"; label=\"קו פנימי\"; } }" f)
  (write-line "      : column {" f)
  (write-line "        : edit_box { key=\"f_nh_lay\"; label=\"\"; edit_width=12; }" f)
  (write-line "        : popup_list { key=\"nh_lay\"; label=\"שכבת האצ'\"; } }" f)
  (write-line "    }" f)
  (write-line "    : row {" f)
  (write-line "      : popup_list { key=\"n_pat\"; label=\"Pattern\"; }" f)
  (write-line "      : edit_box { key=\"n_sc\"; label=\"Scale\"; edit_width=8; }" f)
  (write-line "      : edit_box { key=\"n_an\"; label=\"Angle\"; edit_width=8; } }" f)
  (write-line "  }" f)
  (write-line "  : button { key=\"newlay\"; label=\"שכבה חדשה...\"; }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  path
)

;;; רשימת patterns בסיסית
(defun col:pattern-list ()
  (list "SOLID" "ANSI31" "ANSI32" "ANSI33" "ANSI37"
        "NET" "NET3" "GRAVEL" "EARTH" "DOTS"
        "GRASS" "HONEY" "BRICK" "AR-CONC" "AR-SAND")
)

;;; ============================================================
;;;  col:dlg — חלון הגדרות עם סינון שכבות בזמן אמת
;;; ============================================================
(defun col:dlg ( cur / dclid path lays pats res result
                       fl-ll-src fl-hl-src fl-ll-cpy fl-hl-cpy
                       fl-ax-lay fl-nl-lay fl-nl-in-lay fl-nh-lay )
  (setq path (col:write-dcl))
  (setq lays (col:layer-list))
  (setq pats (col:pattern-list))
  (setq res nil)
  (while
    (progn
      (setq dclid (load_dialog path))
      (if (not (new_dialog "col_dlg" dclid)) (exit))
      ;; אתחול רשימות מסוננות
      (setq fl-ll-src lays  fl-hl-src lays
            fl-ll-cpy lays  fl-hl-cpy lays
            fl-ax-lay lays  fl-nl-lay lays
            fl-nl-in-lay lays  fl-nh-lay lays)
      ;; מילוי רשימות
      (start_list "ll_src")    (mapcar 'add_list lays) (end_list)
      (start_list "hl_src")    (mapcar 'add_list lays) (end_list)
      (start_list "ll_cpy")    (mapcar 'add_list lays) (end_list)
      (start_list "hl_cpy")    (mapcar 'add_list lays) (end_list)
      (start_list "pat_src")   (mapcar 'add_list pats) (end_list)
      (start_list "pat_cpy")   (mapcar 'add_list pats) (end_list)
      (start_list "ax_lay")    (mapcar 'add_list lays) (end_list)
      (start_list "nl_lay")    (mapcar 'add_list lays) (end_list)
      (start_list "nl_in_lay") (mapcar 'add_list lays) (end_list)
      (start_list "nh_lay")    (mapcar 'add_list lays) (end_list)
      (start_list "n_pat")     (mapcar 'add_list pats) (end_list)
      ;; ניקוי שדות חיפוש
      (set_tile "f_ll_src" "")     (set_tile "f_hl_src" "")
      (set_tile "f_ll_cpy" "")     (set_tile "f_hl_cpy" "")
      (set_tile "f_ax_lay" "")     (set_tile "f_nl_lay" "")
      (set_tile "f_nl_in_lay" "")  (set_tile "f_nh_lay" "")
      ;; ערכים נוכחיים
      (set_tile "ll_src"    (itoa (col:idx (nth 0 cur) lays)))
      (set_tile "hl_src"    (itoa (col:idx (nth 1 cur) lays)))
      (set_tile "ll_cpy"    (itoa (col:idx (nth 2 cur) lays)))
      (set_tile "hl_cpy"    (itoa (col:idx (nth 3 cur) lays)))
      (set_tile "pat_src"   (itoa (col:idx (nth 4 cur) pats)))
      (set_tile "sc_src"    (nth 5 cur))
      (set_tile "an_src"    (nth 6 cur))
      (set_tile "pat_cpy"   (itoa (col:idx (nth 7 cur) pats)))
      (set_tile "sc_cpy"    (nth 8 cur))
      (set_tile "an_cpy"    (nth 9 cur))
      (set_tile "dx"        (nth 10 cur))
      (set_tile "dy"        (nth 11 cur))
      (set_tile "dz"        (nth 12 cur))
      (set_tile "ax_lay"    (itoa (col:idx (nth 14 cur) lays)))
      (set_tile "ax_rect"   (nth 15 cur))
      (set_tile "ax_circ"   (nth 16 cur))
      (set_tile "ax_ell"    (nth 17 cur))
      (set_tile "nl_lay"    (itoa (col:idx (nth 18 cur) lays)))
      (set_tile "nh_lay"    (itoa (col:idx (nth 19 cur) lays)))
      (set_tile "n_pat"     (itoa (col:idx (nth 20 cur) pats)))
      (set_tile "n_sc"      (nth 21 cur))
      (set_tile "n_an"      (nth 22 cur))
      (set_tile "nl_in_lay" (itoa (col:idx (nth 23 cur) lays)))
      ;; ===== פעולות סינון שכבות =====
      (action_tile "f_ll_src"
        "(progn (setq fl-ll-src (util:filter-list lays (get_tile \"f_ll_src\"))) (start_list \"ll_src\") (mapcar 'add_list fl-ll-src) (end_list) (set_tile \"ll_src\" \"0\"))")
      (action_tile "f_hl_src"
        "(progn (setq fl-hl-src (util:filter-list lays (get_tile \"f_hl_src\"))) (start_list \"hl_src\") (mapcar 'add_list fl-hl-src) (end_list) (set_tile \"hl_src\" \"0\"))")
      (action_tile "f_ll_cpy"
        "(progn (setq fl-ll-cpy (util:filter-list lays (get_tile \"f_ll_cpy\"))) (start_list \"ll_cpy\") (mapcar 'add_list fl-ll-cpy) (end_list) (set_tile \"ll_cpy\" \"0\"))")
      (action_tile "f_hl_cpy"
        "(progn (setq fl-hl-cpy (util:filter-list lays (get_tile \"f_hl_cpy\"))) (start_list \"hl_cpy\") (mapcar 'add_list fl-hl-cpy) (end_list) (set_tile \"hl_cpy\" \"0\"))")
      (action_tile "f_ax_lay"
        "(progn (setq fl-ax-lay (util:filter-list lays (get_tile \"f_ax_lay\"))) (start_list \"ax_lay\") (mapcar 'add_list fl-ax-lay) (end_list) (set_tile \"ax_lay\" \"0\"))")
      (action_tile "f_nl_lay"
        "(progn (setq fl-nl-lay (util:filter-list lays (get_tile \"f_nl_lay\"))) (start_list \"nl_lay\") (mapcar 'add_list fl-nl-lay) (end_list) (set_tile \"nl_lay\" \"0\"))")
      (action_tile "f_nl_in_lay"
        "(progn (setq fl-nl-in-lay (util:filter-list lays (get_tile \"f_nl_in_lay\"))) (start_list \"nl_in_lay\") (mapcar 'add_list fl-nl-in-lay) (end_list) (set_tile \"nl_in_lay\" \"0\"))")
      (action_tile "f_nh_lay"
        "(progn (setq fl-nh-lay (util:filter-list lays (get_tile \"f_nh_lay\"))) (start_list \"nh_lay\") (mapcar 'add_list fl-nh-lay) (end_list) (set_tile \"nh_lay\" \"0\"))")
      ;; כפתור שכבה חדשה
      (action_tile "newlay" "(setq res \"NEWLAYER\")(done_dialog 2)")
      ;; accept — משתמש ברשימות המסוננות
      (action_tile "accept"
        (strcat
          "(setq res (list"
          " (nth (atoi (get_tile \"ll_src\")) fl-ll-src)"
          " (nth (atoi (get_tile \"hl_src\")) fl-hl-src)"
          " (nth (atoi (get_tile \"ll_cpy\")) fl-ll-cpy)"
          " (nth (atoi (get_tile \"hl_cpy\")) fl-hl-cpy)"
          " (nth (atoi (get_tile \"pat_src\")) pats)"
          " (get_tile \"sc_src\") (get_tile \"an_src\")"
          " (nth (atoi (get_tile \"pat_cpy\")) pats)"
          " (get_tile \"sc_cpy\") (get_tile \"an_cpy\")"
          " (get_tile \"dx\") (get_tile \"dy\") (get_tile \"dz\")"
          " \"" (nth 13 cur) "\""
          " (nth (atoi (get_tile \"ax_lay\")) fl-ax-lay)"
          " (get_tile \"ax_rect\") (get_tile \"ax_circ\") (get_tile \"ax_ell\")"
          " (nth (atoi (get_tile \"nl_lay\")) fl-nl-lay)"
          " (nth (atoi (get_tile \"nh_lay\")) fl-nh-lay)"
          " (nth (atoi (get_tile \"n_pat\")) pats)"
          " (get_tile \"n_sc\") (get_tile \"n_an\")"
          " (nth (atoi (get_tile \"nl_in_lay\")) fl-nl-in-lay)))"
          "(done_dialog 1)"))
      (setq result (start_dialog))
      (unload_dialog dclid)
      (if (= res "NEWLAYER")
        (progn
          (setq res nil)
          (if (vl-catch-all-error-p
                (vl-catch-all-apply 'command (list "_.CLASSICLAYER")))
            (command "_.LAYER"))
          (getstring "\n[DCOL] סיימת ליצור שכבות? לחץ Enter להמשך: ")
          (setq lays (col:layer-list))
          t)
        nil))
  )
  (vl-file-delete path)
  res
)

;;; אינדקס פריט ברשימה (0 אם לא נמצא)
(defun col:idx ( item lst / i found )
  (setq i 0 found 0)
  (foreach x lst
    (if (= x item) (setq found i))
    (setq i (1+ i)))
  found
)

;;; ============================================================
;;;  ודא שיש הגדרות; אם לא -> פתח דיאלוג
;;; ============================================================
(defun col:ensure-settings ( / s def )
  (setq s (col:get-settings))
  (if s
    (progn
      (setq def (col:default-settings))
      (while (< (length s) (length def))
        (setq s (append s (list (nth (length s) def)))))
      (col:put-settings s)
      s)
    (progn
      (princ "\nפעם ראשונה - אנא הגדר את הפרמטרים.")
      (setq s (col:dlg (col:default-settings)))
      (col:put-settings (if s s (col:default-settings))))))

;;; ============================================================
;;;  ציור צורות — קלט מהמשתמש
;;; ============================================================

;;; פוליגון זמני פתוח (לתצוגה חיה)
(defun col:mk-pline-open ( pts / data )
  (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                   '(8 . "0") '(100 . "AcDbPolyline")
                   (cons 90 (length pts)) '(70 . 0)))
  (foreach p pts
    (setq data (append data (list (cons 10 (list (car p) (cadr p)))))))
  (entmakex data)
)

;;; Polygon סגור — אוסף נקודות
(defun col:draw-poly ( / pts pt tmp )
  (setq pts '() tmp nil)
  (setq pt (getpoint "\nנקודה ראשונה: "))
  (if pt
    (progn
      (setq pts (list pt))
      (while
        (progn
          (setq pt (getpoint (last pts) "\nנקודה הבאה (Enter לסיום): "))
          (if pt
            (progn
              (setq pts (append pts (list pt)))
              (if tmp (entdel tmp))
              (setq tmp (col:mk-pline-open pts))
              t)
            nil)))
      (if tmp (entdel tmp))
      (redraw)
      pts)
    (progn (redraw) nil))
)

;;; Polyline פתוח — מינימום 2 נקודות (לשימוש ב-Border)
(defun col:draw-polyline ( / pts pt tmp )
  (setq pts '() tmp nil)
  (setq pt (getpoint "\nנקודה ראשונה: "))
  (if pt
    (progn
      (setq pts (list pt))
      (while
        (progn
          (setq pt (getpoint (last pts) "\nנקודה הבאה (Enter לסיום): "))
          (if pt
            (progn
              (setq pts (append pts (list pt)))
              (if tmp (entdel tmp))
              (setq tmp (col:mk-pline-open pts))
              t)
            nil)))
      (if tmp (entdel tmp))
      (redraw)
      pts)
    (progn (redraw) nil))
)

;;; Rectangle: שתי פינות אלכסוניות
(defun col:draw-rect ( / p1 p2 )
  (setq p1 (getpoint "\nפינה ראשונה: "))
  (if p1
    (progn
      (setq p2 (getcorner p1 "\nפינה אלכסונית: "))
      (if p2 (list p1 p2) nil))
    nil)
)

;;; יצירת מלבן כ-LWPOLYLINE סגור משתי פינות
(defun col:mk-rect ( p1 p2 lay / x1 y1 z1 x2 y2 )
  (setq x1 (car p1) y1 (cadr p1) z1 (caddr p1))
  (setq x2 (car p2) y2 (cadr p2))
  (col:mk-pline
    (list p1 (list x2 y1 z1) p2 (list x1 y2 z1))
    lay)
)

;;; Circle: מרכז + רדיוס עם תצוגה חיה
(defun col:draw-circle ( / cen gr tmp r )
  (setq cen (getpoint "\nמרכז העיגול: "))
  (if (not cen) nil
    (progn
      (setq tmp nil)
      (princ "\nרדיוס: ")
      (while
        (progn
          (setq gr (grread t 4 0))
          (cond
            ((= (car gr) 5)
             (setq r (distance cen (cadr gr)))
             (if tmp (entdel tmp))
             (setq tmp (entmakex (list '(0 . "CIRCLE") '(8 . "0")
                                       (cons 10 cen) (cons 40 r))))
             t)
            ((= (car gr) 3)
             (setq r (distance cen (cadr gr)))
             (if tmp (entdel tmp))
             nil)
            ((= (car gr) 2)
             (if tmp (entdel tmp))
             (setq tmp nil)
             (setq r (getdist cen ""))
             nil)
            (t t))))
      (redraw)
      (if r (list cen r) nil)))
)

;;; Ellipse: קצה ראשון -> קצה שני -> רוחב, עם תצוגה חיה
(defun col:draw-ellipse ( / ep1 ep2 cen width p2 gr tmp maj ratio )
  (setq ep1 (getpoint "\nקצה ראשון של ציר ראשי: "))
  (if (not ep1) nil
    (progn
      (setq tmp nil ep2 nil)
      (princ "\nקצה שני של ציר ראשי: ")
      (while
        (progn
          (setq gr (grread t 4 0))
          (cond
            ((= (car gr) 5)
             (if tmp (entdel tmp))
             (setq tmp (entmakex (list '(0 . "LINE") '(8 . "0")
                                       (cons 10 ep1) (cons 11 (cadr gr)))))
             t)
            ((= (car gr) 3)
             (setq ep2 (cadr gr))
             (if tmp (entdel tmp))
             nil)
            ((= (car gr) 2)
             (if tmp (entdel tmp))
             (setq tmp nil)
             (setq ep2 (getpoint ep1 ""))
             nil)
            (t t))))
      (if (not ep2) (progn (redraw) nil)
        (progn
          (setq cen (list (/ (+ (car ep1) (car ep2)) 2.0)
                          (/ (+ (cadr ep1) (cadr ep2)) 2.0)
                          (/ (+ (caddr ep1) (caddr ep2)) 2.0)))
          (setq tmp nil width nil)
          (princ "\nרוחב (חצי ציר משני): ")
          (while
            (progn
              (setq gr (grread t 4 0))
              (cond
                ((= (car gr) 5)
                 (setq width (distance cen (cadr gr)))
                 (if (> width 0)
                   (progn
                     (if tmp (entdel tmp))
                     (setq maj (mapcar '- ep1 cen))
                     (setq ratio (/ width (distance cen ep1)))
                     (setq tmp (entmakex (list '(0 . "ELLIPSE") '(100 . "AcDbEntity")
                                               '(8 . "0") '(100 . "AcDbEllipse")
                                               (cons 10 cen) (cons 11 maj)
                                               (cons 40 ratio) '(41 . 0.0) (cons 42 (* 2 pi)))))))
                 t)
                ((= (car gr) 3)
                 (setq width (distance cen (cadr gr)))
                 (if tmp (entdel tmp))
                 nil)
                ((= (car gr) 2)
                 (if tmp (entdel tmp))
                 (setq tmp nil)
                 (setq width (getdist cen ""))
                 nil)
                (t t))))
          (redraw)
          (if (not width) nil
            (progn
              (setq p2 (list (car cen) (+ (cadr cen) width) (caddr cen)))
              (list cen ep1 p2)))))))
)

;;; ============================================================
;;;  יצירת אובייקטים (קו + האצ') + עותק
;;; ============================================================

;;; וקטור הזחה מההגדרות
(defun col:disp ( s )
  (list (atof (nth 10 s)) (atof (nth 11 s)) (atof (nth 12 s)))
)

;;; הוסף XData עם גרסה, pair-id ותפקיד
(defun col:tag ( ent pid role )
  (entmod
    (append (entget ent)
      (list (list -3
        (list *COL-XAPP*
          (cons 1000 *COL-VER*)
          (cons 1000 pid)
          (cons 1000 role))))))
)

;;; LWPOLYLINE סגור על שכבה
(defun col:mk-pline ( pts lay / data )
  (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                   (cons 8 lay) '(100 . "AcDbPolyline")
                   (cons 90 (length pts)) '(70 . 1)))
  (foreach p pts
    (setq data (append data (list (cons 10 (list (car p) (cadr p)))))))
  (entmakex data)
)

;;; LWPOLYLINE פתוח על שכבה (לשימוש ב-Border Polyline)
(defun col:mk-pline-open-lay ( pts lay / data )
  (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                   (cons 8 lay) '(100 . "AcDbPolyline")
                   (cons 90 (length pts)) '(70 . 0)))
  (foreach p pts
    (setq data (append data (list (cons 10 (list (car p) (cadr p)))))))
  (entmakex data)
)

;;; עיגול
(defun col:mk-circle ( cen r lay )
  (entmakex (list '(0 . "CIRCLE") (cons 8 lay)
                  (cons 10 cen) (cons 40 r)))
)

;;; מוצא האצ' שנוצר אחרי before-ent
(defun col:find-new-hatch ( before-ent lay / scan h ed )
  (setq h nil)
  (setq scan (if before-ent (entnext before-ent) (entnext)))
  (while (and scan (not h))
    (if (and (entget scan)
             (= "HATCH" (cdr (assoc 0 (entget scan)))))
      (setq h scan))
    (setq scan (entnext scan)))
  (if h
    (progn
      (setq ed (entget h))
      (if (not (equal lay (cdr (assoc 8 ed))))
        (setq ed (subst (cons 8 lay) (assoc 8 ed) ed)))
      (if (assoc 62 ed)
        (setq ed (subst (cons 62 256) (assoc 62 ed) ed))
        (setq ed (append ed (list (cons 62 256)))))
      (entmod ed)
      (entupd h)
      h)
    nil)
)

;;; האצ' על גבול יחיד
(defun col:mk-hatch ( ent lay pat sc ang / ss before )
  (setvar "HPNAME" pat)
  (setvar "HPSCALE" sc)
  (setvar "HPANG" ang)
  (setq ss (ssadd ent (ssadd)))
  (setq before (entlast))
  (command "_.-HATCH" "_LA" lay "_S" ss "" "")
  (col:find-new-hatch before lay)
)

;;; העתק ישות והזח לשכבה אחרת
(defun col:copy-and-move ( ent disp lay / dx dy dz ed )
  (setq dx (car disp) dy (cadr disp) dz (caddr disp))
  (command "_.COPY" ent "" "0,0,0"
           (strcat (rtos dx 2 6) "," (rtos dy 2 6) "," (rtos dz 2 6)))
  (setq ed (entget (entlast)))
  (entmod (subst (cons 8 lay) (assoc 8 ed) ed))
  (entupd (entlast))
  (entlast)
)

;;; ============================================================
;;;  מצב Attach — שיוך האצ' חיצוני
;;; ============================================================
(defun col:do-attach ( s disp pid / ss i ent all hatches lines
                                    srcHatch srcLines cpyLines cpyHatch
                                    assoc? outer inners )
  (setq ss (ssget "\nבחר האצ' וקווי מתאר (בחירה חופשית): "))
  (if (not ss) (progn (princ "\nבוטל.") (exit)))
  (setq all '() i 0)
  (while (< i (sslength ss))
    (setq all (append all (list (ssname ss i))))
    (setq i (1+ i)))
  (setq hatches (vl-remove-if-not
                  '(lambda (e) (= "HATCH" (cdr (assoc 0 (entget e)))))
                  all))
  (setq lines (vl-remove-if
                '(lambda (e) (= "HATCH" (cdr (assoc 0 (entget e)))))
                all))
  (if (not hatches)
    (progn (princ "\nלא נבחר האצ'. בוטל.") (exit)))
  (if (> (length hatches) 1)
    (progn (princ "\nנבחרו יותר מהאצ' אחד. בחר האצ' אחד בלבד. בוטל.") (exit)))
  (if (not lines)
    (progn (princ "\nלא נבחרו קווי מתאר. בוטל.") (exit)))
  (setq srcHatch (car hatches))
  (setq srcLines lines)
  (setq assoc? (= 1 (cdr (assoc 71 (entget srcHatch)))))
  (col:tag srcHatch pid "SH")
  (foreach e srcLines (col:tag e pid "SL"))
  (setq cpyLines
    (mapcar '(lambda (e) (col:copy-and-move e disp (nth 2 s)))
            srcLines))
  (setq outer (car cpyLines))
  (setq inners (cdr cpyLines))
  (if assoc?
    (setq cpyHatch
      (if inners
        (col:mk-hatch-multi outer inners (nth 3 s)
          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))
        (col:mk-hatch outer (nth 3 s)
          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))))
    (setq cpyHatch (col:copy-and-move srcHatch disp (nth 3 s))))
  (foreach e cpyLines (if e (col:tag e pid "CL")))
  (if cpyHatch (col:tag cpyHatch pid "CH"))
  (princ "\nהשיוך הושלם.")
)

;;; ============================================================
;;;  עזרי מצב Multi
;;; ============================================================

(defun col:shape-dlg ( title / path f dclid res )
  (setq path (vl-filename-mktemp "shp" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "shp_dlg : dialog {" f)
  (write-line (strcat "  label = \"" title "\";") f)
  (write-line "  : radio_column {" f)
  (write-line "    : radio_button { key=\"P\"; label=\"Polygon\"; value=\"1\"; }" f)
  (write-line "    : radio_button { key=\"R\"; label=\"Rectangle\"; }" f)
  (write-line "    : radio_button { key=\"C\"; label=\"Circle\"; }" f)
  (write-line "    : radio_button { key=\"E\"; label=\"Ellipse\"; }" f)
  (write-line "  }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  (setq dclid (load_dialog path))
  (if (not (new_dialog "shp_dlg" dclid))
    (progn (unload_dialog dclid) (vl-file-delete path) (exit)))
  (set_tile "P" "1")
  (setq res "P")
  (action_tile "P" "(setq res \"P\")")
  (action_tile "R" "(setq res \"R\")")
  (action_tile "C" "(setq res \"C\")")
  (action_tile "E" "(setq res \"E\")")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(setq res nil)(done_dialog 0)")
  (start_dialog)
  (unload_dialog dclid)
  (vl-file-delete path)
  res
)

(defun col:ask-more-dlg ( / path f dclid res )
  (setq path (vl-filename-mktemp "more" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "more_dlg : dialog {" f)
  (write-line "  label = \"חלוקה פנימית נוספת?\";" f)
  (write-line "  : row {" f)
  (write-line "    : button { key=\"yes\";  label=\"  כן  \"; is_default=true; }" f)
  (write-line "    : button { key=\"no\";   label=\"  לא  \"; }" f)
  (write-line "    : button { key=\"back\"; label=\"  חזרה לשלב הקודם  \"; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  (setq dclid (load_dialog path))
  (if (not (new_dialog "more_dlg" dclid))
    (progn (unload_dialog dclid) (vl-file-delete path) (exit)))
  (setq res "No")
  (action_tile "yes"  "(setq res \"Yes\")(done_dialog 1)")
  (action_tile "no"   "(setq res \"No\")(done_dialog 1)")
  (action_tile "back" "(setq res \"Esc\")(done_dialog 1)")
  (start_dialog)
  (unload_dialog dclid)
  (vl-file-delete path)
  res
)

;;; ============================================================
;;;  חלון תפריט גנרי — כפתור לכל אופציה ברשימה
;;;  opts    = רשימת זוגות (key . label)
;;;  default = key של הכפתור שיודגש כברירת מחדל (או nil)
;;;  מחזיר את ה-key שנבחר, או nil אם בוטל
;;; ============================================================
(defun col:menu-dlg ( title opts default / path f dclid res )
  (setq path (vl-filename-mktemp "menu" nil ".dcl"))
  (setq f (open path "w"))
  (write-line "menu_dlg : dialog {" f)
  (write-line (strcat "  label = \"" title "\";") f)
  (write-line "  : column {" f)
  (foreach o opts
    (write-line
      (strcat "    : button { key=\"" (car o) "\"; label=\"  " (cdr o) "  \";"
              (if (= (car o) default) " is_default=true;" "")
              " }")
      f))
  (write-line "  }" f)
  (write-line "  spacer;" f)
  (write-line "  : button { key=\"cancel_btn\"; label=\"  ביטול  \"; }" f)
  (write-line "}" f)
  (close f)
  (setq dclid (load_dialog path))
  (if (not (new_dialog "menu_dlg" dclid))
    (progn (unload_dialog dclid) (vl-file-delete path) (exit)))
  (setq res nil)
  (foreach o opts
    (action_tile (car o) (strcat "(setq res \"" (car o) "\")(done_dialog 1)")))
  (action_tile "cancel_btn" "(setq res nil)(done_dialog 0)")
  (start_dialog)
  (unload_dialog dclid)
  (vl-file-delete path)
  res
)

(defun col:draw-shape ( prompt / inp opt pts cr rp )
  (setq inp (col:shape-dlg prompt))
  (if (not inp)
    nil   ;; Escape / Cancel בחלון — יציאה מיידית, אין צורה
    (progn
      (setq opt (cond
        ((= inp "R") "Rectangle")
        ((= inp "C") "Circle")
        ((= inp "E") "Ellipse")
        (t "Polygon")))
      (cond
        ((= opt "Polygon")
         (setq pts (col:draw-poly))
         (if (>= (length pts) 3) (cons "P" pts) nil))
        ((= opt "Rectangle")
         (setq rp (col:draw-rect))
         (if rp (list "R" (car rp) (cadr rp)) nil))
        ((= opt "Circle")
         (setq cr (col:draw-circle))
         (if cr (list "C" (car cr) (cadr cr)) nil))
        ((= opt "Ellipse")
         (setq pts (col:draw-ellipse))
         (if pts (list "E" (car pts) (cadr pts) (caddr pts)) nil)))))
)

;;; יצירת ישות מתיאור צורה
(defun col:mk-ent-from-shape ( shp lay )
  (cond
    ((= (car shp) "P") (col:mk-pline (cdr shp) lay))
    ((= (car shp) "R") (col:mk-rect (cadr shp) (caddr shp) lay))
    ((= (car shp) "C") (col:mk-circle (cadr shp) (caddr shp) lay))
    ((= (car shp) "E") (col:mk-ellipse (list (cadr shp) (caddr shp) (cadddr shp)) lay)))
)

;;; הזחת תיאור צורה
(defun col:offset-shape ( shp disp )
  (cond
    ((= (car shp) "P")
     (cons "P" (col:offset-pts (cdr shp) disp)))
    ((= (car shp) "R")
     (list "R" (mapcar '+ (cadr shp) disp) (mapcar '+ (caddr shp) disp)))
    ((= (car shp) "C")
     (list "C" (mapcar '+ (cadr shp) disp) (caddr shp)))
    ((= (car shp) "E")
     (list "E"
       (mapcar '+ (cadr shp) disp)
       (mapcar '+ (caddr shp) disp)
       (mapcar '+ (cadddr shp) disp))))
)

;;; האצ' על מספר גבולות: חיצוני + פנימיים
(defun col:mk-hatch-multi ( outer inners lay pat sc ang / ss before )
  (setvar "HPNAME" pat)
  (setvar "HPSCALE" sc)
  (setvar "HPANG" ang)
  (setq ss (ssadd))
  (ssadd outer ss)
  (foreach inn inners (ssadd inn ss))
  (setq before (entlast))
  (command "_.-HATCH" "_LA" lay "_S" ss "" "")
  (col:find-new-hatch before lay)
)

;;; מבטיח חלוקה פנימית ראשונה — אם בוטלה, מבטל גם את הצורה החיצונית
;;; (אין מצב חוקי של Multi בלי אף חלוקה פנימית)
(defun col:multi-ensure-first-inner ( lay outer-ent / shp )
  (setq shp (col:draw-shape "חלוקה פנימית ראשונה"))
  (if shp
    (list (list shp) (list (col:mk-ent-from-shape shp lay)))
    (progn (col:safe-del outer-ent) (princ "\nבוטל.") (exit)))
)

;;; הסרת חלוקה אחרונה מהרשימות; אם נשארה רשימה ריקה — מצייר חלוקה ראשונה מחדש
;;; (הצורה החיצונית לעולם לא נמחקת בשלב הזה)
(defun col:multi-undo-last ( lay outer-ent inners inners-ents / pair )
  (col:safe-del (last inners-ents))
  (setq inners-ents (reverse (cdr (reverse inners-ents))))
  (setq inners     (reverse (cdr (reverse inners))))
  (if (null inners)
    (progn
      (setq pair (col:multi-ensure-first-inner lay outer-ent))
      (list (car pair) (cadr pair)))
    (list inners inners-ents))
)

;;; איסוף גבולות ל-Multi
(defun col:draw-multi ( lay / outer outer-ent inners inners-ents shp ans pair )
  (setq outer (col:draw-shape "גבול חיצוני"))
  (if (not outer) (progn (princ "\nבוטל.") (exit)))
  (setq outer-ent (col:mk-ent-from-shape outer lay))

  (setq pair (col:multi-ensure-first-inner lay outer-ent))
  (setq inners (car pair))
  (setq inners-ents (cadr pair))

  (while
    (progn
      (setq ans (col:ask-more-dlg))
      (cond
        ((= ans "Esc")
         (setq pair (col:multi-undo-last lay outer-ent inners inners-ents))
         (setq inners (car pair))
         (setq inners-ents (cadr pair))
         t)
        ((= ans "Yes") t)
        (t nil)))
    (setq shp (col:draw-shape "חלוקה פנימית"))
    (if shp
      (progn
        (setq inners (append inners (list shp)))
        (setq inners-ents (append inners-ents (list (col:mk-ent-from-shape shp lay)))))
      (progn
        (setq pair (col:multi-undo-last lay outer-ent inners inners-ents))
        (setq inners (car pair))
        (setq inners-ents (cadr pair)))))
  (list outer outer-ent inners inners-ents))

;;; ============================================================
;;;  הפקודה הראשית DCOL
;;; ============================================================
(defun c:DCOL ( / s shape top-opt opt pts cen r p1 p2
                 srcLine srcHatch cpyLine cpyHatch
                 disp pid srcAxis cpyAxis axLay
                 mdata srcOuter srcInners cpyOuter cpyInners )
  (col:regapp)
  (setq s (col:ensure-settings))
  (setq shape (nth 13 s))
  ;; תפריט עליון
  (setq top-opt (col:menu-dlg "DCOL"
    (list (cons "ColWall" "COL / WALL") (cons "Geometry" "Geometry") (cons "Settings" "Settings"))
    "ColWall"))
  (if (not top-opt) (progn (princ "\nבוטל.") (exit)))
  (cond
    ((= top-opt "Settings")
     (setq s (col:dlg s))
     (if s (col:put-settings s))
     (exit))
    ((= top-opt "Geometry")
     (col:do-geometry s)
     (exit)))
  ;; --- COL/WALL: תפריט משני ---
  (setq opt (col:menu-dlg "COL / WALL"
    (list (cons "Polygon" "Polygon") (cons "Rectangle" "Rectangle")
          (cons "Circle" "Circle") (cons "Ellipse" "Ellipse")
          (cons "Multi" "Multi") (cons "Attach" "Attach")
          (cons "Update" "Update"))
    (cond ((= shape "P") "Polygon")
          ((= shape "R") "Rectangle")
          ((= shape "C") "Circle")
          ((= shape "E") "Ellipse")
          ((= shape "M") "Multi")
          (t "Polygon"))))
  (if (not opt) (progn (princ "\nבוטל.") (exit)))
  (cond
    ((= opt "Polygon")   (setq shape "P"))
    ((= opt "Rectangle") (setq shape "R"))
    ((= opt "Circle")    (setq shape "C"))
    ((= opt "Ellipse")   (setq shape "E"))
    ((= opt "Multi")     (setq shape "M"))
    ((= opt "Attach")
     (col:put-settings (append (col:sublist s 0 13) (list shape) (col:sublist s 14 (length s))))
     (setq s (col:get-settings))
     (col:do-attach s (col:disp s) (col:newid))
     (exit))
    ((= opt "Update")
     (c:DCOLUP)
     (exit)))
  ;; שמור צורה פעילה
  (col:put-settings
    (append (col:sublist s 0 13) (list shape) (col:sublist s 14 (length s))))
  (setq s (col:get-settings))
  (setq disp (col:disp s))
  (setq pid (col:newid))
  ;; ----- ציור ויצירה -----
  (cond
    ((= shape "R")
     (setq pts (col:draw-rect))
     (if pts
       (progn
         (setq srcLine (col:mk-rect (car pts) (cadr pts) (nth 0 s)))
         (setq srcHatch (col:mk-hatch srcLine (nth 1 s)
                          (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
         (setq cpyLine (col:mk-rect
                         (mapcar '+ (car pts) disp)
                         (mapcar '+ (cadr pts) disp) (nth 2 s)))
         (setq cpyHatch (col:mk-hatch cpyLine (nth 3 s)
                          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))
         (if (= "1" (nth 15 s))
           (progn
             (setq axLay (nth 14 s))
             (setq srcAxis (col:mk-axis "R" pts axLay))
             (setq cpyAxis (col:mk-axis "R"
               (list (mapcar '+ (car pts) disp) (mapcar '+ (cadr pts) disp)) axLay)))))))
    ((= shape "P")
     (setq pts (col:draw-poly))
     (if (>= (length pts) 3)
       (progn
         (setq srcLine (col:mk-pline pts (nth 0 s)))
         (setq srcHatch (col:mk-hatch srcLine (nth 1 s)
                          (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
         (setq cpyLine (col:mk-pline (col:offset-pts pts disp) (nth 2 s)))
         (setq cpyHatch (col:mk-hatch cpyLine (nth 3 s)
                          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))))))
    ((= shape "C")
     (setq cen (col:draw-circle))
     (setq r (cadr cen) cen (car cen))
     (setq srcLine (col:mk-circle cen r (nth 0 s)))
     (setq srcHatch (col:mk-hatch srcLine (nth 1 s)
                      (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
     (setq cpyLine (col:mk-circle (mapcar '+ cen disp) r (nth 2 s)))
     (setq cpyHatch (col:mk-hatch cpyLine (nth 3 s)
                      (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))
     (if (= "1" (nth 16 s))
       (progn
         (setq axLay (nth 14 s))
         (setq srcAxis (col:mk-axis "C" (list cen r) axLay))
         (setq cpyAxis (col:mk-axis "C" (list (mapcar '+ cen disp) r) axLay)))))
    ((= shape "E")
     (setq pts (col:draw-ellipse))
     (if pts
       (progn
         (setq srcLine (col:mk-ellipse pts (nth 0 s)))
         (setq srcHatch (col:mk-hatch srcLine (nth 1 s)
                          (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
         (setq cpyLine (col:mk-ellipse (col:offset-ell pts disp) (nth 2 s)))
         (setq cpyHatch (col:mk-hatch cpyLine (nth 3 s)
                          (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))
         (if (= "1" (nth 17 s))
           (progn
             (setq axLay (nth 14 s))
             (setq srcAxis (col:mk-axis "E" pts axLay))
             (setq cpyAxis (col:mk-axis "E" (col:offset-ell pts disp) axLay)))))))
    ((= shape "M")
     (setq mdata (col:draw-multi (nth 0 s)))
     (setq srcOuter  (cadr mdata))
     (setq srcInners (cadddr mdata))
     (setq srcHatch  (col:mk-hatch-multi srcOuter srcInners (nth 1 s)
                       (nth 4 s) (atof (nth 5 s)) (atof (nth 6 s))))
     (setq cpyOuter  (col:mk-ent-from-shape
                       (col:offset-shape (car mdata) disp) (nth 2 s)))
     (setq cpyInners (mapcar '(lambda (shp)
                                (col:mk-ent-from-shape (col:offset-shape shp disp) (nth 2 s)))
                              (caddr mdata)))
     (setq cpyHatch  (col:mk-hatch-multi cpyOuter cpyInners (nth 3 s)
                       (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))))
  ;; ----- תיוג הזוג -----
  (cond
    ((= shape "M")
     (if srcOuter  (col:tag srcOuter  pid "SOL"))
     (foreach e srcInners (if e (col:tag e pid "SIL")))
     (if srcHatch  (col:tag srcHatch  pid "SH"))
     (if cpyOuter  (col:tag cpyOuter  pid "COL"))
     (foreach e cpyInners (if e (col:tag e pid "CIL")))
     (if cpyHatch  (col:tag cpyHatch  pid "CH")))
    (t
     (if srcLine  (col:tag srcLine  pid "SL"))
     (if srcHatch (col:tag srcHatch pid "SH"))
     (if cpyLine  (col:tag cpyLine  pid "CL"))
     (if cpyHatch (col:tag cpyHatch pid "CH"))
     (if srcAxis (foreach e srcAxis (if e (col:tag e pid "SAX"))))
     (if cpyAxis (foreach e cpyAxis (if e (col:tag e pid "CAX"))))))
  (princ)
)

;;; אליפסה מ-(cen p1 p2)
(defun col:mk-ellipse ( prm lay / cen p1 p2 maj ratio )
  (setq cen (car prm) p1 (cadr prm) p2 (caddr prm))
  (setq maj (mapcar '- p1 cen))
  (setq ratio (/ (distance cen p2) (distance cen p1)))
  (entmakex (list '(0 . "ELLIPSE") '(100 . "AcDbEntity")
                  (cons 8 lay) '(100 . "AcDbEllipse")
                  (cons 10 cen) (cons 11 maj)
                  (cons 40 ratio) '(41 . 0.0) (cons 42 (* 2 pi))))
)

;;; ============================================================
;;;  קווי ציר
;;; ============================================================

(defun col:mk-line ( p1 p2 lay )
  (entmakex (list '(0 . "LINE") (cons 8 lay) (cons 10 p1) (cons 11 p2)))
)

(defun col:mk-axis-from-ent ( ent axLay / ed type cen maj ratio mlen mnlen ep1 p2 ux uy pts z )
  (if (not (and ent (entget ent))) nil
    (progn
      (setq ed   (entget ent))
      (setq type (cdr (assoc 0 ed)))
      (cond
        ((= type "CIRCLE")
         (col:mk-axis "C"
           (list (cdr (assoc 10 ed)) (cdr (assoc 40 ed)))
           axLay))
        ((= type "ELLIPSE")
         (setq cen   (cdr (assoc 10 ed)))
         (setq maj   (cdr (assoc 11 ed)))
         (setq ratio (cdr (assoc 40 ed)))
         (setq mlen  (distance '(0 0 0) maj))
         (if (> mlen 0)
           (progn
             (setq mnlen (* ratio mlen))
             (setq ep1 (mapcar '+ cen maj))
             (setq ux (/ (- (cadr maj)) mlen))
             (setq uy (/ (car  maj)    mlen))
             (setq p2 (list (+ (car  cen) (* ux mnlen))
                            (+ (cadr cen) (* uy mnlen))
                            (caddr cen)))
             (col:mk-axis "E" (list cen ep1 p2) axLay))
           nil))
        ((= type "LWPOLYLINE")
         (setq pts (mapcar 'cdr (vl-remove-if-not
                                  '(lambda (x) (= (car x) 10)) ed)))
         (if (= (length pts) 4)
           (progn
             (setq z (if (assoc 38 ed) (cdr (assoc 38 ed)) 0.0))
             (col:mk-axis "R"
               (list (list (car (car   pts)) (cadr (car   pts)) z)
                     (list (car (caddr pts)) (cadr (caddr pts)) z))
               axLay))
           nil))
        (t nil))))
)

(defun col:mk-axis ( shape-type geom lay / cx cy cz
                     hw hh r
                     cen ep1 ep2 mlen mnlen
                     ux uy px py res )
  (setq res '())
  (cond
    ((= shape-type "R")
     (setq cx (/ (+ (car  (car geom)) (car  (cadr geom))) 2.0))
     (setq cy (/ (+ (cadr (car geom)) (cadr (cadr geom))) 2.0))
     (setq cz (caddr (car geom)))
     (setq hw (* (abs (- (car  (cadr geom)) (car  (car geom)))) 0.85))
     (setq hh (* (abs (- (cadr (cadr geom)) (cadr (car geom)))) 0.85))
     (setq res (list
       (col:mk-line (list (- cx hw) cy cz) (list (+ cx hw) cy cz) lay)
       (col:mk-line (list cx (- cy hh) cz) (list cx (+ cy hh) cz) lay))))
    ((= shape-type "C")
     (setq cen (car geom) r (* (cadr geom) 1.7))
     (setq cx (car cen) cy (cadr cen) cz (caddr cen))
     (setq res (list
       (col:mk-line (list (- cx r) cy cz) (list (+ cx r) cy cz) lay)
       (col:mk-line (list cx (- cy r) cz) (list cx (+ cy r) cz) lay))))
    ((= shape-type "E")
     (setq cen (car geom) ep1 (cadr geom) ep2 (caddr geom))
     (setq cx (car cen) cy (cadr cen) cz (caddr cen))
     (setq mlen  (* (distance cen ep1) 1.7))
     (setq mnlen (* (distance cen ep2) 1.7))
     (setq ux (/ (- (car  ep1) cx) (distance cen ep1)))
     (setq uy (/ (- (cadr ep1) cy) (distance cen ep1)))
     (setq px (- uy))
     (setq py ux)
     (setq res (list
       (col:mk-line (list (- cx (* ux mlen)) (- cy (* uy mlen)) cz)
                    (list (+ cx (* ux mlen)) (+ cy (* uy mlen)) cz) lay)
       (col:mk-line (list (- cx (* px mnlen)) (- cy (* py mnlen)) cz)
                    (list (+ cx (* px mnlen)) (+ cy (* py mnlen)) cz) lay)))))
  res
)

;;; ----- עזרי הזחה -----
(defun col:offset-pts ( pts disp )
  (mapcar '(lambda (p) (mapcar '+ p disp)) pts)
)
(defun col:offset-ell ( prm disp )
  (mapcar '(lambda (p) (mapcar '+ p disp)) prm)
)

;;; ----- תת-רשימה -----
(defun col:sublist ( lst start end / i r )
  (setq i 0 r '())
  (foreach x lst
    (if (and (>= i start) (< i end)) (setq r (cons x r)))
    (setq i (1+ i)))
  (reverse r)
)

;;; ----- מזהה זוג ייחודי -----
(if (not *COL-COUNTER*) (setq *COL-COUNTER* 0))
(defun col:newid ()
  (setq *COL-COUNTER* (1+ *COL-COUNTER*))
  (strcat (rtos (getvar "MILLISECS") 2 0) "-" (itoa *COL-COUNTER*))
)

;;; ============================================================
;;;  Nested — חישוב צורה פנימית (לשימוש ב-Hole)
;;; ============================================================

(defun col:nested-inner-rect ( p1 p2 / tl tr bl dx dy z px py )
  (setq tl p1)
  (setq z  (caddr p1))
  (setq dx (- (car  p2) (car  p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  (setq tr (list (car p2) (cadr p1) z))
  (setq bl (list (car p1) (cadr p2) z))
  (setq px (+ (car  tl) (* 0.15 dx)))
  (setq py (+ (cadr tl) (* 0.15 dy)))
  (list tl tr (list px py z) bl)
)

(defun col:nested-inner-poly ( pts / n i p1 p2 len max-len max-idx
                                    s1-p1 s1-p2
                                    prev-i prev-p1 prev-p2 prev-len
                                    next-p1 next-p2 next-len
                                    s2-len tl tr bl z px py )
  (setq n (length pts))
  (if (< n 3) nil
    (progn
      (setq max-len 0  max-idx 0  i 0)
      (while (< i n)
        (setq p1 (nth i pts))
        (setq p2 (nth (rem (1+ i) n) pts))
        (setq len (distance p1 p2))
        (if (> len max-len) (progn (setq max-len len) (setq max-idx i)))
        (setq i (1+ i)))
      (setq s1-p1 (nth max-idx pts))
      (setq s1-p2 (nth (rem (1+ max-idx) n) pts))
      (setq prev-i   (rem (+ n (1- max-idx)) n))
      (setq prev-p1  (nth prev-i pts))
      (setq prev-p2  s1-p1)
      (setq prev-len (distance prev-p1 prev-p2))
      (setq next-p1  s1-p2)
      (setq next-p2  (nth (rem (+ max-idx 2) n) pts))
      (setq next-len (distance next-p1 next-p2))
      (if (>= prev-len next-len)
        (progn
          (setq tl s1-p1  tr s1-p2  bl prev-p1  s2-len prev-len))
        (progn
          (setq tl s1-p2  tr s1-p1  bl next-p2  s2-len next-len)))
      (setq z  (caddr tl))
      (setq px (+ (car  tl) (* 0.15 (- (car  tr) (car  tl)))
                             (* 0.15 (- (car  bl) (car  tl)))))
      (setq py (+ (cadr tl) (* 0.15 (- (cadr tr) (cadr tl)))
                             (* 0.15 (- (cadr bl) (cadr tl)))))
      (list tl tr (list px py z) bl)))
)

;;; ============================================================
;;;  Geometry — תפריט ראשי: Border / Hole
;;; ============================================================
(defun col:do-geometry ( s / choice )
  (setq choice (col:menu-dlg "Geometry"
    (list (cons "Border" "Border") (cons "Hole" "Hole"))
    "Border"))
  (if (not choice) (progn (princ "\nבוטל.") (exit)))
  (cond
    ((= choice "Border") (col:do-border s))
    ((= choice "Hole")   (col:do-hole   s)))
)

;;; ============================================================
;;;  Border — קו מתאר בלבד + עותק מוזח (אף פעם בלי חור/האצ')
;;;  Rectangle = מלבן סגור
;;;  Polygon   = מצולע סגור
;;;  Polyline  = פולי-ליין פתוח (2+ נקודות)
;;; ============================================================
(defun col:do-border ( s / disp pid choice pts p1 p2 src-ent cpy-ent lay-nest )
  (setq disp     (col:disp s))
  (setq pid      (col:newid))
  (setq lay-nest (nth 18 s))
  (setq choice (col:menu-dlg "Border"
    (list (cons "Rectangle" "Rectangle") (cons "Polygon" "Polygon") (cons "Polyline" "Polyline"))
    "Rectangle"))
  (if (not choice) (progn (princ "\nבוטל.") (exit)))
  (cond
    ((= choice "Rectangle")
     (setq pts (col:draw-rect))
     (if (not pts) (progn (princ "\nבוטל.") (exit)))
     (setq p1 (car pts)  p2 (cadr pts))
     (setq src-ent (col:mk-rect p1 p2 lay-nest))
     (setq cpy-ent (col:mk-rect (mapcar '+ p1 disp) (mapcar '+ p2 disp) lay-nest)))
    ((= choice "Polygon")
     (setq pts (col:draw-poly))
     (if (< (length pts) 3) (progn (princ "\nבוטל.") (exit)))
     (setq src-ent (col:mk-pline pts lay-nest))
     (setq cpy-ent (col:mk-pline (col:offset-pts pts disp) lay-nest)))
    ((= choice "Polyline")
     (setq pts (col:draw-polyline))
     (if (or (not pts) (< (length pts) 2)) (progn (princ "\nבוטל.") (exit)))
     (setq src-ent (col:mk-pline-open-lay pts lay-nest))
     (setq cpy-ent (col:mk-pline-open-lay (col:offset-pts pts disp) lay-nest))))
  (if src-ent (col:tag src-ent pid "BOL"))
  (if cpy-ent (col:tag cpy-ent pid "CBOL"))
  (princ "\nBorder נוצר.")
)

;;; ============================================================
;;;  Hole — צורה סגורה + חישוב פנימי (0.15) + האצ' + עותק מוזח
;;;  (תמיד עם חור — אם רוצים בלי, יש להשתמש ב-Border)
;;; ============================================================
(defun col:do-hole ( s / disp pid choice pts p1 p2 inner-pts
                         outer-ent inner-ent hatch-ent
                         cpy-outer cpy-inner cpy-hatch
                         lay-nest in-nest hl-nest pat-nest sc-nest ang-nest )
  (setq disp     (col:disp s))
  (setq pid      (col:newid))
  (setq lay-nest (nth 18 s))
  (setq hl-nest  (nth 19 s))
  (setq pat-nest (nth 20 s))
  (setq sc-nest  (atof (nth 21 s)))
  (setq ang-nest (atof (nth 22 s)))
  (setq in-nest  (nth 23 s))
  (setq choice (col:menu-dlg "Hole"
    (list (cons "Rectangle" "Rectangle") (cons "Polygon" "Polygon"))
    "Rectangle"))
  (if (not choice) (progn (princ "\nבוטל.") (exit)))
  (cond
    ((= choice "Rectangle")
     (setq pts (col:draw-rect))
     (if (not pts) (progn (princ "\nבוטל.") (exit)))
     (setq p1 (car pts)  p2 (cadr pts))
     (setq outer-ent (col:mk-rect p1 p2 lay-nest))
     (setq inner-pts (col:nested-inner-rect p1 p2))
     (setq inner-ent (col:mk-pline inner-pts in-nest))
     (setq hatch-ent (col:mk-hatch inner-ent hl-nest pat-nest sc-nest ang-nest))
     (setq cpy-outer (col:mk-rect (mapcar '+ p1 disp) (mapcar '+ p2 disp) lay-nest))
     (setq cpy-inner (col:mk-pline (col:offset-pts inner-pts disp) in-nest))
     (setq cpy-hatch (col:mk-hatch cpy-inner hl-nest pat-nest sc-nest ang-nest)))
    ((= choice "Polygon")
     (setq pts (col:draw-poly))
     (if (< (length pts) 3) (progn (princ "\nבוטל.") (exit)))
     (setq outer-ent (col:mk-pline pts lay-nest))
     (setq inner-pts (col:nested-inner-poly pts))
     (if inner-pts
       (progn
         (setq inner-ent (col:mk-pline inner-pts in-nest))
         (setq hatch-ent (col:mk-hatch inner-ent hl-nest pat-nest sc-nest ang-nest)))
       (progn
         (princ "\nלא ניתן לחשב צורה פנימית.")
         (setq inner-ent nil  hatch-ent nil)))
     (setq cpy-outer (col:mk-pline (col:offset-pts pts disp) lay-nest))
     (if inner-pts
       (progn
         (setq cpy-inner (col:mk-pline (col:offset-pts inner-pts disp) in-nest))
         (setq cpy-hatch (col:mk-hatch cpy-inner hl-nest pat-nest sc-nest ang-nest)))
       (setq cpy-inner nil  cpy-hatch nil))))
  (if outer-ent (col:tag outer-ent pid "NOL"))
  (if inner-ent (col:tag inner-ent pid "NIL"))
  (if hatch-ent (col:tag hatch-ent pid "NH"))
  (if cpy-outer (col:tag cpy-outer pid "CNOL"))
  (if cpy-inner (col:tag cpy-inner pid "CNIL"))
  (if cpy-hatch (col:tag cpy-hatch pid "CNH"))
  (princ "\nHole נוצר.")
)

;;; ============================================================
;;;  DCOLUP - עזרים
;;; ============================================================

(defun col:read-xdata ( ent / xd app-data fields n )
  (setq xd (assoc -3 (entget ent (list *COL-XAPP*))))
  (if xd
    (progn
      (setq app-data (assoc *COL-XAPP* (cdr xd)))
      (if app-data
        (progn
          (setq fields (mapcar 'cdr (cdr app-data)))
          (setq n (length fields))
          (cond
            ((= n 3) (list (cadr fields) (caddr fields)))
            ((= n 2) (list (car fields) (cadr fields)))
            (t nil)))
        nil))
    nil)
)

(defun col:group-by-pid ( lst / result pid role ent pg rg )
  (setq result '())
  (foreach item lst
    (setq pid (car item) role (cadr item) ent (caddr item))
    (setq pg (assoc pid result))
    (if pg
      (progn
        (setq rg (assoc role (cdr pg)))
        (if rg
          (setq result (subst
            (cons pid (subst (append rg (list ent)) rg (cdr pg)))
            pg result))
          (setq result (subst
            (append pg (list (list role ent)))
            pg result))))
      (setq result (cons (list pid (list role ent)) result))))
  result
)

(defun col:clone-hatch-offset ( sh disp lay pat sc ang / ed dx dy dz new-ed item code solid-dst edge-type )
  (if (not (and sh (entget sh))) nil
    (progn
      (setq ed (entget sh))
      (setq dx (car disp) dy (cadr disp) dz (caddr disp))
      (setq solid-dst (= "SOLID" pat))
      (setq new-ed '())
      (setq edge-type 0)
      (foreach item ed
        (setq code (car item))
        (if (= code 72) (setq edge-type (cdr item)))
        (cond
          ((member code '(-1 -2 -3 5 102 330 340 360)) nil)
          ((member code '(53 43 44 45 46 49 79)) nil)
          ((= code 78) (setq new-ed (append new-ed (list (cons 78 0)))))
          ((= code 8)  (setq new-ed (append new-ed (list (cons 8  lay)))))
          ((= code 2)  (setq new-ed (append new-ed (list (cons 2  pat)))))
          ((= code 70) (setq new-ed (append new-ed (list (cons 70 (if solid-dst 1 0))))))
          ((= code 76) (setq new-ed (append new-ed (list (cons 76 (if solid-dst 0 1))))))
          ((= code 41) (if (not solid-dst)
                         (setq new-ed (append new-ed (list (cons 41 sc))))))
          ((= code 52) (if (not solid-dst)
                         (setq new-ed (append new-ed (list (cons 52 ang))))))
          ((= code 62) (setq new-ed (append new-ed (list (cons 62 256)))))
          ((= code 10)
           (setq new-ed (append new-ed (list
             (cons 10 (list (+ (car  (cdr item)) dx)
                            (+ (cadr (cdr item)) dy)
                            (caddr (cdr item))))))))
          ((= code 11)
           (if (= edge-type 3)
             (setq new-ed (append new-ed (list item)))
             (setq new-ed (append new-ed (list
               (cons 11 (list (+ (car  (cdr item)) dx)
                              (+ (cadr (cdr item)) dy)
                              (caddr (cdr item)))))))))
          (t (setq new-ed (append new-ed (list item))))))
      (entmakex new-ed)))
)

(defun col:safe-del ( ent )
  (if (and ent (entget ent)) (entdel ent))
)

(defun col:clone-line-offset ( src disp lay / ed type cl-pts cl-closed )
  (if (not (and src (entget src))) (progn (princ "\n[DCOLUP] מקור חסר.") nil)
    (progn
      (setq ed (entget src))
      (setq type (cdr (assoc 0 ed)))
      (cond
        ((= type "LWPOLYLINE")
         (progn
           (setq cl-pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) ed)))
           (setq cl-closed (= 1 (cdr (assoc 70 ed))))
           (if cl-closed
             (col:mk-pline (col:offset-pts cl-pts disp) lay)
             (col:mk-pline-open-lay (col:offset-pts cl-pts disp) lay))))
        ((= type "CIRCLE")
         (col:mk-circle (mapcar '+ (cdr (assoc 10 ed)) disp)
                        (cdr (assoc 40 ed)) lay))
        ((= type "ELLIPSE")
         (entmakex (list '(0 . "ELLIPSE") '(100 . "AcDbEntity")
                   (cons 8 lay) '(100 . "AcDbEllipse")
                   (cons 10 (mapcar '+ (cdr (assoc 10 ed)) disp))
                   (cons 11 (cdr (assoc 11 ed)))
                   (cons 40 (cdr (assoc 40 ed)))
                   '(41 . 0.0) (cons 42 (* 2 pi)))))
        (t nil))))
)

;;; ============================================================
;;;  col:sync-pair — סנכרון זוג בודד
;;; ============================================================
(defun col:sync-pair ( pg s / pid roles has-dup disp assoc?
                         sh sl-list cl-list sil-list cil-list
                         sol col-ent
                         new-cl-list new-ch new-col new-cil
                         new-sax new-cax ax-src-type ax-need
                         nol-ent nil-line lay-nest in-nest hl-nest pat-nest
                         sc-nest ang-nest new-cnol new-cnil new-cnh
                         bol-ent new-cbol )
  (setq pid (car pg))
  (setq roles (cdr pg))
  (setq disp (col:disp s))
  (setq has-dup nil)
  (foreach r roles
    (if (and (member (car r) '("SH" "CH"))
             (> (length (cdr r)) 1))
      (setq has-dup t)))
  (if has-dup
    (princ (strcat "\n[DCOLUP] זוג " pid ": כפילויות — מדולג."))
    (progn
      (setq sh      (cadr (assoc "SH"  roles)))
      (setq sl-list (cdr  (assoc "SL"  roles)))
      (setq cl-list (cdr  (assoc "CL"  roles)))
      (setq sol     (cadr (assoc "SOL" roles)))
      (setq col-ent (cadr (assoc "COL" roles)))
      (setq sil-list (cdr (assoc "SIL" roles)))
      (setq cil-list (cdr (assoc "CIL" roles)))
      (cond
        ;; --- זוג רגיל / Attach ---
        (sl-list
         (col:safe-del (cadr (assoc "CH" roles)))
         (foreach e cl-list (col:safe-del e))
         (setq new-cl-list
           (vl-remove nil
             (mapcar '(lambda (e) (col:clone-line-offset e disp (nth 2 s)))
                     sl-list)))
         (setq new-ch
           (if (and sh (entget sh))
             (col:clone-hatch-offset sh disp (nth 3 s)
               (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))
             (if new-cl-list
               (if (= 1 (length new-cl-list))
                 (col:mk-hatch (car new-cl-list) (nth 3 s)
                   (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))
                 (col:mk-hatch-multi (car new-cl-list) (cdr new-cl-list) (nth 3 s)
                   (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s))))
               nil)))
         (foreach e new-cl-list (if e (col:tag e pid "CL")))
         (if new-ch
           (if (= "HATCH" (cdr (assoc 0 (entget new-ch))))
             (col:tag new-ch pid "CH")
             (princ "\n[DCOLUP] אזהרה: entlast אחרי HATCH הוא לא האצ'!"))))
        ;; --- זוג Multi ---
        (sol
         (col:safe-del (cadr (assoc "CH" roles)))
         (col:safe-del col-ent)
         (foreach e cil-list (col:safe-del e))
         (setq new-col (col:clone-line-offset sol disp (nth 2 s)))
         (setq new-cil
           (vl-remove nil
             (mapcar '(lambda (e) (col:clone-line-offset e disp (nth 2 s)))
                     sil-list)))
         (setq new-ch
           (if new-col
             (col:mk-hatch-multi new-col new-cil (nth 3 s)
               (nth 7 s) (atof (nth 8 s)) (atof (nth 9 s)))
             nil))
         (if new-col (col:tag new-col pid "COL"))
         (foreach e new-cil (if e (col:tag e pid "CIL")))
         (if new-ch (col:tag new-ch pid "CH")))
        ;; --- זוג Nested / Hole ---
        ((cadr (assoc "NOL" roles))
         (setq nol-ent  (cadr (assoc "NOL"  roles)))
         (setq nil-line (cadr (assoc "NIL"  roles)))
         (setq lay-nest (nth 18 s))
         (setq in-nest  (nth 23 s))
         (setq hl-nest  (nth 19 s))
         (setq pat-nest (nth 20 s))
         (setq sc-nest  (atof (nth 21 s)))
         (setq ang-nest (atof (nth 22 s)))
         (col:safe-del (cadr (assoc "CNOL" roles)))
         (col:safe-del (cadr (assoc "CNIL" roles)))
         (col:safe-del (cadr (assoc "CNH"  roles)))
         (setq new-cnol (col:clone-line-offset nol-ent  disp lay-nest))
         (setq new-cnil (if nil-line (col:clone-line-offset nil-line disp in-nest) nil))
         (setq new-cnh  (if new-cnil (col:mk-hatch new-cnil hl-nest pat-nest sc-nest ang-nest) nil))
         (if new-cnol (col:tag new-cnol pid "CNOL"))
         (if new-cnil (col:tag new-cnil pid "CNIL"))
         (if new-cnh  (col:tag new-cnh  pid "CNH")))
        ;; --- זוג Border ---
        ((cadr (assoc "BOL" roles))
         (setq bol-ent  (cadr (assoc "BOL" roles)))
         (setq lay-nest (nth 18 s))
         (col:safe-del (cadr (assoc "CBOL" roles)))
         (setq new-cbol (col:clone-line-offset bol-ent disp lay-nest))
         (if new-cbol (col:tag new-cbol pid "CBOL")))
        (t
         (princ (strcat "\n[DCOLUP] זוג " pid ": מבנה לא מוכר — מדולג."))))
      ;; --- עדכון קווי ציר ---
      (if sl-list
        (progn
          (foreach e (cdr (assoc "SAX" roles)) (col:safe-del e))
          (foreach e (cdr (assoc "CAX" roles)) (col:safe-del e))
          (setq ax-src-type (cdr (assoc 0 (entget (car sl-list)))))
          (setq ax-need
            (cond
              ((and (= ax-src-type "CIRCLE")     (= "1" (nth 16 s))) t)
              ((and (= ax-src-type "ELLIPSE")    (= "1" (nth 17 s))) t)
              ((and (= ax-src-type "LWPOLYLINE") (= "1" (nth 15 s))
                    (= 4 (length (vl-remove-if-not
                                   '(lambda (x) (= (car x) 10))
                                   (entget (car sl-list)))))) t)
              (t nil)))
          (if ax-need
            (progn
              (setq new-sax (col:mk-axis-from-ent (car sl-list) (nth 14 s)))
              (setq new-cax
                (if new-cl-list
                  (col:mk-axis-from-ent (car new-cl-list) (nth 14 s))
                  nil))
              (foreach e new-sax (if e (col:tag e pid "SAX")))
              (foreach e new-cax (if e (col:tag e pid "CAX")))))))
    ))
)

;;; ============================================================
;;;  DCOLUP — סנכרון כל הזוגות
;;; ============================================================
(defun c:DCOLUP ( / ss i ent pairs pair-list s xd )
  (col:regapp)
  (setq s (col:ensure-settings))
  (princ "\nמסנכרן זוגות...")
  (setq ss (ssget "X" (list (list -3 (list *COL-XAPP*)))))
  (if (not ss)
    (princ "\nאין זוגות בשרטוט.")
    (progn
      (setq pairs '() i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq xd (col:read-xdata ent))
        (if xd (setq pairs (cons (list (car xd) (cadr xd) ent) pairs)))
        (setq i (1+ i)))
      (setq pair-list (col:group-by-pid pairs))
      (foreach pg pair-list (col:sync-pair pg s))
      (princ (strcat "\nסנכרון הושלם. "
                     (itoa (length pair-list)) " זוגות עובדו."))))
  (princ)
)

;;; ============================================================
;;;  DCOLSET — פתיחת הגדרות ידנית
;;; ============================================================
(defun c:DCOLSET ( / s )
  (setq s (col:ensure-settings))
  (setq s (col:dlg s))
  (if s (col:put-settings s))
  (princ "\nההגדרות נשמרו.")
  (princ)
)

(princ "\n=== COL/WALL נטען. פקודות: DCOL , DCOLUP , DCOLSET ===")
(princ)
