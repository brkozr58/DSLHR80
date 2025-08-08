*&---------------------------------------------------------------------*
*& Report /DSL/HR80_P004
*&---------------------------------------------------------------------*
REPORT /dsl/hr80_p004 MESSAGE-ID /dsl/hr80.

  TABLES : sscrfields,rlgrap.
  TABLES : t510,
           t7trt01,
           /dsl/hr80_tvergd,
           t7trt02,
           /dsl/hr80_tvergi.


  INCLUDE <icon> .
  INCLUDE /dsl/hr80_i001.

  DATA : gv_error     TYPE flag .
  DATA : gs_t003      TYPE /dsl/hr80_t003.
  DATA : gt_t510      TYPE TABLE OF t510 WITH HEADER LINE .
  DATA : gt_t007      TYPE TABLE OF /dsl/hr80_t007 WITH HEADER LINE .
  DATA : gt_t7trt01   TYPE TABLE OF t7trt01 WITH HEADER LINE .
  DATA : gt_tvergd    TYPE TABLE OF /dsl/hr80_tvergd WITH HEADER LINE .
  DATA : gt_t7trt02   TYPE TABLE OF t7trt02 WITH HEADER LINE .
  DATA : gt_tvergi    TYPE TABLE OF /dsl/hr80_tvergi WITH HEADER LINE .




*  INITIALIZATION.
  INITIALIZATION.


*  AT SELECTION-SCREEN
  AT SELECTION-SCREEN   .

*  AT SELECTION-SCREEN OUTPUT.
  AT SELECTION-SCREEN OUTPUT.
    PERFORM at_selection_screen_output.


*  START-OF-SELECTION .
  START-OF-SELECTION .
    PERFORM check_paramaters CHANGING gv_error.
    CHECK gv_error IS INITIAL .
    PERFORM get_table_data.


*  END-OF-SELECTION.
  END-OF-SELECTION.
    CHECK gv_error IS INITIAL .


*&---------------------------------------------------------------------*
*& Form AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
FORM at_selection_screen_output.

  LOOP AT SCREEN.
    IF screen-name CP '*S_STATU*'.
      screen-input = 0 .
      screen-output = 1 .
    ENDIF.
    IF screen-name CP '*S_GJAHR*'.
      screen-active = 0 .
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PARAMATERS
*&---------------------------------------------------------------------*
FORM check_paramaters  CHANGING cv_check.
  IF s_grpid[] IS INITIAL .
    MESSAGE i001 DISPLAY LIKE 'E'.
    cv_check = 'X'.
  ENDIF.

  IF s_vrsid[] IS INITIAL .
    MESSAGE i003 DISPLAY LIKE 'E'.
    cv_check = 'X'.
  ENDIF.

  SELECT SINGLE * FROM /dsl/hr80_t003 INTO gs_t003
    WHERE grpid IN s_grpid[]
      AND vrsid IN s_vrsid[]
      AND statu IN s_statu[].
  IF sy-subrc NE 0 .
    MESSAGE i005 DISPLAY LIKE 'E'.
    cv_check = 'X'.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TABLE_DATA
*&---------------------------------------------------------------------*
FORM get_table_data .


  SELECT * FROM t510 INTO TABLE gt_t510
      WHERE molga EQ p_molga
        AND begda LE gs_t003-endda
        AND endda GE gs_t003-begda
    .

  SELECT * FROM t7trt01 INTO TABLE gt_t7trt01
      WHERE begda LE gs_t003-endda
        AND endda GE gs_t003-begda
    .

  SELECT * FROM t7trt02 INTO TABLE gt_t7trt02
      WHERE begda LE gs_t003-endda
        AND endda GE gs_t003-begda
    .

    gt_t007[] = VALUE #( FOR ls IN gt_t510
          (
            grpid = gs_t003-grpid
            vrsid = gs_t003-vrsid
            molga = gs_t003-molga
            trfar = ls-trfar
            trfgb = ls-trfgb
            trfkz = ls-trfkz
            trfgr = ls-trfgr
            trfst = ls-trfst
            lgart = ls-lgart
            endda = ls-endda
            begda = ls-begda
            betrg = ls-betrg
          )
           ) .

    gt_tvergd[] = VALUE #( FOR ls1 IN gt_t7trt01
          (
            grpid = gs_t003-grpid
            vrsid = gs_t003-vrsid
            molga = gs_t003-molga
            grtax = ls1-grtax
            dilim = ls1-dilim
            endda = ls1-endda
            begda = ls1-begda
            anbtr = ls1-anbtr
            enbtr = ls1-enbtr
            waers = ls1-waers
            prznt = ls1-prznt
          )
           ) .

    gt_tvergi[] = VALUE #( FOR ls2 IN gt_t7trt02
          (
            grpid = gs_t003-grpid
            vrsid = gs_t003-vrsid
            molga = gs_t003-molga
            grtax = ls2-grtax
            sskod = ls2-sskod
            ssgrp = ls2-ssgrp
            endda = ls2-endda
            begda = ls2-begda
            oezel = ls2-oezel
            cocuk = ls2-cocuk
            imbtr = ls2-imbtr
            aile  = ls2-aile
            waers = ls2-waers
             )
           ) .

    MODIFY /dsl/hr80_t007   FROM TABLE gt_t007[].
    MODIFY /dsl/hr80_tvergd FROM TABLE gt_tvergd[].
    MODIFY /dsl/hr80_tvergi FROM TABLE gt_tvergi[].


  IF gt_t007[]   IS NOT INITIAL OR
     gt_tvergd[] IS NOT INITIAL OR
     gt_tvergi[] IS NOT INITIAL .
      MESSAGE i020 .
      COMMIT WORK AND WAIT .
  ENDIF.


ENDFORM.
