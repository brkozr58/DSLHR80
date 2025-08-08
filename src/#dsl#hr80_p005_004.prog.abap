*----------------------------------------------------------------------*
***INCLUDE /DSL/HR80_P005_004.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
 SET PF-STATUS '2GUI'.

 PERFORM modify_screen_2000.


* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2000 INPUT.
  DATA : chk. CLEAR chk.
  CASE sy-ucomm.
    WHEN 'OK'.
      PERFORM itab_change CHANGING chk .
      IF chk IS INITIAL . " hata yoksa ekranı kapat
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANC'. LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN_2000
*&---------------------------------------------------------------------*
FORM modify_screen_2000 .

  LOOP AT SCREEN.

    CASE screen-group1 .
      WHEN 'N'.
        CASE /dsl/hr80_s005-mode.
          WHEN 'N'. screen-active = 1.
          WHEN 'C' OR 'I'. screen-active = 0.
        ENDCASE.
      WHEN 'C'  .
        CASE /dsl/hr80_s005-mode.
          WHEN 'N'. screen-active = 0.
          WHEN 'C' OR 'I'. screen-active = 1.
        ENDCASE.
    ENDCASE.

    IF /dsl/hr80_s005-mode EQ 'C' .
      IF screen-name EQ '/DSL/HR80_S005-PERNR'.
        screen-input = 0.
        screen-output = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ITAB_CHANGE
*&---------------------------------------------------------------------*
FORM itab_change CHANGING chk.
" Sadece dummy personel eklemede kontrol çalışsın
*  IF /dsl/hr80_s005-mode EQ 'I'.
  PERFORM read_objid_text .

*  IF /dsl/hr80_s005-mode EQ 'I' AND /dsl/hr80_s005-pernr GT '89999999'.
  IF /dsl/hr80_s005-mode EQ 'I' AND
     /dsl/hr80_s005-pernr GT go_alv->gmc_dummy.
    MESSAGE i037 DISPLAY LIKE 'E'.
    chk = 'X'. " buraya gelmişse hata almış demektir.
    EXIT.
  ENDIF.

  IF /dsl/hr80_s005-mode EQ 'N'.
    go_main->check_values(
      EXPORTING
        is_data   = /dsl/hr80_s005
        i_gjahr   = s_gjahr-low
      EXCEPTIONS
        no_begda  = 032
        no_endda  = 033
        big_begda = 010
        ne_year   = 034
        no_count  = 035
        no_refpr  = 031
    ).
  ENDIF.

  IF sy-subrc EQ 0  .
    go_main->process_itab_data(
       EXPORTING
          is_data           = /dsl/hr80_s005
          mode              = /dsl/hr80_s005-mode
          index             = /dsl/hr80_s005-zindex
      EXCEPTIONS
        no_refpr          = 031
        no_range          = 021 ).

    IF sy-subrc EQ 0.
      go_alv->record_check = 'C'." DEğişiklik kontrolü için
      EXIT.
    ENDIF.
  ENDIF.

  CASE sy-subrc.
    WHEN 031.   MESSAGE i031 DISPLAY LIKE 'E'.
    WHEN 021.   MESSAGE i021 DISPLAY LIKE 'E'.
    WHEN 032.   MESSAGE i032 DISPLAY LIKE 'E'.
    WHEN 033.   MESSAGE i033 DISPLAY LIKE 'E'.
    WHEN 010.   MESSAGE i010 DISPLAY LIKE 'E'.
    WHEN 034.   MESSAGE i034 WITH s_gjahr-low DISPLAY LIKE 'E'.
    WHEN 035.   MESSAGE i035 .
    WHEN OTHERS.
  ENDCASE.
  chk = 'X'. " buraya gelmişse hata almış demektir.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  INPUT_RFPER  INPUT
*&---------------------------------------------------------------------*
MODULE input_rfper INPUT.
  PERFORM input_rfper_read .
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INPUT_CHECK  INPUT
*&---------------------------------------------------------------------*
MODULE input_check INPUT.
  go_main->check_values(
    EXPORTING
      is_data   = /dsl/hr80_s005
      i_gjahr   = s_gjahr-low
    EXCEPTIONS
      no_begda  = 032
      no_endda  = 033
      big_begda = 010
      ne_year   = 034
      no_count  = 035
      no_refpr  = 031
  ).
  CASE sy-subrc.
    WHEN 031.   MESSAGE i031 DISPLAY LIKE 'E'.
    WHEN 021.   MESSAGE i021 DISPLAY LIKE 'E'.
    WHEN 032.   MESSAGE i032 DISPLAY LIKE 'E'.
    WHEN 033.   MESSAGE i033 DISPLAY LIKE 'E'.
    WHEN 010.   MESSAGE i010 DISPLAY LIKE 'E'.
    WHEN 034.   MESSAGE i034 WITH s_gjahr-low DISPLAY LIKE 'E'.
    WHEN 035.   MESSAGE i035 .
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INPUT_PERNR  INPUT
*&---------------------------------------------------------------------*
MODULE input_pernr INPUT.


*  /DSL/HR80_T010 tablosu için CDS den oku
  SELECT SINGLE ename AS ename
                orgeh
                orgeh_t
                plans
                plans_t
                stell
                stell_t
                kokrs
                kokrs_t
                kostl
                kostl_t
                pa1_begda AS begda
                pa1_endda AS endda
                werks AS werks_new
                btrtl AS btrtl_new
                persg AS persg_new
                persk AS persk_new
                trfar AS trfar_new
                trfgb AS trfgb_new
                trfgr AS trfgr_new
                trfst AS trfst_new
                lga01 AS lga01_new
                salry AS salry_new
      INTO CORRESPONDING FIELDS OF /dsl/hr80_s005
    FROM /dsl/hr80_ddl001
      WHERE pernr     EQ /dsl/hr80_s005-pernr
        AND grpid     EQ /dsl/hr80_s005-grpid
        AND vrsid     EQ /dsl/hr80_s005-vrsid
        AND gjahr     EQ /dsl/hr80_s005-gjahr
        AND molga     EQ /dsl/hr80_s005-molga
        AND pa1_endda GE sy-datum.

  CHECK sy-subrc NE 0 .
  " daha önce işlenmiş veriden aldıysa tekrar PA ya bakmasın

  SELECT SINGLE ename AS ename
                orgeh
                orgeh_t
                plans
                plans_t
                stell
                stell_t
                kokrs
                kokrs_t
                kostl
                kostl_t
                pa1_begda AS begda
                pa1_endda AS endda
                werks AS werks_new
                btrtl AS btrtl_new
                persg AS persg_new
                persk AS persk_new
                trfar AS trfar_new
                trfgb AS trfgb_new
                trfgr AS trfgr_new
                trfst AS trfst_new
                lga01 AS lga01_new
                salry AS salry_new
      INTO CORRESPONDING FIELDS OF /dsl/hr80_s005
    FROM /dsl/hr80_ddl002                         " PA tabloları
      WHERE pernr     EQ /dsl/hr80_s005-pernr
        AND grpid     EQ /dsl/hr80_s005-grpid
        AND vrsid     EQ /dsl/hr80_s005-vrsid
        AND gjahr     EQ /dsl/hr80_s005-gjahr
        AND molga     EQ /dsl/hr80_s005-molga
        AND pa1_endda GE sy-datum.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  OBJID_TEXT  INPUT
*&---------------------------------------------------------------------*
MODULE objid_text INPUT.

  PERFORM read_objid_text .

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module OBJID_TEXT_2000 OUTPUT
*&---------------------------------------------------------------------*
MODULE objid_text_2000 OUTPUT.

  PERFORM read_objid_text .

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form READ_OBJID_TEXT
*&---------------------------------------------------------------------*
FORM read_objid_text .

  DEFINE read_objid_text .
    CHECK &2 IS NOT INITIAL AND &2 NE '00000000'.
    SELECT SINGLE stext FROM hrp1000 INTO &3
          WHERE plvar EQ '01'
            AND otype EQ &1
            AND objid EQ &2
            AND endda GE sy-datum
            AND langu EQ sy-langu .
  END-OF-DEFINITION.

  read_objid_text : 'O' /dsl/hr80_s005-orgeh /dsl/hr80_s005-orgeh_t,
                    'S' /dsl/hr80_s005-plans /dsl/hr80_s005-plans_t,
                    'C' /dsl/hr80_s005-stell /dsl/hr80_s005-stell_t.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INPUT_RFPER_READ
*&---------------------------------------------------------------------*
FORM input_rfper_read .

*  /DSL/HR80_T010 tablosu için CDS den oku
  SELECT SINGLE ename AS rfper_e
                orgeh
                orgeh_t
                plans
                plans_t
                stell
                stell_t
                kokrs
                kokrs_t
                kostl
                kostl_t
                pa1_begda AS begda
                pa1_endda AS endda
                werks AS werks_new
                btrtl AS btrtl_new
                persg AS persg_new
                persk AS persk_new
                trfar AS trfar_new
                trfgb AS trfgb_new
                trfgr AS trfgr_new
                trfst AS trfst_new
                lga01 AS lga01_new
                salry AS salry_new
      INTO CORRESPONDING FIELDS OF /dsl/hr80_s005
    FROM /dsl/hr80_ddl001
      WHERE pernr     EQ /dsl/hr80_s005-rfper
        AND grpid     EQ /dsl/hr80_s005-grpid
        AND vrsid     EQ /dsl/hr80_s005-vrsid
        AND gjahr     EQ /dsl/hr80_s005-gjahr
        AND molga     EQ /dsl/hr80_s005-molga
        AND pa1_endda GE sy-datum.

  CHECK sy-subrc NE 0 .
  " daha önce işlenmiş veriden aldıysa tekrar PA ya bakmasın

  SELECT SINGLE ename AS rfper_e
                orgeh
                orgeh_t
                plans
                plans_t
                stell
                stell_t
                kokrs
                kokrs_t
                kostl
                kostl_t
                pa1_begda AS begda
                pa1_endda AS endda
                werks AS werks_new
                btrtl AS btrtl_new
                persg AS persg_new
                persk AS persk_new
                trfar AS trfar_new
                trfgb AS trfgb_new
                trfgr AS trfgr_new
                trfst AS trfst_new
                lga01 AS lga01_new
                salry AS salry_new
      INTO CORRESPONDING FIELDS OF /dsl/hr80_s005
    FROM /dsl/hr80_ddl002                         " PA tabloları
      WHERE pernr     EQ /dsl/hr80_s005-rfper
        AND grpid     EQ /dsl/hr80_s005-grpid
        AND vrsid     EQ /dsl/hr80_s005-vrsid
        AND gjahr     EQ /dsl/hr80_s005-gjahr
        AND molga     EQ /dsl/hr80_s005-molga
        AND pa1_endda GE sy-datum.



ENDFORM.
