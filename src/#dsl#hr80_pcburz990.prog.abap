*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_PCBURZ990
*&---------------------------------------------------------------------*


**********************************************************************
***!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
***!!!Bu include tanımlı olması gerekiyor.
***!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INCLUDE /dsl/hr80_calc.
***!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**********************************************************************



**********************************************************************
***!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**********************************************************************
*&---------------------------------------------------------------------*
*& Form FUZBDGT
*&---------------------------------------------------------------------*
FORM fuzbdgt.

  CASE as-parm1.
    WHEN '01'. "Bütçe Pers. Verileri
      "/dsl/hr80_t010 tablosundan personel için güncel verileri alınacak
      PERFORM change_person_data.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
**********************************************************************
***!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**********************************************************************




*&---------------------------------------------------------------------*
*& Form CHANGE_PERSON_DATA
*&---------------------------------------------------------------------*
FORM change_person_data .
  PERFORM refresh_tables .
  PERFORM get_budget_datas.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_TABLES
*&---------------------------------------------------------------------*
FORM refresh_tables .
  REFRESH : gt_t001,gt_t002,gt_t003,gt_t004,
            gt_t005,gt_t007,gt_t010,gt_t011,
            gt_tvergd,gt_tvergi .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BUDGET_DATAS
*&---------------------------------------------------------------------*
FORM get_budget_datas .
  DATA : lr_rfper TYPE RANGE OF /dsl/hr80_t010-rfper WITH HEADER LINE.
  DATA : lr_pernr TYPE RANGE OF /dsl/hr80_t010-pernr WITH HEADER LINE.

" PA personelleri için pernr-pernr yi al. seçim ekranında ki zli eklenen
" P_PERNR  parametresini boş göndermelisin
  IF p_pernr IS NOT INITIAL .
    lr_pernr = 'IEQ'.lr_pernr-low = p_pernr.APPEND lr_pernr.
  ELSE.
    lr_pernr = 'IEQ'.lr_pernr-low = pernr-pernr.APPEND lr_pernr.
  ENDIF.

" DUMMY personellerde PA daki sicil gönderilmeli
  IF p_rfper IS NOT INITIAL .
    lr_rfper = 'IEQ'.lr_rfper-low = p_rfper.APPEND lr_rfper.
  ENDIF.

  SELECT * FROM /dsl/hr80_t001   INTO TABLE gt_t001   WHERE molga EQ p_molga AND grpid EQ p_grpid.
  SELECT * FROM /dsl/hr80_t002   INTO TABLE gt_t002   WHERE molga EQ p_molga AND grpid EQ p_grpid.
  SELECT * FROM /dsl/hr80_t003   INTO TABLE gt_t003   WHERE molga EQ p_molga AND grpid EQ p_grpid AND vrsid EQ p_vrsid AND statu EQ p_statu.
  IF sy-subrc NE 0 OR p_statu NE '2'. "
    MESSAGE ID '/DSL/HR80' TYPE 'E' NUMBER '043'
        INTO DATA(mtext)
        WITH p_vrsid  .
    PERFORM log_budget  TABLES ptext USING '1' '80' '0' 1 mtext .
  ENDIF.

  SELECT * FROM /dsl/hr80_t004   INTO TABLE gt_t004   WHERE molga EQ p_molga AND grpid EQ p_grpid AND vrsid EQ p_vrsid.
  IF sy-subrc NE 0  .
    MESSAGE ID '/DSL/HR80' TYPE 'E' NUMBER '044'
        INTO mtext
        WITH p_vrsid  .
    PERFORM log_budget  TABLES ptext USING '1' '80' '0' 1 mtext .
  ENDIF.

  SELECT * FROM /dsl/hr80_t005   INTO TABLE gt_t005   WHERE molga EQ p_molga AND grpid EQ p_grpid AND vrsid EQ p_vrsid AND pernr IN lr_pernr[].
  SELECT * FROM /dsl/hr80_t007   INTO TABLE gt_t007   WHERE molga EQ p_molga AND grpid EQ p_grpid AND vrsid EQ p_vrsid.
  SELECT * FROM /dsl/hr80_t010   INTO TABLE gt_t010   WHERE molga EQ p_molga AND grpid EQ p_grpid AND vrsid EQ p_vrsid AND pernr IN lr_pernr[] AND rfper IN lr_rfper[].
  IF sy-subrc NE 0  .
    MESSAGE ID '/DSL/HR80' TYPE 'E' NUMBER '037'
        INTO mtext   .
    PERFORM log_budget  TABLES ptext USING '1' '80' '0' 1 mtext .
  ENDIF.

  SELECT * FROM /dsl/hr80_tvergd INTO TABLE gt_tvergd WHERE molga EQ p_molga AND grpid EQ p_grpid AND vrsid EQ p_vrsid.
  SELECT * FROM /dsl/hr80_tvergi INTO TABLE gt_tvergi WHERE molga EQ p_molga AND grpid EQ p_grpid AND vrsid EQ p_vrsid.


ENDFORM.
*---------------------------------------------------------------------*
*       FORM LOG_BUDGET
*---------------------------------------------------------------------*
FORM log_budget  TABLES ptext STRUCTURE plog_text
                            USING $level $lengt $inten $eline &text1.
  ptext-tlevel      = $level.
  ptext-text1       = &text1.
  ptext-tlength1    = $lengt.
  ptext-tintensiv1  = $inten.
  ptext-empty_lines = $eline.
  APPEND ptext.
ENDFORM.                                                    "log_0769
