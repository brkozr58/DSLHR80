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

  CHECK p_grpid IS NOT INITIAL AND p_vrsid IS NOT INITIAL .
  " buraya girerse bütçe çalışıyor demektir.

  CASE as-parm1.
    WHEN '01'. "Bütçe Pers. Verileri
      PERFORM refresh_tables .
      PERFORM get_budget_datas.

    WHEN '02'. " Personel vr. düzenle
      "/dsl/hr80_t010 tablosundan personel için güncel verileri alınacak
      PERFORM change_person_data.

    WHEN '03'. " Bütçe Oranları
      PERFORM change_ratio .

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
**********************************************************************
***!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**********************************************************************




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
*&---------------------------------------------------------------------*
*& Form CHANGE_PERSON_DATA
*&---------------------------------------------------------------------*
FORM change_person_data .
  DATA : lr_rfper TYPE RANGE OF /dsl/hr80_t010-rfper WITH HEADER LINE.
  DATA : lr_pernr TYPE RANGE OF /dsl/hr80_t010-pernr WITH HEADER LINE.

  DEFINE change_value .
    IF &1 IS NOT INITIAL .
      &2 = &1.
    ENDIF.
  END-OF-DEFINITION.

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

*  REFRESH : p0000,p0001.
  CLEAR : p0000,p0001.
  DATA : lt_p0000 TYPE TABLE OF p0000 WITH HEADER LINE .
  DATA : lt_p0001 TYPE TABLE OF p0001 WITH HEADER LINE .
  SORT gt_t010 ASCENDING BY begda endda.
  LOOP AT p0000.
    LOOP AT gt_t010 INTO DATA(ls_t010)
          WHERE pernr EQ p0000-pernr
            AND begda LE p0000-endda
            AND endda GE p0000-begda.
      IF ls_t010-begda GT p0000-begda AND
         ls_t010-massn EQ p0000-massn AND
         ls_t010-massg EQ p0000-massg AND
         ls_t010-endda LT p0000-endda  .
        " bütçede işlemler dizisi değişikliği varmı
        LOOP AT gt_t010 TRANSPORTING NO FIELDS
          WHERE pernr EQ p0000-pernr
            AND begda LE p0000-endda
            AND endda GE p0000-begda
            AND massn NE p0000-massn
            AND massg NE p0000-massg.
        ENDLOOP.
        IF sy-subrc EQ 0. " varsa pa0 dakini bütçe verisi ile sınırla
          MOVE-CORRESPONDING p0000 TO lt_p0000.
          lt_p0000-endda = ls_t010-endda.
          COLLECT lt_p0000.
        ELSE. " yoksa pa0000 dan al
          MOVE-CORRESPONDING p0000 TO lt_p0000.
          COLLECT lt_p0000.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING p0000 TO lt_p0000.
        MOVE-CORRESPONDING ls_t010 TO lt_p0000.
        IF ls_t010-rfper IS NOT INITIAL ." DUMMY personellerde referans personel numarasnı al
          lt_p0000-pernr = ls_t010-rfper.
        ENDIF.
        COLLECT lt_p0000.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0 ." Bütçe verisine denk gelmeyen eski işlemler dizisini aynen al
      MOVE-CORRESPONDING p0000 TO lt_p0000.
      COLLECT lt_p0000.
    ENDIF.
  ENDLOOP.

  LOOP AT p0001.
    LOOP AT gt_t010 INTO ls_t010
          WHERE pernr EQ p0001-pernr
            AND begda LE p0001-endda
            AND endda GE p0001-begda.
      " bütçe işlemler dizisini ekle
      MOVE-CORRESPONDING p0001 TO lt_p0001.
      MOVE-CORRESPONDING ls_t010 TO lt_p0001.
      IF ls_t010-rfper IS NOT INITIAL . " DUMMY personellerde referans personel numarasnı al
        lt_p0001-pernr = ls_t010-rfper.
      ENDIF.
      change_value : ls_t010-abkrs     lt_p0001-abkrs,
                     ls_t010-werks_new lt_p0001-werks,
                     ls_t010-btrtl_new lt_p0001-btrtl,
                     ls_t010-persg_new lt_p0001-persg,
                     ls_t010-persk_new lt_p0001-persk,
                     ls_t010-orgeh     lt_p0001-orgeh,
                     ls_t010-plans     lt_p0001-plans,
                     ls_t010-stell     lt_p0001-stell,
                     ls_t010-kokrs     lt_p0001-kokrs,
                     ls_t010-kostl     lt_p0001-kostl.
      COLLECT lt_p0001.
    ENDLOOP.
    IF sy-subrc NE 0 .
      MOVE-CORRESPONDING p0001 TO lt_p0001.
      COLLECT lt_p0001.
    ENDIF.
  ENDLOOP.

  p0000[] = lt_p0000[].
  p0001[] = lt_p0001[].

  SORT p0000 ASCENDING BY begda endda.
  SORT p0001 ASCENDING BY begda endda.

  LOOP AT p0008 ASSIGNING FIELD-SYMBOL(<p0008>)
    WHERE begda LE aper-endda.
    LOOP AT gt_t010 INTO ls_t010  .
      change_value : ls_t010-trfar_new <p0008>-trfar,
                     ls_t010-trfgb_new <p0008>-trfgb,
                     ls_t010-trfgr_new <p0008>-trfgr,
                     ls_t010-trfst_new <p0008>-trfst,
                     ls_t010-lga01_new <p0008>-lga01,
                     ls_t010-salry_new <p0008>-bet01 .
    ENDLOOP.
  ENDLOOP.


" DUMMY personellerde PA daki sicil gönderilmeli
*p_rfper

  DATA : lv_infty(7).
  FIELD-SYMBOLS <infty> TYPE any .
  FIELD-SYMBOLS <infty_tab> TYPE ANY TABLE.

  " Ek ödemeleri bilgi tiplerine aktar.
  LOOP AT gt_t005 WHERE pernr IN lr_pernr[].
    LOOP AT gt_t512z WHERE lgart = gt_t005-lgart .
      lv_infty = 'P' && gt_t512z-infty.
      ASSIGN (lv_infty) TO <infty>.
      lv_infty = lv_infty && '[]'.
      ASSIGN (lv_infty) TO <infty_tab>.
      CHECK <infty> IS ASSIGNED AND <infty_tab> IS ASSIGNED  .


      UNASSIGN <infty> .
      UNASSIGN <infty_tab> .
    ENDLOOP.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_TABLES
*&---------------------------------------------------------------------*
FORM refresh_tables .
  REFRESH : gt_t001,gt_t002,gt_t003,gt_t004,
            gt_t005,gt_t007,gt_t010,gt_t011,
            gt_tvergd,gt_tvergi,gt_t512z .
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

  SELECT * FROM t512z INTO TABLE gt_t512z
      WHERE molga EQ p_molga.

  SELECT * FROM /dsl/hr80_t001   INTO TABLE gt_t001
        WHERE molga EQ p_molga
          AND grpid EQ p_grpid.
  SELECT * FROM /dsl/hr80_t002   INTO TABLE gt_t002
        WHERE molga EQ p_molga
          AND grpid EQ p_grpid.
  SELECT * FROM /dsl/hr80_t003   INTO TABLE gt_t003
        WHERE molga EQ p_molga
          AND grpid EQ p_grpid
          AND vrsid EQ p_vrsid
          AND statu EQ p_statu.
  IF sy-subrc NE 0 OR p_statu NE '2'. "
    MESSAGE ID '/DSL/HR80' TYPE 'E' NUMBER '043'
        INTO DATA(mtext)
        WITH p_vrsid  .
    PERFORM log_budget  TABLES ptext USING '1' '80' '0' 1 mtext .
    PERFORM errors TABLES error_ptext.
  ENDIF.

  SELECT * FROM /dsl/hr80_t004   INTO TABLE gt_t004
        WHERE molga EQ p_molga
          AND grpid EQ p_grpid
          AND vrsid EQ p_vrsid.
  IF sy-subrc NE 0  .
    MESSAGE ID '/DSL/HR80' TYPE 'E' NUMBER '044'
        INTO mtext
        WITH p_vrsid  .
    PERFORM log_budget  TABLES ptext USING '1' '80' '0' 1 mtext .
    PERFORM errors TABLES error_ptext.
  ENDIF.

  SELECT * FROM /dsl/hr80_t005   INTO TABLE gt_t005
        WHERE molga EQ p_molga
          AND grpid EQ p_grpid
          AND vrsid EQ p_vrsid
          AND pernr IN lr_pernr[].
  SELECT * FROM /dsl/hr80_t007   INTO TABLE gt_t007
        WHERE molga EQ p_molga
          AND grpid EQ p_grpid
          AND vrsid EQ p_vrsid.
  SELECT * FROM /dsl/hr80_t010   INTO TABLE gt_t010
        WHERE molga EQ p_molga
          AND grpid EQ p_grpid
          AND vrsid EQ p_vrsid
          AND pernr IN lr_pernr[]
          AND rfper IN lr_rfper[].
  IF sy-subrc NE 0  .
    MESSAGE ID '/DSL/HR80' TYPE 'E' NUMBER '037'
        INTO mtext   .
    PERFORM log_budget  TABLES ptext USING '1' '80' '0' 1 mtext .
    PERFORM errors TABLES error_ptext.
  ENDIF.

  SELECT * FROM /dsl/hr80_tvergd INTO TABLE gt_tvergd
        WHERE molga EQ p_molga
          AND grpid EQ p_grpid
          AND vrsid EQ p_vrsid.
  SELECT * FROM /dsl/hr80_tvergi INTO TABLE gt_tvergi
        WHERE molga EQ p_molga
          AND grpid EQ p_grpid
          AND vrsid EQ p_vrsid.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_RATIO
*&---------------------------------------------------------------------*
FORM change_ratio .
  DATA : lv_field(30).
  DATA : lr_bukrs  TYPE RANGE OF bukrs.
  DATA : lr_kostl  TYPE RANGE OF kostl.
  DATA : lr_ansvh  TYPE RANGE OF ansvh.
  DATA : lr_werks  TYPE RANGE OF persa.
  DATA : lr_btrtl  TYPE RANGE OF btrtl.
  DATA : lr_persg  TYPE RANGE OF persg.
  DATA : lr_persk  TYPE RANGE OF persk.
  DATA : lr_orgeh  TYPE RANGE OF orgeh.
  DATA : lr_stell  TYPE RANGE OF stell.
  DATA : lr_abkrs  TYPE RANGE OF abkrs.
  DATA : lr_pernr  TYPE RANGE OF persno.
  DATA : lt_param  TYPE TABLE OF /dsl/hr80_t004    WITH HEADER LINE.


  DATA : lr_rfper TYPE RANGE OF /dsl/hr80_t010-rfper WITH HEADER LINE.

" PA personelleri için pernr-pernr yi al. seçim ekranında ki zli eklenen
" P_PERNR  parametresini boş göndermelisin
  IF p_pernr IS NOT INITIAL ." DUMMY
    lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = p_pernr  )
                        ( sign = 'I' option = 'EQ' low = space  ) ).
  ELSE.
    lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = pernr-pernr  )
                        ( sign = 'I' option = 'EQ' low = space  ) ).
  ENDIF.

" DUMMY personellerde PA daki sicil gönderilmeli
  IF p_rfper IS NOT INITIAL .
    lr_rfper = 'IEQ'.lr_rfper-low = p_rfper.APPEND lr_rfper.
  ENDIF.


  LOOP AT wpbp.
    lt_param[] = gt_t004[].


    " personel numarasına özel kayıt girildiyse aynı lgartta boş olanları sil
    SORT lt_param BY pernr lgart .

    LOOP AT lt_param INTO DATA(ls_param) .
      IF lt_param-pernr IS NOT INITIAL .
        READ TABLE lt_param TRANSPORTING NO FIELDS
                       WITH KEY pernr = space
                                lgart = ls_param-lgart .
        IF sy-subrc EQ 0 .
          DELETE lt_param WHERE pernr = space
                            AND lgart = ls_param-lgart .
        ENDIF .
      ENDIF .
    ENDLOOP .

    lr_bukrs = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-bukrs )
                        ( sign = 'I' option = 'EQ' low = space ) ).
    lr_kostl = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-kostl )
                        ( sign = 'I' option = 'EQ' low = space ) ).
    lr_ansvh = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-ansvh )
                        ( sign = 'I' option = 'EQ' low = space ) ).
    lr_werks = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-werks )
                        ( sign = 'I' option = 'EQ' low = space ) ).
    lr_btrtl = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-btrtl )
                        ( sign = 'I' option = 'EQ' low = space ) ).
    lr_persg = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-persg )
                        ( sign = 'I' option = 'EQ' low = space ) ).
    lr_persk = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-persk )
                        ( sign = 'I' option = 'EQ' low = space ) ).
    lr_orgeh = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-orgeh )
                        ( sign = 'I' option = 'EQ' low = space ) ).
    lr_stell = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-stell )
                        ( sign = 'I' option = 'EQ' low = space ) ).
*    lr_abkrs = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-ansvh )
*                        ( sign = 'I' option = 'EQ' low = space ) ).
*    lr_pernr = VALUE #( ( sign = 'I' option = 'EQ' low = wpbp-bukrs )
*                        ( sign = 'I' option = 'EQ' low = space ) ).


    LOOP AT lt_param INTO ls_param
        WHERE bukrs IN lr_bukrs[]
          AND kostl IN lr_kostl[]
          AND ansvh IN lr_ansvh[]
          AND werks IN lr_werks[]
          AND btrtl IN lr_btrtl[]
          AND persg IN lr_persg[]
          AND persk IN lr_persk[]
          AND orgeh IN lr_orgeh[]
          AND stell IN lr_stell[]
          AND abkrs IN lr_abkrs[]
          AND pernr IN lr_pernr[].

      READ TABLE it ASSIGNING FIELD-SYMBOL(<it>)
            WITH KEY lgart = ls_param-lgart
                     apznr = wpbp-apznr.
      IF sy-subrc EQ 0 .
        lv_field = 'RAT' && aper-begda+4(2).
        ASSIGN COMPONENT lv_field OF STRUCTURE ls_param TO
                FIELD-SYMBOL(<rat>).
        lv_field = 'ANZ' && aper-begda+4(2).
        ASSIGN COMPONENT lv_field OF STRUCTURE ls_param TO
                FIELD-SYMBOL(<anz>).
        lv_field = 'BET' && aper-begda+4(2).
        ASSIGN COMPONENT lv_field OF STRUCTURE ls_param TO
                FIELD-SYMBOL(<bet>).

        IF <rat> IS ASSIGNED AND <rat> IS NOT INITIAL .
          IF <it>-betpe IS NOT INITIAL.
            <it>-betpe = <it>-betpe + ( ( <it>-betpe * <rat> ) / 100 ).
          ENDIF.
        ENDIF.

        IF <anz> IS ASSIGNED AND <anz> IS NOT INITIAL .
          IF <it>-anzhl IS NOT INITIAL.
            <it>-anzhl = <it>-anzhl + ( ( <it>-anzhl * <anz> ) / 100 ).
          ENDIF.
        ENDIF.

        IF <bet> IS ASSIGNED AND <bet> IS NOT INITIAL .
          IF <it>-betrg IS NOT INITIAL.
            <it>-betrg = <it>-betrg + ( ( <it>-betrg * <bet> ) / 100 ).
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.
  ENDLOOP.

  DATA : ls_it LIKE LINE OF it .

  LOOP AT gt_t005 WHERE pernr IN lr_pernr[]
                    AND begda LE aper-endda
                    AND endda GE aper-begda.
    ls_it-abart = wpbp-abart.
*    ls_it-apznr = wpbp-apznr.
    ls_it-lgart = gt_t005-lgart.
    ls_it-anzhl = gt_t005-anzhl.
    ls_it-betrg = gt_t005-betrg.
    APPEND ls_it TO it.
    CLEAR ls_it .

  ENDLOOP.



ENDFORM.
