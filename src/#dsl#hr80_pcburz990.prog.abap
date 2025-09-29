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

    WHEN '03'. " Temel, Ek ve zaman
      PERFORM basic_additional_time_data.

    WHEN '04'. " Bütçe Oranları
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
  APPEND ptext.CLEAR ptext .
  CLEAR &text1 .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_TABLES
*&---------------------------------------------------------------------*
FORM refresh_tables .
  REFRESH : gt_t001,gt_t002,gt_t003,gt_t004,
            gt_t005,gt_t007,gt_t010,gt_t011,
            gt_tvergd,gt_tvergi,gt_t005_subty.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BUDGET_DATAS
*&---------------------------------------------------------------------*
FORM get_budget_datas .
  DATA : lr_rfper TYPE RANGE OF /dsl/hr80_t010-rfper WITH HEADER LINE.
  DATA : lr_pernr TYPE RANGE OF /dsl/hr80_t010-pernr WITH HEADER LINE.
  DATA : lr_moabw TYPE RANGE OF t001p-moabw WITH HEADER LINE.
  DATA : lr_infty TYPE RANGE OF t512z-infty WITH HEADER LINE.
  DATA : lt_t001p TYPE TABLE OF t001p WITH HEADER LINE .


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

  APPEND VALUE #( sign = 'I' option = 'EQ'  low = '0014' ) TO lr_infty.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = '0015' ) TO lr_infty.
  APPEND VALUE #( sign = 'I' option = 'EQ'  low = '2010' ) TO lr_infty.

  SELECT t1~molga
         t2~sprsl
         t1~infty
         t1~lgart
         t2~lgtxt AS lgart_t
         t1~begda
         t1~endda
        FROM t512z AS t1
        INNER JOIN t512t AS t2
            ON    t2~molga EQ t1~molga
              AND t2~lgart EQ t1~lgart
      INTO CORRESPONDING FIELDS OF TABLE gt_t005_subty
      WHERE t1~molga EQ p_molga
        AND t2~sprsl EQ sy-langu
        AND t1~infty IN lr_infty[]
        AND t1~begda LE sy-datum
        AND t1~endda GE sy-datum.
  SORT gt_t005_subty ASCENDING BY infty lgart .
  DELETE ADJACENT DUPLICATES FROM gt_t005_subty COMPARING infty lgart.

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
  IF sy-subrc NE 0 OR ( p_statu NE '2'   ). "
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
  ELSE.
    SELECT * FROM t001p INTO TABLE lt_t001p
        FOR ALL ENTRIES IN gt_t010
        WHERE molga EQ p_molga
          AND ( ( werks EQ gt_t010-werks AND
                  btrtl EQ gt_t010-btrtl )
                OR
                ( werks EQ gt_t010-werks_new AND
                  btrtl EQ gt_t010-btrtl_new )
               ) .
    SORT lt_t001p ASCENDING .
    DELETE ADJACENT DUPLICATES FROM lt_t001p.
    lr_moabw[] = VALUE #( FOR ls_t001p IN lt_t001p
              WHERE ( molga EQ p_molga )
                    ( sign    = 'I'
                      option  = 'EQ'
                      low     = ls_t001p-moabw
                      high    = ls_t001p-moabw ) ).

    SELECT
          FROM t554s AS t1
          INNER JOIN t554t AS t2
              ON    t2~awart EQ t1~subty
      FIELDS
           t2~sprsl,
            t1~subty AS lgart     ,
           t2~atext AS lgart_t   ,
           CAST( '2001' AS CHAR( 4 ) )  AS infty,
           t1~begda              ,
           t1~endda
        WHERE t1~moabw IN @lr_moabw
          AND t2~sprsl EQ @sy-langu
          AND t1~begda LE @sy-datum
          AND t1~endda GE @sy-datum
        APPENDING CORRESPONDING FIELDS OF TABLE @gt_t005_subty.

    SORT gt_t005_subty ASCENDING BY infty lgart .
    DELETE ADJACENT DUPLICATES FROM gt_t005_subty COMPARING infty lgart.

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
*& Form CHANGE_PERSON_DATA
*&---------------------------------------------------------------------*
FORM change_person_data .
  DATA : lr_rfper TYPE RANGE OF /dsl/hr80_t010-rfper WITH HEADER LINE.
  DATA : lr_pernr TYPE RANGE OF /dsl/hr80_t010-pernr WITH HEADER LINE.
  DATA   lr_lgart TYPE RANGE OF lgart WITH HEADER LINE .

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
          WHERE begda LE p0000-endda
            AND endda GE p0000-begda
*            AND  pernr EQ p0000-pernr
             .
      IF ls_t010-begda GT p0000-begda AND
         ls_t010-massn EQ p0000-massn AND
         ls_t010-massg EQ p0000-massg AND
         ls_t010-endda LT p0000-endda  .
        " bütçede işlemler dizisi değişikliği varmı
        LOOP AT gt_t010 TRANSPORTING NO FIELDS
          WHERE begda LE p0000-endda
            AND endda GE p0000-begda
            AND massn NE p0000-massn
            AND massg NE p0000-massg
            AND pernr IN lr_pernr[]
*            AND  pernr EQ p0000-pernr
          .
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
          WHERE begda LE p0001-endda
            AND endda GE p0001-begda
            AND  pernr IN lr_pernr[]
*            AND  pernr EQ p0000-pernr
      .
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

*  LOOP AT p0008 ASSIGNING FIELD-SYMBOL(<p0008>)
*    WHERE begda LE aper-endda.
*    LOOP AT gt_t010 INTO ls_t010  WHERE pernr IN lr_pernr[].
*      change_value : ls_t010-trfar_new <p0008>-trfar,
*                     ls_t010-trfgb_new <p0008>-trfgb,
*                     ls_t010-trfgr_new <p0008>-trfgr,
*                     ls_t010-trfst_new <p0008>-trfst,
*                     ls_t010-lga01_new <p0008>-lga01,
*                     ls_t010-salry_new <p0008>-bet01 .
*    ENDLOOP.
*  ENDLOOP.


*  LOOP AT wpbp.
    SELECT 'I' AS option , 'EQ' AS sing , lgart AS low
        FROM t512z INTO TABLE @lr_lgart
                           WHERE infty EQ '0008'
                             AND molga EQ @p_molga
                             AND begda LE @aper-endda
                             AND endda GE @aper-endda.

    LOOP AT p0008 ASSIGNING FIELD-SYMBOL(<p0008>)
      WHERE begda LE aper-endda.
      LOOP AT gt_t010 INTO ls_t010
              WHERE pernr IN lr_pernr[].
        change_value : ls_t010-trfar_new <p0008>-trfar,
                       ls_t010-trfgb_new <p0008>-trfgb,
                       ls_t010-trfgr_new <p0008>-trfgr,
                       ls_t010-trfst_new <p0008>-trfst.
        IF ls_t010-lga01_new IS NOT INITIAL AND
           ls_t010-salry_new IS NOT INITIAL .
          change_value : ls_t010-salry_new <p0008>-bet01,
                         ls_t010-lga01_new <p0008>-lga01 .
        ELSE.
          IF ls_t010-lga01_new IS NOT INITIAL .
            LOOP AT gt_t007 INTO DATA(ls_t007)
                    WHERE molga EQ p_molga
                      AND ( ( trfar EQ ls_t010-trfar AND
                              trfgb EQ ls_t010-trfgb AND
*                              trfkz EQ ls_t010-abart AND
                              trfgr EQ ls_t010-trfgr AND
                              trfst EQ ls_t010-trfst )
                        OR
                          ( trfar EQ ls_t010-trfar_new AND
                            trfgb EQ ls_t010-trfgb_new     AND
*                            trfkz EQ ls_t010-abart     AND
                            trfgr EQ ls_t010-trfgr_new     AND
                            trfst EQ ls_t010-trfst_new )
                        )
                      AND lgart EQ ls_t010-lga01_new
                      AND begda LE aper-endda
                      AND endda GE aper-begda.
            ENDLOOP.
            IF sy-subrc EQ 0 AND ls_t007-betrg IS INITIAL .
              change_value : ls_t010-salry_new <p0008>-bet01,
                             ls_t007-betrg     <p0008>-lga01 .
            ENDIF.
          ELSE.
            LOOP AT gt_t007 INTO ls_t007
                    WHERE molga EQ p_molga
                      AND ( ( trfar EQ ls_t010-trfar AND
                              trfgb EQ ls_t010-trfgb AND
*                              trfkz EQ ls_t010-abart AND
                              trfgr EQ ls_t010-trfgr AND
                              trfst EQ ls_t010-trfst )
                        OR
                          ( trfar EQ ls_t010-trfar_new AND
                            trfgb EQ ls_t010-trfgb_new     AND
*                            trfkz EQ ls_t010-abart     AND
                            trfgr EQ ls_t010-trfgr_new     AND
                            trfst EQ ls_t010-trfst_new )
                        )
                      AND lgart EQ ls_t010-lga01
                      AND begda LE aper-endda
                      AND endda GE aper-begda.
            ENDLOOP.
            IF sy-subrc EQ 0 AND ls_t007-betrg IS INITIAL .
              change_value : ls_t010-lga01     <p0008>-bet01,
                             ls_t007-betrg     <p0008>-lga01 .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BASIC_ADDITIONAL_TIME_DATA
*&---------------------------------------------------------------------*
FORM basic_additional_time_data.

  DATA : lr_rfper TYPE RANGE OF /dsl/hr80_t010-rfper WITH HEADER LINE.
  DATA : lr_pernr TYPE RANGE OF /dsl/hr80_t010-pernr WITH HEADER LINE.
  DATA : lt_p0014 TYPE TABLE OF p0014,
         lt_p0015 TYPE TABLE OF p0015,
         lt_p2010 TYPE TABLE OF p2010,
         lt_p2001 TYPE TABLE OF p2001.
  DATA   lr_lgart TYPE RANGE OF lgart WITH HEADER LINE .
  DATA : lv_pernr TYPE persno .


" PA personelleri için pernr-pernr yi al. seçim ekranında ki zli eklenen
" P_PERNR  parametresini boş göndermelisin
  IF p_pernr IS NOT INITIAL .
    lr_pernr = 'IEQ'.lr_pernr-low = p_pernr.APPEND lr_pernr.
    lv_pernr =  p_pernr.
  ELSE.
    lr_pernr = 'IEQ'.lr_pernr-low = pernr-pernr.APPEND lr_pernr.
    lv_pernr = pernr-pernr.
  ENDIF.

" DUMMY personellerde PA daki sicil gönderilmeli
  IF p_rfper IS NOT INITIAL .
    lr_rfper = 'IEQ'.lr_rfper-low = p_rfper.APPEND lr_rfper.
  ENDIF.

  IF p_rfper IS NOT INITIAL .
    LOOP AT gt_t003 WHERE grpid EQ p_grpid AND vrsid EQ p_vrsid.ENDLOOP.
    DELETE p0014 WHERE begda LE gt_t003-endda AND endda GE gt_t003-begda.
    DELETE p0015 WHERE begda LE gt_t003-endda AND endda GE gt_t003-begda.
    DELETE p2010 WHERE begda LE gt_t003-endda AND endda GE gt_t003-begda.
    DELETE p2001 WHERE begda LE gt_t003-endda AND endda GE gt_t003-begda.
  ENDIF.


  DELETE gt_t005 WHERE NOT ( begda LE aper-endda AND
                             endda GE aper-begda ).
  IF gt_t005[] IS NOT INITIAL .

  " DUMMY personellerde PA daki sicil gönderilmeli
*  p_rfper
    " Ek ödemeleri bilgi tiplerine aktar.
    REFRESH lr_lgart.
    lr_lgart[] = VALUE #( FOR ls_tsub IN gt_t005_subty
                WHERE ( infty EQ '0014' )
                      ( sign    = 'I'
                        option  = 'EQ'
                        low     = ls_tsub-lgart
                        high    = ls_tsub-lgart ) ).
    IF lr_lgart[] IS NOT INITIAL .
      lt_p0014[] = VALUE #( FOR ls_t5 IN gt_t005
                  WHERE ( pernr IN lr_pernr[] AND
                          lgart IN lr_lgart[] )
                        ( pernr = lv_pernr
                          infty = '0014'
                          subty = ls_t5-lgart
                          lgart = ls_t5-lgart
                          begda = ls_t5-begda
                          endda = ls_t5-endda
                          betrg = ls_t5-betrg
                          anzhl = ls_t5-anzhl
                          waers = p0008-waers
                           )  ) .
      APPEND LINES OF lt_p0014[] TO p0014.
      REFRESH lt_p0014.
    ENDIF.

    REFRESH lr_lgart.
    lr_lgart[] = VALUE #( FOR ls_tsub IN gt_t005_subty
                WHERE ( infty EQ '0015' )
                      ( sign    = 'I'
                        option  = 'EQ'
                        low     = ls_tsub-lgart
                        high    = ls_tsub-lgart ) ).
    IF lr_lgart[] IS NOT INITIAL .
      lt_p0015[] = VALUE #( FOR ls_t5 IN gt_t005
                  WHERE ( pernr IN lr_pernr[] AND
                          lgart IN lr_lgart[] )
                        ( pernr = lv_pernr
                          infty = '0015'
                          subty = ls_t5-lgart
                          lgart = ls_t5-lgart
                          begda = ls_t5-begda
                          endda = ls_t5-endda
                          betrg = ls_t5-betrg
                          anzhl = ls_t5-anzhl
                          waers = p0008-waers
                           )  ) .
      APPEND LINES OF lt_p0015[] TO p0015.
      REFRESH lt_p0015.
    ENDIF.

    REFRESH lr_lgart.
    lr_lgart[] = VALUE #( FOR ls_tsub IN gt_t005_subty
                WHERE ( infty EQ '2001' )
                      ( sign    = 'I'
                        option  = 'EQ'
                        low     = ls_tsub-lgart
                        high    = ls_tsub-lgart ) ).
    IF lr_lgart[] IS NOT INITIAL .
      lt_p2001[] = VALUE #( FOR ls_t5 IN gt_t005
                  WHERE ( pernr IN lr_pernr[] AND
                          lgart IN lr_lgart[] )
                        ( pernr = lv_pernr
                          infty = '2001'
                          subty = ls_t5-lgart
                          awart = ls_t5-lgart
                          begda = ls_t5-begda
                          endda = ls_t5-endda
                          kaltg = ls_t5-anzhl
                           )  ) .
      APPEND LINES OF lt_p2001[] TO p2001.
      REFRESH lt_p2001.
    ENDIF.


    REFRESH lr_lgart.
    lr_lgart[] = VALUE #( FOR ls_tsub IN gt_t005_subty
                WHERE ( infty EQ '2010' )
                      ( sign    = 'I'
                        option  = 'EQ'
                        low     = ls_tsub-lgart
                        high    = ls_tsub-lgart ) ).
    IF lr_lgart[] IS NOT INITIAL .
      lt_p2010[] = VALUE #( FOR ls_t5 IN gt_t005
                  WHERE ( pernr IN lr_pernr[] AND
                          lgart IN lr_lgart[] )
                        ( pernr = lv_pernr
                          infty = '2010'
                          subty = ls_t5-lgart
                          lgart = ls_t5-lgart
                          begda = ls_t5-begda
                          endda = ls_t5-endda
                          betrg = ls_t5-betrg
                          anzhl = ls_t5-anzhl
                          waers = p0008-waers
                           )  ) .
      APPEND LINES OF lt_p2010[] TO p2010.
      REFRESH lt_p2010.
    ENDIF.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_RATIO
*&---------------------------------------------------------------------*
FORM change_ratio .
  FIELD-SYMBOLS <lfs> TYPE any .
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
  DATA : ls_param  TYPE /dsl/hr80_t004.
  DATA : lt_t512t TYPE TABLE OF t512t WITH HEADER LINE .
  DATA : lv_mtext TYPE text132.
  DATA : lv_oran(10).
  DATA : ls_temp LIKE LINE OF it .
  DATA : lr_rfper TYPE RANGE OF /dsl/hr80_t010-rfper WITH HEADER LINE.
  DATA : betpe,
         anzhl,
         betrg.


  DEFINE calc_ratio .
    lv_field = &1 && aper-begda+4(2).
    ASSIGN COMPONENT lv_field OF STRUCTURE ls_param TO <lfs>.
    IF <lfs> IS ASSIGNED .
      IF <lfs> IS NOT INITIAL .
        IF &3-&2 IS NOT INITIAL.
          &3-&2 = &3-&2 + ( ( &3-&2 * <lfs> ) / 100 ).

"<<--------log------>>
          WRITE <lfs> TO lv_oran .
          SHIFT lv_oran LEFT DELETING LEADING space.
*          lv_mtext+30(10) = &4 && ' %' && lv_oran.
          lv_mtext+30(10) = &4.
          lv_mtext+30(10) = '%' && lv_oran.
          WRITE &5 TO lv_mtext+35(21).
*          SHIFT lv_mtext+35(21) LEFT DELETING LEADING space.
          WRITE &3-&2 TO lv_mtext+58(21).
*          SHIFT lv_mtext+58(21) LEFT DELETING LEADING space.
          PERFORM log_budget  TABLES ptext USING '1' '90' '0' 0 lv_mtext .
"<<-------- ------>>


        ENDIF.
      ENDIF.
    ENDIF.
    UNASSIGN <lfs>.
  END-OF-DEFINITION.

  SELECT * FROM t512t INTO TABLE lt_t512t
      FOR ALL ENTRIES IN it
        WHERE molga EQ p_molga
          AND sprsl EQ sy-langu
          AND lgart EQ it-lgart.


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
    lt_param[] = gt_t004[]. " Oran tablosu aktarımı
"<<--------İşleme günlüğü
    lv_mtext = 'Dönem:       '.
    WRITE aper-begda TO lv_mtext+14(10).
    WRITE aper-endda TO lv_mtext+27(10).
    PERFORM log_budget  TABLES ptext USING '1' '90' '0' 1 lv_mtext .

    lv_mtext = 'Ücret türü'.
    lv_mtext+30(10) = 'Oran'.
    lv_mtext+45(21) = 'Eski değer'.
    lv_mtext+68(21) = 'Yeni değer'.
    PERFORM log_budget  TABLES ptext USING '1' '90' '0' 0 lv_mtext .
"<<-------------->>
    " personel numarasına özel kayıt girildiyse aynı lgartta boş olanları sil
    SORT lt_param BY pernr lgart .
    LOOP AT lt_param INTO ls_param .
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
        MOVE-CORRESPONDING <it> TO ls_temp. " işleme için gerekli
        READ TABLE lt_t512t WITH KEY lgart = <it>-lgart.
        CLEAR lv_mtext.
        lv_mtext = <it>-lgart && '-' && lt_t512t-lgtxt.
        calc_ratio : 'RAT' betpe <it> 'BETPE' ls_temp-betpe,
                     'ANZ' anzhl <it> 'ANZHL' ls_temp-anzhl,
                     'BET' betrg <it> 'BETRG' ls_temp-betrg.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
