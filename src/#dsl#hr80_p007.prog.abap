*&---------------------------------------------------------------------*
*& Report /DSL/HR80_P007
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /dsl/hr80_p007.

TABLES : /dsl/hr80_t001,
         /dsl/hr80_t003.


SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-000  .
  PARAMETERS : p_molga TYPE /dsl/hr80_t001-molga
                      OBLIGATORY
                      DEFAULT '47'.
SELECT-OPTIONS : s_grpid FOR /dsl/hr80_t003-grpid
                      NO INTERVALS
                      NO-EXTENSION
                      MATCHCODE OBJECT /dsl/hr80_grpid
                      OBLIGATORY  ,

                s_vrsid FOR /dsl/hr80_t003-vrsid
                      NO INTERVALS
                      NO-EXTENSION
                      MATCHCODE OBJECT /dsl/hr80_vrsid
                      OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl1.

*SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-t00 .
*
*PARAMETERS :
**             p_datum TYPE datum OBLIGATORY,
*             p_abkrs TYPE t569v-abkrs OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_relas RADIOBUTTON GROUP rb1 DEFAULT 'X',
            p_corre RADIOBUTTON GROUP rb1,
            p_exit RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK bl3.

START-OF-SELECTION .
  PERFORM get_data .




*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM get_data .

  DATA : lr_abkrs            TYPE RANGE OF abkrs WITH HEADER LINE  .
  DATA : lt_t596v TYPE TABLE OF t569v,
         ls_linev TYPE  t569v,
         lv_datum TYPE datum,
         lt_t569u TYPE TABLE OF t569u,
         ls_t569u TYPE t569u,
        lv_statu TYPE text25.
  DATA : ls_t003 TYPE /dsl/hr80_t003.

  CLEAR: lt_t596v, ls_linev, lv_datum.

  CLEAR :ls_t003.
  SELECT SINGLE * FROM /dsl/hr80_t003 INTO ls_t003
    WHERE grpid IN s_grpid[]
      AND vrsid IN s_vrsid[] .


  SELECT
      abkrs
    FROM /dsl/hr80_ddl001 INTO TABLE @DATA(lt_abkrs)
    WHERE grpid       IN @s_grpid[]
      AND vrsid       IN @s_vrsid[].
  SORT lt_abkrs ASCENDING .
  DELETE ADJACENT DUPLICATES FROM lt_abkrs.

  lr_abkrs[] = VALUE #( FOR ls_abkrs IN lt_abkrs
                ( option = 'EQ'
                  sign   = 'I'
                  low    = ls_abkrs )  ) .
  SORT lr_abkrs ASCENDING .
  DELETE ADJACENT DUPLICATES FROM lr_abkrs.

  SELECT * FROM t569v INTO TABLE lt_t596v
                      WHERE abkrs IN lr_abkrs[]  .

  LOOP AT lt_t596v INTO ls_linev.

    CASE 'X'.
      WHEN p_relas. ls_linev-state = '1'.
      WHEN p_corre. ls_linev-state = '2'.
      WHEN p_exit.  ls_linev-state = '3'.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = ls_t003-endda
            days      = 0
            months    = 1
            signum    = '+'
            years     = 0
          IMPORTING
            calc_date = lv_datum.
    ENDCASE.


    ls_linev-pabrp = ls_t003-endda+4(2).
    ls_linev-pabrj = ls_t003-endda+0(4).



    IF p_exit NE 'X'.
      ls_linev-uabrp = ls_t003-begda+4(2).
    ELSE.
      ls_linev-uabrp = lv_datum+4(2).
    ENDIF.
    ls_linev-uabrj = ls_t003-begda+0(4).

    MODIFY t569v FROM ls_linev.
    COMMIT WORK.
    IF sy-subrc EQ 0.
      CASE ls_linev-state.
        WHEN '1'."release.
          lv_statu = 'Canlı Bordro'.
        WHEN '2'."correct.
          lv_statu = 'Ana Veri Düzeltme'.
        WHEN '3'."exit.
          lv_statu = 'Bordrodan Çıkış'.
        WHEN OTHERS.
      ENDCASE.

      WRITE :/ ls_linev-abkrs , 'Bordro alt birimi için ->',
               'Bordro dönemi:',               ls_linev-pabrp ,'.', ls_linev-pabrj ,
               'Yeniden hspl.en erken dönem:', ls_linev-uabrp ,'.', ls_linev-uabrj ,
               ' ->' , lv_statu,
               'Statüsünde.'.
      REFRESH lt_t569u.
      SELECT  * FROM t569u  UP TO 1 ROWS
       INTO TABLE  lt_t569u
       WHERE abkrs EQ ls_linev-abkrs
        ORDER BY aedat ASCENDING uzeit ASCENDING.

      LOOP AT lt_t569u INTO ls_t569u.

        ls_t569u-abkrs = ls_linev-abkrs.
        ls_t569u-vwsaz = '1'.
        ls_t569u-uname = sy-uname.
        ls_t569u-pabrj = ls_linev-pabrj.
        ls_t569u-pabrp = ls_linev-pabrp.
        ls_t569u-state = ls_linev-state.
        CONVERT DATE sy-datum INTO INVERTED-DATE ls_t569u-aedat.
        CONVERT DATE sy-uzeit INTO INVERTED-DATE ls_t569u-uzeit.
        ls_t569u-srtfd = '1'.

        MODIFY t569u FROM ls_t569u.
        COMMIT WORK.

      ENDLOOP.

    ENDIF.

  ENDLOOP.

ENDFORM.
