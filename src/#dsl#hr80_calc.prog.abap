*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_CALC
*&---------------------------------------------------------------------*


TABLES :  /dsl/hr80_t001    ,"Bütçe - Grup tanımı
          /dsl/hr80_t002    ,"Bütçe - Yetki alanları
          /dsl/hr80_t003    ,"Bütçe - Versiyon yönetimi
          /dsl/hr80_t004    ,"Bütçe - Parametre tanımlamaları
          /dsl/hr80_t005    ,"Bütçe - Ek ödeme ücretleri
          /dsl/hr80_t007    ,"Bütçe - Ücret skalası grupları (T510)
          /dsl/hr80_t010    ,"Bütçe - Personel anaverileri
          /dsl/hr80_t011    ,"Bütçe - Bordro sonuçları
          /dsl/hr80_tvergd  ,"Bütçe - Vergi dilimleri (T7TRT01)
          /dsl/hr80_tvergi  ."Bütçe - Vergi indirimleri (T7TRT02)
  DATA : btn_bdg.

  " CALC programında bu parametrelere ulaşabilecek.
  "sadece submitte çalışması sağlanacaktır.
  " değiştirmeyiniz!!!!!!!!!!!
SELECTION-SCREEN BEGIN OF BLOCK bdgt  WITH FRAME .
  PARAMETERS p_molga TYPE /dsl/hr80_t011-molga  DEFAULT '47' MODIF ID bdg.
  PARAMETERS p_grpid TYPE /dsl/hr80_t011-grpid               MODIF ID bdg.
  PARAMETERS p_vrsid TYPE /dsl/hr80_t011-vrsid               MODIF ID bdg.
  PARAMETERS p_statu TYPE /dsl/hr80_t003-statu NO-DISPLAY DEFAULT '2'  MODIF ID bdg.
  PARAMETERS p_pernr TYPE /dsl/hr80_t010-pernr               MODIF ID bdg.
  PARAMETERS p_rfper TYPE /dsl/hr80_t010-rfper               MODIF ID bdg.
SELECTION-SCREEN END OF BLOCK bdgt.

SELECTION-SCREEN BEGIN OF BLOCK bdgt2  WITH FRAME .
  SELECTION-SCREEN PUSHBUTTON 1(42) btbdgt
                  USER-COMMAND bdg VISIBLE LENGTH 60.
SELECTION-SCREEN END OF BLOCK bdgt2.


  DATA :  gt_t001   TYPE TABLE OF /dsl/hr80_t001    WITH HEADER LINE, "Bütçe - Grup tanımı
          gt_t002   TYPE TABLE OF /dsl/hr80_t002    WITH HEADER LINE, "Bütçe - Yetki alanları
          gt_t003   TYPE TABLE OF /dsl/hr80_t003    WITH HEADER LINE, "Bütçe - Versiyon yönetimi
          gt_t004   TYPE TABLE OF /dsl/hr80_t004    WITH HEADER LINE, "Bütçe - Parametre tanımlamaları
          gt_t005   TYPE TABLE OF /dsl/hr80_t005    WITH HEADER LINE, "Bütçe - Ek ödeme ücretleri
          gt_t007   TYPE TABLE OF /dsl/hr80_t007    WITH HEADER LINE, "Bütçe - Ücret skalası grupları (T510)
          gt_t010   TYPE TABLE OF /dsl/hr80_t010    WITH HEADER LINE, "Bütçe - Personel anaverileri
          gt_t011   TYPE TABLE OF /dsl/hr80_t011    WITH HEADER LINE, "Bütçe - Bordro sonuçları
          gt_tvergd TYPE TABLE OF /dsl/hr80_tvergd  WITH HEADER LINE, "Bütçe - Vergi dilimleri (T7TRT01)
          gt_tvergi TYPE TABLE OF /dsl/hr80_tvergi  WITH HEADER LINE, "Bütçe - Vergi indirimleri (T7TRT02)
          gt_t512z  TYPE TABLE OF t512z             WITH HEADER LINE .






*&---------------------------------------------------------------------*
*& Form BDG_BTN_EVENT
*&---------------------------------------------------------------------*
FORM bdg_btn_event .
  CASE sscrfields-ucomm .
    WHEN 'BDG'.
      IF btn_bdg IS NOT INITIAL .
        CLEAR btn_bdg.
      ELSE.
        btn_bdg = 'X'.
        prt_prot = 'X'.
        tst_on = 'X'.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDG_SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM bdg_screen_modify .
  FIELD-SYMBOLS <lfs> TYPE any .

  DEFINE change_text.
    ASSIGN (screen-name) TO <lfs>.
    IF <lfs> IS ASSIGNED .
      <lfs> = &1 .
    ENDIF.
    UNASSIGN <lfs>.
  END-OF-DEFINITION.

  btbdgt = 'Bütçe parametreleri'.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'BDG'.

        CASE screen-name+2(7).
          WHEN 'P_MOLGA'.
            change_text : 'Ülke Gruplaması'.
          WHEN 'P_GRPID'.
            change_text : 'Bütçe Grubu'.
          WHEN 'P_VRSID'.
            change_text : 'Bütçe Versiyonu'.
          WHEN 'P_STATU'.
            change_text : 'Bütçe Durumu'.
          WHEN 'P_PERNR'.
            change_text : 'DUMMY Personel'.
          WHEN 'P_RFPER'.
            change_text : 'Referans Personel'.
        ENDCASE.
        IF btn_bdg EQ 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN .
  ENDLOOP.
ENDFORM.
