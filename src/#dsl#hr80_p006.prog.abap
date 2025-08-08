*&---------------------------------------------------------------------*
*& Report /DSL/HR80_P006
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&"ÖNEMLİ :
*&"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*&Simülasyon çalıştığında buffer dan bordro sonucu okumak için
*&aşağıdaki linkte T77s0 tablosuna
*&
*&
*&PBON = X: Activate Central PCLx Puffer
*&PBOFF = X: Deactivate Central PCLx Buffer
*&
*&https://me.sap.com/notes/2498143
*&
*&Aynı zamanda su01 den kullanıcının parametre sabitlerine
*&  HRPCLX_NEW_BUFFERING  + tanımlanmalı
*&"each user can switch the central buffer ON or OFF by maintaining
*&  "user parameter HRPCLX_NEW_BUFFERING in
*&  "  Menu->System->User Profile->Own Data: Parameters
*&
*&"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*&
*&/DSL/HR80 numara aralığı tanımlaması yapılmalıdır. Tanımlı değil ise
*& /DSL/HR80_001 Bütçe Yönetimi  programı çalıştığında otomatik oluşacak
*&
*&/DSL/HR80   BS    0100000000  9999999999
*&/DSL/HR80   GR    0000000001  0060000000
*&/DSL/HR80   PR    0099000000  0099999999
*&/DSL/HR80   VR    0060000001  0089999999
*&---------------------------------------------------------------------*
REPORT /dsl/hr80_p006 MESSAGE-ID /dsl/hr80.

  TABLES : sscrfields,rlgrap,t596f.


  CLASS lcl_alv     DEFINITION DEFERRED    .
  CLASS lcl_report  DEFINITION DEFERRED.

  DATA : go_main    TYPE REF TO lcl_report.
  DATA : go_alv     TYPE REF TO lcl_alv.
  DATA : gv_error   TYPE flag .


  INCLUDE <icon> .
  INCLUDE /dsl/hr80_i001.
  INCLUDE /dsl/hr80_p006_001.
  INCLUDE /dsl/hr80_p006_002.
  INCLUDE /dsl/hr80_p006_003.

*  INITIALIZATION.
  INITIALIZATION.
    go_main = NEW #( ).
    go_alv = NEW #( ).


*  AT SELECTION-SCREEN
  AT SELECTION-SCREEN   .
    go_main->at_selection_screen( ).


  AT SELECTION-SCREEN OUTPUT.
    go_main->at_selection_screen( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_schema.
    go_main->f4_popup_schema(
      EXPORTING
        molga  = p_molga
      CHANGING
        schema = p_schema
    ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
    go_main->f4_popup_p_vanam(
      CHANGING
        p_vanam = p_vari
    ).

*  START-OF-SELECTION .
  START-OF-SELECTION .
    CLEAR gv_error.
    go_main->check_paramaters( CHANGING cv_check = gv_error ).
    CHECK gv_error IS INITIAL .
    go_main->get_data( ).


*  END-OF-SELECTION.
  END-OF-SELECTION.
    CHECK gv_error IS INITIAL .
    CALL SCREEN 0100.




*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

 SET PF-STATUS 'GUI'.
 go_main->create_alv_grid( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA : popup_return .
  DATA : lv_ucomm TYPE sy-ucomm .
  lv_ucomm = sy-ucomm.
  CASE lv_ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'  OR '&SAVE'.
      go_alv->check_changed_data( ).
      IF go_alv->record_check IS INITIAL OR
         go_alv->record_check EQ 'E' ." Değişiklik kontrolü için
         IF lv_ucomm NE '&SAVE'.
           FREE go_main.
           LEAVE TO SCREEN 0 .
         ELSE.
           MESSAGE s029.
         ENDIF.
      ELSE.
        go_main->popup_to_confirm(
          EXPORTING
            titlebar       = 'Bilgi'
            text_question  = 'Değişiklikler saklansın mı? '
            text_button_1  = 'Evet'
            text_button_2  = 'Hayır'
          IMPORTING
            answer         = popup_return
          EXCEPTIONS
            text_not_found = 1
        ).
        IF popup_return EQ '1'. " evet
          IF go_alv->record_check EQ 'C'." DEğişiklik kontrolü için
            go_main->save_main( ).
            IF lv_ucomm NE '&SAVE'.
              FREE go_main.
              LEAVE TO SCREEN 0 .
            ELSE.
              go_alv->refresh_alv( ).
              cl_gui_cfw=>flush( ).
            ENDIF.
          ENDIF.
        ELSEIF popup_return EQ '2'.  " hayır
          CLEAR : go_alv->record_check.
          IF lv_ucomm NE '&SAVE'.
            FREE go_main.
            LEAVE TO SCREEN 0 .
          ELSE.
            go_alv->refresh_alv( ).
            cl_gui_cfw=>flush( ).
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDMODULE.
