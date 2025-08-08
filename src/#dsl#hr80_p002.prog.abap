*&---------------------------------------------------------------------*
*& Report /DSL/HR80_P002
*&---------------------------------------------------------------------*
REPORT /dsl/hr80_p002 MESSAGE-ID /dsl/hr80.

  TABLES : sscrfields.
  CLASS lcl_alv     DEFINITION DEFERRED    .
  CLASS lcl_report  DEFINITION DEFERRED.

  DATA : go_main TYPE REF TO lcl_report.
  DATA : go_alv  TYPE REF TO lcl_alv.

  SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN PUSHBUTTON /1(24) grpid  USER-COMMAND pb_grpid  .
  SELECTION-SCREEN END OF BLOCK bl2.

  INCLUDE <icon> .
  INCLUDE /dsl/hr80_i001.
  INCLUDE /dsl/hr80_p002_001.
  INCLUDE /dsl/hr80_p002_002.

  DATA : gv_error TYPE flag .


*  INITIALIZATION.
  INITIALIZATION.
    go_main = NEW #( ).
    go_alv = NEW #( ).
    REFRESH s_statu.
    APPEND VALUE #( sign = 'I' option = 'EQ'  low = '0' ) TO s_statu.
    APPEND VALUE #( sign = 'I' option = 'EQ'  low = '1' ) TO s_statu.

    WRITE icon_usergroup AS ICON TO grpid+0(4).
    grpid+4(20) = TEXT-grp && ' oluştur'.

*  AT SELECTION-SCREEN
  AT SELECTION-SCREEN   .
    go_main->at_selection_screen( ).

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
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR '&SAVE'.
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
          go_alv->check_changed_data( ).
          IF go_alv->record_check EQ 'C'." DEğişiklik kontrolü için
            go_main->save_main( ).

            IF lv_ucomm NE '&SAVE'.
              FREE go_main.
              LEAVE TO SCREEN 0 .
            ENDIF.
          ENDIF.
        ELSE.                   " hayır
          CLEAR : go_alv->record_check.
          IF lv_ucomm NE '&SAVE'.
            FREE go_main.
            LEAVE TO SCREEN 0 .
          ENDIF.
        ENDIF.
      ENDIF.

  ENDCASE.
ENDMODULE.
