*&---------------------------------------------------------------------*
*& Report /DSL/HR80_P005
*&---------------------------------------------------------------------*
REPORT /dsl/hr80_p005 MESSAGE-ID /dsl/hr80.

  TABLES : sscrfields,rlgrap.


  CLASS lcl_alv     DEFINITION DEFERRED    .
  CLASS lcl_report  DEFINITION DEFERRED.

  DATA : go_main    TYPE REF TO lcl_report.
  DATA : go_alv     TYPE REF TO lcl_alv.
  DATA : gv_error   TYPE flag .


  INCLUDE <icon> .
  INCLUDE /dsl/hr80_i001.
  INCLUDE /dsl/hr80_p005_001.
  INCLUDE /dsl/hr80_p005_002.
  INCLUDE /dsl/hr80_p005_003.
  INCLUDE /dsl/hr80_p005_004.



*  INITIALIZATION.
  INITIALIZATION.
    go_main = NEW #( ).
    go_alv = NEW #( ).

    REFRESH s_statu.
    APPEND VALUE #( sign = 'I' option = 'EQ'  low = '0' ) TO s_statu.
    APPEND VALUE #( sign = 'I' option = 'EQ'  low = '1' ) TO s_statu.

*  AT SELECTION-SCREEN
  AT SELECTION-SCREEN   .
    go_main->at_selection_screen( ).


  AT SELECTION-SCREEN OUTPUT.
    go_main->at_selection_screen( ).

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
    go_main->exc_func( ).


*  START-OF-SELECTION .
  START-OF-SELECTION .
    CLEAR gv_error.
    go_main->check_paramaters( CHANGING cv_check = gv_error ).
    CHECK gv_error IS INITIAL .
    CASE 'X'.
      WHEN r_rd1 . " Rapor
        go_main->get_data( ).

      WHEN r_rd2 . " Toplu aktarım
        go_main->get_excel(
         EXPORTING
           file_name = p_file ).

        go_main->get_text( ).

      WHEN OTHERS.
    ENDCASE.

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
