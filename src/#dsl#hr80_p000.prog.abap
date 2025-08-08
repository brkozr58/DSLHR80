*&---------------------------------------------------------------------*
*& Report /DSL/HR80_P000
*&---------------------------------------------------------------------*
REPORT /dsl/hr80_p000.

INCLUDE : /dsl/hr80_p000_i001.
INCLUDE : /dsl/hr80_p000_i002.
INCLUDE : /dsl/hr80_p000_i003.


INITIALIZATION.

  gr_alv = NEW #( ) .
  SELECTION-SCREEN FUNCTION KEY 3.

AT SELECTION-SCREEN OUTPUT.
  sscrfields-functxt_03 = TEXT-001.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  gr_alv->get_filename( ).

AT SELECTION-SCREEN .
  gr_alv->at_sel_scr( ).

START-OF-SELECTION.
  IF p_file IS NOT INITIAL.
    gr_alv->get_data( ).
  ELSE.
    MESSAGE TEXT-010 TYPE 'I' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

END-OF-SELECTION.
  gr_alv->end_of_sel( ).
