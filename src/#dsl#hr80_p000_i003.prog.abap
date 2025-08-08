*&---------------------------------------------------------------------*
*& Include          ZNCPRH_P006_I003
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZZP0001_I003
*&---------------------------------------------------------------------*

FORM gui USING g_xpa TYPE slis_t_extab.
  SET PF-STATUS 'GUIALV'.
ENDFORM.                    "gui
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = gt_top[].
ENDFORM.                    "top_of_page
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  DATA: li_count TYPE i.
  rs_selfield-refresh = 'X'.
  CASE r_ucomm.
    WHEN 'BASL'.
      IF rb1 EQ 'X'.
        gr_alv->batch_ise_alim( ).
      ELSE.
        gr_alv->batch_isten_cikis( ).
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
