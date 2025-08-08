*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_P006_002
*&---------------------------------------------------------------------*
  INCLUDE lhrplto1.
  INCLUDE lhrplto3.
*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD constructor.

    SELECT * FROM t596f INTO t596f WHERE sname EQ '47SCHEMA'
                          AND begda LE sy-datum
                          AND endda GE sy-datum
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc EQ 0.
      IF t596f-modna NE 'DUMMY'.
        PERFORM (t596f-modna) IN PROGRAM (t596f-pgmna)
                                   USING   p_schema.
      ENDIF.
    ENDIF.

    p_schema = '=R00'.
    REFRESH s_statu.
    APPEND VALUE #( sign = 'I' option = 'EQ'  low = '2' ) TO s_statu.

    chlogvar = '@0Z@'.

  ENDMETHOD.

  METHOD check_paramaters.
    PERFORM check_paramaters CHANGING cv_check.
  ENDMETHOD.


  METHOD at_selection_screen.
    PERFORM at_selection_screen .
  ENDMETHOD.

  METHOD split_container.
    mo_cont = NEW #(  container_name = scrfname
                      dynnr          = screen
                      repid          = sy-repid ).
    IF mo_cont IS BOUND.
      mo_splitter = NEW #(
        parent  = mo_cont
        rows    = 2
        columns = 1 ).

      mo_splitter->set_border( abap_true ).
      mo_splitter->set_row_height( id = 1 height = 20 ).
      mo_splitter->set_row_height( id = 2 height = 80 ).
    ENDIF.
  ENDMETHOD.

  METHOD create_alv_grid.
    CHECK mo_splitter IS NOT BOUND.
    split_container( scrfname = 'CONT' screen   = '0100' ).
    mo_alv_cont = mo_splitter->get_container( row    = 2
                                              column = 1 ).


    go_alv->mo_grid = NEW #( i_parent = mo_alv_cont ).

    IF go_alv IS BOUND.
      go_alv->set_layout( ).
*      go_alv->create_fcat( ).
      go_alv->exclude_buttons( ).
      go_alv->set_top_of_page(
        CHANGING
          mo_splitter = mo_splitter
      ).
      go_alv->set_events( ).
      go_alv->display_alv_grid(
        CHANGING
          it_main = go_alv->gt_main[] ).
    ENDIF.
  ENDMETHOD.

  METHOD get_data.
    PERFORM get_data .
  ENDMETHOD.

  METHOD popup_to_confirm.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = titlebar
        text_question         = text_question
        text_button_1         = text_button_1
        text_button_2         = text_button_2
        default_button        = '2'
        display_cancel_button = 'X'
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc NE 0 .
      RAISE text_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD save_main.
    PERFORM save_main.
  ENDMETHOD.


  METHOD check_values .

  ENDMETHOD.

  METHOD f4_popup_schema.
    CALL FUNCTION 'F4_POPUP_SCHEMA'
     EXPORTING
        molga   = molga
        titel   = 'Geçerli şemalar'
     IMPORTING
        sname   = schema
    EXCEPTIONS
        OTHERS  = 1.
  ENDMETHOD.

  METHOD f4_popup_p_vanam.
    CALL FUNCTION 'F4_POPUP_P_VANAM'
      IMPORTING
        p_vanam = p_vanam
      EXCEPTIONS
        OTHERS  = 0.
  ENDMETHOD.

  METHOD calc_simu.
    PERFORM calc_simu .
  ENDMETHOD.

ENDCLASS.                 " lcl_report IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.

  METHOD constructor.
    DATA : ls_nriv TYPE nriv.

    SELECT SINGLE * FROM nriv INTO ls_nriv
      WHERE object    EQ '/DSL/HR80'
        AND nrrangenr EQ 'PR'.

      gmc_dummy = ls_nriv-fromnumber+2(8).

  ENDMETHOD.


  METHOD set_layout.
    ms_layout-box_fname = 'MARK'.
    ms_layout-col_opt = 'X'.
    ms_layout-sel_mode = 'A'.
    ms_layout-zebra = 'X'.
    ms_layout-stylefname = 'T_STYL'.
    ms_layout-info_fname  = 'COLOR'.
  ENDMETHOD.

  METHOD create_fcat.
    CHECK mt_fcat[] IS INITIAL .
    REFRESH mt_fcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = gc_strname
        i_client_never_display = abap_true
        i_bypassing_buffer     = abap_true
      CHANGING
        ct_fieldcat            = mt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.
      me->modify_fcat( ).
    ENDIF.

  ENDMETHOD.

  METHOD modify_fcat.
    PERFORM modify_fcat .

  ENDMETHOD.

  METHOD modify_target_value.
    READ TABLE mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
        WITH KEY fieldname  = fname.
    IF sy-subrc EQ 0 .
      CASE targt .
        WHEN 'TEXT'.
          modify_target_value(  fname  = fname targt = 'SCRTEXT_L' zvalue = zvalue ).
          modify_target_value(  fname  = fname targt = 'SCRTEXT_M' zvalue = zvalue ).
          modify_target_value(  fname  = fname targt = 'SCRTEXT_S' zvalue = zvalue ).
          modify_target_value(  fname  = fname targt = 'REPTEXT'   zvalue = zvalue ).
        WHEN OTHERS.
          ASSIGN COMPONENT targt OF STRUCTURE <ls_fcat>
              TO FIELD-SYMBOL(<fs>).
          CHECK <fs> IS ASSIGNED .
          <fs> = zvalue.
      ENDCASE.
      IF targt NE 'COL_OPT'.
        modify_target_value(  fname  = fname targt = 'COL_OPT' zvalue = 'X' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD check_changed_data.
    mo_grid->check_changed_data( ).

  ENDMETHOD.

  METHOD set_events.
    IF go_alv->gs_t003-statu EQ '0' OR go_alv->gs_t003-statu EQ '1'..
      mo_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
      mo_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
    ENDIF.

    IF mo_document IS BOUND.
      mo_grid->list_processing_events( i_event_name = 'TOP_OF_PAGE'
                                       i_dyndoc_id  = mo_document ).
    ENDIF.

    SET HANDLER me->handle_user_command  FOR mo_grid.
    SET HANDLER me->handle_toolbar       FOR mo_grid.
    SET HANDLER me->handle_hotspot_click FOR mo_grid.
    SET HANDLER me->handle_data_changed  FOR mo_grid.

  ENDMETHOD.

  METHOD handle_user_command.
    DATA : lt_rows TYPE lvc_t_row.
    DATA answer  .

    CASE e_ucomm.
      WHEN 'CALC'.
        go_main->popup_to_confirm(
          EXPORTING
            titlebar       = 'Bilgi'
            text_question  = TEXT-drm
            text_button_1  = 'Evet'
            text_button_2  = 'Hayır'
          IMPORTING
            answer         = answer
          EXCEPTIONS
            text_not_found = 1
        ).
        IF answer EQ '1'. " EVET
          go_main->calc_simu(
            EXCEPTIONS
              program_not_exist   = 038
              variant_not_exist   = 039
              missing_parameter   = 040
              wrong_parameter     = 041
              wrong_country_group = 042
              OTHERS              = 999
          ).
          CASE sy-subrc .
            WHEN 038. MESSAGE e038 WITH 'HTRCALC0' DISPLAY LIKE 'I'.
            WHEN 039. MESSAGE e039 WITH p_vari 'HTRCALC0' DISPLAY LIKE 'I'.
            WHEN 040. MESSAGE e040 DISPLAY LIKE 'I'.
            WHEN 041. MESSAGE e041 DISPLAY LIKE 'I'.
            WHEN 042. MESSAGE e042 DISPLAY LIKE 'I'.
            WHEN 999. MESSAGE e000 WITH 'HTRCALC0' DISPLAY LIKE 'I'.
          ENDCASE.

        ENDIF.

      WHEN 'STATU'.
        PERFORM change_statu .

      WHEN OTHERS.
        cl_gui_cfw=>dispatch( ).

    ENDCASE.
    refresh_alv( ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD handle_hotspot_click.
    CASE e_column_id.
      WHEN ''.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_toolbar.

    INSERT VALUE #( butn_type = 0
                    function  = 'STATU'
                    icon      = icon_set_state
                    disabled  = space
                    text      = 'Versiyon durumu değiştir'
                    )
            INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( butn_type = 0
                    function  = 'CALC'
                    icon      = icon_cashing_up
                    disabled  = space
                    text      = 'Hesaplamayı başlat'
                    )
            INTO TABLE e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_data_changed.
    PERFORM handle_data_changed USING er_data_changed
                                      e_onf4
                                      e_onf4_before
                                      e_onf4_after .
    refresh_alv( ).
  ENDMETHOD.

  METHOD refresh_alv.
    DATA(ls_stable) = VALUE lvc_s_stbl( row = 'X' col = 'X' ).

    IF mo_grid IS BOUND.
      mo_grid->refresh_table_display( is_stable = ls_stable ).
    ENDIF.

  ENDMETHOD.

  METHOD display_alv_grid.
    DATA : ls_var TYPE disvariant .
    ls_var-report = sy-repid.

    mo_grid->set_table_for_first_display(
      EXPORTING
        is_layout                     = ms_layout
        i_bypassing_buffer            = 'X'
        it_toolbar_excluding          = mt_exclude
        i_save                        = 'A'
        is_variant                    = ls_var
      CHANGING
        it_outtab                     = it_main
        it_fieldcatalog               = mt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD exclude_buttons.
    APPEND :
      cl_gui_alv_grid=>mc_fc_graph             TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_info              TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_print_back        TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_loc_copy_row      TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_loc_copy          TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_loc_insert_row    TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_loc_append_row    TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_loc_delete_row    TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_loc_cut           TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_loc_paste         TO mt_exclude ,
      cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO mt_exclude .
  ENDMETHOD.

  METHOD set_top_of_page.
    DATA : ls_t001 TYPE /dsl/hr80_t001.
    DATA : ls_t003 TYPE /dsl/hr80_t003.
    DATA : lv_statu TYPE text80.
    DATA : ls_dd07t TYPE dd07t .

    DEFINE add_line.
      mo_document->add_text( text = CONV #( &1 )
                            sap_color     = cl_dd_area=>list_heading_inv
                            sap_fontsize  = cl_dd_document=>large
                            sap_emphasis = cl_dd_document=>strong ).

      mo_document->add_text( text = CONV #( &2 )
                            sap_color     = cl_dd_document=>list_positive
                            sap_fontsize  = cl_dd_document=>large
                             sap_style = cl_dd_area=>emphasis ).
      mo_document->new_line(   ).
    END-OF-DEFINITION.

    MOVE-CORRESPONDING go_alv->gs_t003 TO ls_t003.

    SELECT SINGLE * FROM /dsl/hr80_t001 INTO ls_t001
          WHERE molga EQ p_molga
            AND grpid EQ ls_t003-grpid .

    mo_top_page = mo_splitter->get_container(
      row    = 1
      column = 1 ).

    mo_document = NEW #( style = 'ALV_GRID' ).

    IF mo_document IS BOUND.

*    CALL METHOD mo_document->add_picture
*      EXPORTING
*        picture_id = 'logo'.
*
*      CALL METHOD mo_document->new_line.

      mo_document->add_text( text = CONV #( sy-title )
          sap_style    = cl_dd_area=>heading
          sap_fontsize = cl_dd_area=>large
                              ).
      mo_document->new_line( 1  ).

      SELECT SINGLE * FROM dd07t INTO ls_dd07t
            WHERE domname     EQ '/DSL/HR80_STATU'
              AND ddlanguage  EQ sy-langu
              AND domvalue_l  EQ ls_t003-statu.

      SHIFT ls_t001-grpid LEFT DELETING LEADING '0'.
      SHIFT ls_t003-vrsid LEFT DELETING LEADING '0'.
      CONCATENATE ls_t001-grpid ls_t001-grpid_t INTO ls_t001-grpid_t SEPARATED BY '-'.
      CONCATENATE ls_t003-vrsid ls_t003-vrsid_t INTO ls_t003-vrsid_t SEPARATED BY '-'.
      CONCATENATE ls_t003-statu ls_dd07t-ddtext INTO lv_statu SEPARATED BY '-'.
      add_line : 'Ülke gruplaması   :' p_molga.
      add_line : 'Mali yıl          :' ls_t003-gjahr.
      add_line : 'Bütçe Grubu       :' ls_t001-grpid_t.
      add_line : 'Bütçe Versiyonu   :' ls_t003-vrsid_t.
      add_line : 'Versiyonu durumu  :' lv_statu.


      mo_document->display_document( parent = mo_top_page ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.                 " lcl_alv IMPLEMENTATION
