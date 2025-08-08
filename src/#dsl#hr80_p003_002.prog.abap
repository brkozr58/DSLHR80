*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_P003_002
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD constructor.
*    go_alv = NEW #( ).

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

  METHOD template_file.

    PERFORM template_file .

  ENDMETHOD.


  METHOD exc_func.
    DATA : lv_line TYPE string.

    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        program_name  = sy-repid
        dynpro_number = sy-dynnr
      IMPORTING
        file_name     = p_file.

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

  METHOD get_excel.

    PERFORM get_excel USING file_name
                    CHANGING  data      .


  ENDMETHOD.

  METHOD number_simul.
    DATA : ls_nriv TYPE nriv .

    SELECT SINGLE * FROM nriv INTO ls_nriv
      WHERE object    EQ '/DSL/HR80'
        AND nrrangenr EQ nrnr.

    IF sy-subrc EQ 0.
      IF ls_nriv-nrlevel IS INITIAL .
        number = ls_nriv-fromnumber.
      ELSE.
        number = ls_nriv-nrlevel.
        ADD 1 TO number.
      ENDIF.
    ELSE.
      MESSAGE e000 WITH nrnr
                        'Numara aralığı oluşturulmalı! '
                        'Sistem yöneticinize başvurunuz'
          DISPLAY LIKE 'I'.
      RAISE no_range.
    ENDIF.

  ENDMETHOD.

  METHOD number_get_next.
    CLEAR : number.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr                   = nrnr
        object                        = '/DSL/HR80'
      IMPORTING
        number                        = number
      EXCEPTIONS
        interval_not_found            = 1
        number_range_not_intern       = 2
        object_not_found              = 3
        quantity_is_0                 = 4
        quantity_is_not_1             = 5
        interval_overflow             = 6
        buffer_overflow               = 7
              .
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
    DATA : ls_t004 TYPE /dsl/hr80_t004 .


    LOOP AT go_alv->gt_delete INTO DATA(ls_delete).
      MOVE-CORRESPONDING ls_delete TO ls_t004.
      DELETE  /dsl/hr80_t004 FROM ls_t004.
    ENDLOOP.
    IF sy-subrc EQ 0 .
      MESSAGE s022 DISPLAY LIKE 'I' .
      CLEAR go_alv->record_check .
      REFRESH go_alv->gt_delete.
      COMMIT WORK AND WAIT .
    ENDIF.


    LOOP AT go_alv->gt_main ASSIGNING FIELD-SYMBOL(<ls_report>)
          WHERE oprtn = 'N' OR oprtn = 'C'.
      CLEAR ls_t004.

      MOVE-CORRESPONDING <ls_report> TO ls_t004.

      ls_t004-uname = sy-uname.
      ls_t004-datum = sy-datum.
      ls_t004-uzeit = sy-uzeit.
      CLEAR <ls_report>-oprtn.
      MODIFY /dsl/hr80_t004 FROM ls_t004.
    ENDLOOP.
    IF sy-subrc EQ 0 .
      MESSAGE s020 DISPLAY LIKE 'I' .
      CLEAR go_alv->record_check .
      COMMIT WORK AND WAIT .
    ENDIF.

  ENDMETHOD.

  METHOD row_record.

    PERFORM row_record USING mode
                          pt_rows .
  ENDMETHOD.

  METHOD param_move_field.
    DATA : lv_year TYPE numc2.
    DATA : lv_field(30).
    DATA : ls_t004 TYPE /dsl/hr80_t004 .

    IF begda IS INITIAL OR
       endda IS INITIAL OR
       begda EQ space   OR
       endda EQ space.
      RAISE no_date.RETURN.
    ENDIF.

    IF begda GT endda.
      RAISE big_begda.RETURN.
    ENDIF.
    IF begda(4) NE endda(4) OR
       begda(4) NE gjahr OR
       endda(4) NE gjahr.
      RAISE ne_year.RETURN.
    ENDIF.

*    IF bukrs IS INITIAL .
*      RAISE no_bukrs.RETURN.
*    ENDIF.

*    SELECT SINGLE * FROM /dsl/hr80_t004 INTO ls_t004
*          WHERE grpid IN s_grpid[]
*            AND vrsid IN s_vrsid[]
*            AND gjahr IN s_gjahr[]
*            AND bukrs EQ bukrs
*            AND kostl EQ kostl
*            AND ansvh EQ ansvh
*            AND werks EQ werks
*            AND btrtl EQ btrtl
*            AND persg EQ persg
*            AND persk EQ persk
*            AND orgeh EQ orgeh
*            AND stell EQ stell
*            AND abkrs EQ abkrs
*            AND pernr EQ pernr.
*    IF sy-subrc EQ 0 .
*      RAISE record_available.RETURN.
*    ENDIF.



    MOVE : bukrs TO ps_s004-bukrs,
           kostl TO ps_s004-kostl,
           ansvh TO ps_s004-ansvh,
           werks TO ps_s004-werks,
           btrtl TO ps_s004-btrtl,
           persg TO ps_s004-persg,
           persk TO ps_s004-persk,
           orgeh TO ps_s004-orgeh,
           stell TO ps_s004-stell,
           abkrs TO ps_s004-abkrs,
           pernr TO ps_s004-pernr.

    lv_year = begda+4(2).

    WHILE lv_year LE endda+4(2).
      CONCATENATE 'RAT' lv_year INTO lv_field  .
      ASSIGN COMPONENT lv_field OF STRUCTURE ps_s004 TO FIELD-SYMBOL(<fs>).
      <fs> = ratxx.
      CONCATENATE 'ANZ' lv_year INTO lv_field  .
      ASSIGN COMPONENT lv_field OF STRUCTURE ps_s004 TO <fs>.
      <fs> = anzxx .
      CONCATENATE 'BET' lv_year INTO lv_field  .
      ASSIGN COMPONENT lv_field OF STRUCTURE ps_s004 TO <fs>.
      <fs> = betxx.
      ADD 1 TO lv_year  .
    ENDWHILE.

  ENDMETHOD.


ENDCLASS.                 " lcl_report IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.


  METHOD set_layout.
    ms_layout-box_fname = 'MARK'.
    ms_layout-col_opt = 'X'.
    ms_layout-sel_mode = 'A'.
    ms_layout-zebra = abap_true.
    ms_layout-stylefname = 'T_STYL'.
    ms_layout-info_fname  = 'COLOR'.
  ENDMETHOD.

  METHOD create_fcat.
*    CHECK mt_fcat[] IS INITIAL .
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
    modify_target_value(  fname  = 'MARK'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'OPRTN'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'COLOR'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'GJAHR'    targt  = 'TEXT'   zvalue = TEXT-gja ).
    modify_target_value(  fname  = 'GRPID'    targt  = 'TEXT'   zvalue = TEXT-grp ).
    modify_target_value(  fname  = 'VRSID'    targt  = 'TEXT'   zvalue = TEXT-vrs ).

    modify_target_value(  fname  = 'MOLGA'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'GJAHR'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'GRPID'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'VRSID'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'T_STYL'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'WAERS'    targt  = 'NO_OUT' zvalue = 'X' ).

    modify_target_value(  fname  = 'UNAME'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'DATUM'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value(  fname  = 'UZEIT'    targt  = 'NO_OUT' zvalue = 'X' ).

    modify_target_value(  fname  = 'VRSID'    targt  = 'COL_POS' zvalue = '1' ).
    modify_target_value(  fname  = 'GRPID'    targt  = 'COL_POS' zvalue = '1' ).
    modify_target_value(  fname  = 'GJAHR'    targt  = 'COL_POS' zvalue = '1' ).
    modify_target_value(  fname  = 'MOLGA'    targt  = 'COL_POS' zvalue = '1' ).


    SELECT * FROM t247 INTO TABLE @DATA(lt_t247)
      WHERE spras EQ @sy-langu.

    LOOP AT mt_fcat INTO DATA(ls_fcat) WHERE fieldname(3) EQ 'RAT'
                                          OR fieldname(3) EQ 'ANZ'
                                          OR fieldname(3) EQ 'BET'.
      READ TABLE lt_t247 INTO DATA(ls_t247)
            WITH KEY mnr = ls_fcat-fieldname+3(2).
      CONCATENATE ls_t247-ltx
                  ls_fcat-scrtext_l
               INTO ls_fcat-scrtext_l SEPARATED BY space.

      modify_target_value(  fname  = ls_fcat-fieldname
                            targt  = 'TEXT'
                            zvalue = ls_fcat-scrtext_l ).
      modify_target_value(  fname  = ls_fcat-fieldname
                            targt  = 'EDIT'
                            zvalue = 'X' ).
    ENDLOOP.

    LOOP AT mt_fcat INTO ls_fcat
         WHERE fieldname    EQ 'BUKRS'
            OR fieldname    EQ 'KOSTL'
            OR fieldname    EQ 'ANSVH'
            OR fieldname    EQ 'WERKS'
            OR fieldname    EQ 'BTRTL'
            OR fieldname    EQ 'PERSG'
            OR fieldname    EQ 'PERSK'
            OR fieldname    EQ 'ORGEH'
            OR fieldname    EQ 'STELL'
            OR fieldname    EQ 'ABKRS'
            OR fieldname    EQ 'PERNR'
            OR fieldname    EQ 'VRSID'
            OR fieldname    EQ 'GRPID'
            OR fieldname    EQ 'GJAHR'
            OR fieldname    EQ 'MOLGA' .
      IF r_rd1 EQ 'X' .
        modify_target_value(  fname  = ls_fcat-fieldname
                              targt  = 'EDIT'
                              zvalue = space ).
        modify_target_value(  fname  = ls_fcat-fieldname
                              targt  = 'KEY'
                              zvalue = 'X' ).
      ELSE.
        modify_target_value(  fname  = ls_fcat-fieldname
                              targt  = 'EDIT'
                              zvalue = 'X' ).

      ENDIF.
      modify_target_value(  fname  = ls_fcat-fieldname
                            targt  = 'COLDDICTXT'
                            zvalue = 'L' ).

    ENDLOOP.
    SORT mt_fcat ASCENDING BY col_pos.

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
    mo_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
    mo_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).

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
    DATA lt_rows TYPE lvc_t_row.
    DATA lt_rown TYPE lvc_t_roid.
    DATA : answer.


    CASE e_ucomm.
      WHEN 'INSERT'.
        go_main->row_record( mode = 'I' ).

      WHEN 'MODFIY'.
        check_changed_data( ).
        IF record_check EQ 'C'." DEğişiklik kontrolü için
          go_main->popup_to_confirm(
            EXPORTING
              titlebar       = 'Bilgi'
              text_question  = 'Değişiklikler saklansın mı? '
              text_button_1  = 'Evet'
              text_button_2  = 'Hayır'
            IMPORTING
              answer         = answer
            EXCEPTIONS
              text_not_found = 1
          ).

          IF answer EQ '1'. " EVET
            go_main->save_main( ).
          ELSE.
            CLEAR : record_check.
          ENDIF.
        ENDIF.

      WHEN 'CHANGE'.

        mo_grid->get_selected_rows(
          IMPORTING
            et_index_rows = lt_rows
            et_row_no     = lt_rown
        ).
        IF lines( lt_rows ) IS INITIAL.
          MESSAGE s026 DISPLAY LIKE 'E'.
          RETURN.
        ELSEIF lines( lt_rows ) GT 1 .
          MESSAGE s030 DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        go_main->row_record( mode = 'C' pt_rows = lt_rows ).

      WHEN 'DELETE'.
        mo_grid->get_selected_rows(
          IMPORTING
            et_index_rows = lt_rows
            et_row_no     = lt_rown
        ).

        IF lines( lt_rown ) GT 0.
          go_main->popup_to_confirm(
            EXPORTING
              titlebar       = 'Uyarı'
              text_question  = TEXT-dlt
              text_button_1  = 'Evet'
              text_button_2  = 'Hayır'
            IMPORTING
              answer         = answer
            EXCEPTIONS
              text_not_found = 1
          ).

          IF answer EQ '1'.
            record_check = 'C'." DEğişiklik kontrolü için
            LOOP AT lt_rown ASSIGNING FIELD-SYMBOL(<ls_rown>).
              go_alv->gt_main[ <ls_rown>-row_id ]-mark = abap_true.
            ENDLOOP.

            go_alv->gt_delete = VALUE #( FOR ls IN  go_alv->gt_main
                  WHERE ( mark = 'X' )  ( ls ) ).
            DELETE go_alv->gt_main WHERE mark = abap_true.
          ENDIF.
        ELSE.
          MESSAGE s006 DISPLAY LIKE 'E' .
        ENDIF.

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
    INSERT VALUE #( butn_type = 3 )
            INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( butn_type = 0
                    function  = 'INSERT'
                    icon      = icon_insert_row
                    disabled  = space
                    text      = 'Parametre Ekle'
                    )
            INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( butn_type = 0
                    function  = 'CHANGE'
                    icon      = icon_change
                    disabled  = space
                    text      = 'Parametre Değiştir'
                    )
            INTO TABLE e_object->mt_toolbar.
    INSERT VALUE #( butn_type = 0
                    function  = 'DELETE'
                    icon      = icon_delete_row
                    disabled  = space
                    text      = 'Parametre Sil'
                    )
           INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( butn_type = 0
                    function  = 'MODFIY'
                    icon      = icon_system_save
                    quickinfo = TEXT-t01
                    disabled  = space
                    text      = 'Kaydet'
                    )
            INTO TABLE e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_data_changed.
    DATA : s_t003 TYPE /dsl/hr80_t003.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_cells>).
      READ TABLE me->gt_main ASSIGNING FIELD-SYMBOL(<ls_report>) INDEX <ls_cells>-row_id.
      CHECK sy-subrc = 0.
      IF record_check NE 'E'. " hata varsa değişiklik var mesajı gelmemesi için
        record_check = 'C'. " DEğişiklik kontrolü için
        <ls_report>-oprtn = 'C'.
      ENDIF.

*      <ls_report>-uname = sy-uname.
*      <ls_report>-datum = sy-datum.
*      <ls_report>-uzeit = sy-uzeit.

    ENDLOOP.

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

    SELECT SINGLE * FROM /dsl/hr80_t003 INTO ls_t003
          WHERE grpid IN s_grpid[]
            AND vrsid IN s_vrsid[]
            AND statu IN s_statu[].
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
      SHIFT ls_t001-grpid LEFT DELETING LEADING '0'.
      SHIFT ls_t003-vrsid LEFT DELETING LEADING '0'.
      CONCATENATE ls_t001-grpid ls_t001-grpid_t INTO ls_t001-grpid_t SEPARATED BY '-'.
      CONCATENATE ls_t003-vrsid ls_t003-vrsid_t INTO ls_t003-vrsid_t SEPARATED BY '-'.
      add_line : 'Ülke gruplaması   :' p_molga.
      add_line : 'Mali yıl          :' ls_t003-gjahr.
      add_line : 'Bütçe Grubu       :' ls_t001-grpid_t.
      add_line : 'Bütçe Versiyonu   :' ls_t003-vrsid_t.


      mo_document->display_document( parent = mo_top_page ).
    ENDIF.
  ENDMETHOD.



ENDCLASS.                 " lcl_alv IMPLEMENTATION
