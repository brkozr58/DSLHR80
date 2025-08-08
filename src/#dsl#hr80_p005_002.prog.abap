*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_P005_002
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
    PERFORM modify_editible .
  ENDMETHOD.

  METHOD number_simul.
    DATA : ls_nriv TYPE nriv .
    DATA : rp_prel TYPE nriv .
*
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
    PERFORM save_main.
  ENDMETHOD.

  METHOD process_itab_data.
    DATA : number TYPE numc10.
    DATA : ls_pers TYPE /dsl/hr80_ddl002.
    DATA : lt_temp LIKE TABLE OF go_alv->gs_main.
    DATA : lmc_de TYPE raw4 .
    DATA : lt_styl  TYPE lvc_t_styl.

    lt_styl = VALUE #( FOR ls IN go_alv->mt_fcat
              WHERE ( no_out NE 'X'
                AND edit   EQ 'X'
                AND fieldname NE 'RFPER' )
          ( fieldname = ls-fieldname
            style     = cl_gui_alv_grid=>mc_style_enabled ) ).

    lmc_de = cl_gui_alv_grid=>mc_style_enabled.
*    IF is_data-pernr GT '8999999'.
    IF is_data-pernr GT go_alv->gmc_dummy.
      INSERT VALUE #( fieldname = 'RFPER' style = lmc_de )
            INTO TABLE lt_styl .
    ELSEIF is_data-pernr EQ '00000000'.
      INSERT VALUE #( fieldname = 'RFPER' style = lmc_de )
            INTO TABLE lt_styl .
    ELSE.
      INSERT VALUE #( fieldname = 'RFPER'
                          style = cl_gui_alv_grid=>mc_style_disabled )
            INTO TABLE lt_styl .
    ENDIF.

    go_main->number_simul(
      EXPORTING
        nrnr     = CONV #( TEXT-nrn )
      IMPORTING
        number   = number
      EXCEPTIONS
        no_range = 1
    ).
    IF sy-subrc <> 0.
      RAISE no_range. EXIT.
    ENDIF.

    CASE mode .
      WHEN 'C' . " Değiştir
        READ TABLE go_alv->gt_main ASSIGNING FIELD-SYMBOL(<fs>)
             INDEX index.

      WHEN 'I'. " Yeni kayıt reel personel ekle
*        CLEAR <fs>.
        APPEND INITIAL LINE TO go_alv->gt_main ASSIGNING <fs>.


      WHEN 'N'. " Yeni kayıt DUMMY personel
        SELECT SINGLE * FROM /dsl/hr80_ddl002 INTO ls_pers
          WHERE pernr   EQ is_data-rfper
            AND grpid   IN s_grpid[]
            AND vrsid   IN s_vrsid[]
            AND gjahr   IN s_gjahr[]
            AND molga   EQ p_molga.
        IF sy-subrc EQ 0 .
          DO is_data-zcount TIMES.
            APPEND INITIAL LINE TO go_alv->gt_main ASSIGNING <fs>.
            MOVE-CORRESPONDING ls_pers TO <fs>.
            CLEAR : <fs>-pernr   .
            lt_temp = go_alv->gt_main[].
*            DELETE lt_temp WHERE pernr LT 90000000.
            DELETE lt_temp WHERE pernr LT go_alv->gmc_dummy.
            DESCRIBE TABLE lt_temp LINES DATA(zline).
            <fs>-pernr = number+2(8) + zline . " pernr 8 karekter

            <fs>-orgeh      = is_data-orgeh .
            <fs>-orgeh_t    = is_data-orgeh_t .
            <fs>-plans      = is_data-plans .
            <fs>-plans_t    = is_data-plans_t .
            <fs>-stell      = is_data-stell .
            <fs>-stell_t    = is_data-stell_t .
            <fs>-kokrs      = is_data-kokrs .
            <fs>-kostl      = is_data-kostl .
            <fs>-kostl_t    = is_data-kostl_t .
            <fs>-rfper      = is_data-rfper     .
            <fs>-ename      = is_data-ename     .
            <fs>-pa1_begda  = is_data-begda .
            <fs>-pa1_endda  = is_data-endda .
            <fs>-werks_new  = is_data-werks_new .
            <fs>-btrtl_new  = is_data-btrtl_new .
            <fs>-persg_new  = is_data-persg_new .
            <fs>-persk_new  = is_data-persk_new .
            <fs>-trfar_new  = is_data-trfar_new .
            <fs>-trfgb_new  = is_data-trfgb_new .
            <fs>-trfgr_new  = is_data-trfgr_new .
            <fs>-trfst_new  = is_data-trfst_new .
            <fs>-lga01_new  = is_data-lga01_new .
            <fs>-salry_new  = is_data-salry_new .
            <fs>-oprtn      = 'N'.
            <fs>-color      = 'C110'.
            <fs>-t_styl     = lt_styl[].
          ENDDO.
          EXIT.

        ELSE.
          RAISE no_refpr.RETURN.
        ENDIF.
    ENDCASE.

    CASE mode.
      WHEN 'I' OR 'C'.
        <fs>-pernr      = is_data-pernr .
        <fs>-ename      = is_data-ename .
        <fs>-pa1_begda  = is_data-begda .
        <fs>-pa1_endda  = is_data-endda .
        <fs>-orgeh      = is_data-orgeh .
        <fs>-orgeh_t    = is_data-orgeh_t .
        <fs>-plans      = is_data-plans .
        <fs>-plans_t    = is_data-plans_t .
        <fs>-stell      = is_data-stell .
        <fs>-stell_t    = is_data-stell_t .
        <fs>-kokrs      = is_data-kokrs .
        <fs>-kostl      = is_data-kostl .
        <fs>-kostl_t    = is_data-kostl_t .
        <fs>-werks_new  = is_data-werks_new .
        <fs>-btrtl_new  = is_data-btrtl_new .
        <fs>-persg_new  = is_data-persg_new .
        <fs>-persk_new  = is_data-persk_new .
        <fs>-trfar_new  = is_data-trfar_new .
        <fs>-trfgb_new  = is_data-trfgb_new .
        <fs>-trfgr_new  = is_data-trfgr_new .
        <fs>-trfst_new  = is_data-trfst_new .
        <fs>-lga01_new  = is_data-lga01_new .
        <fs>-salry_new  = is_data-salry_new .
        <fs>-oprtn      = 'C'.
        <fs>-color      = 'C310'.
        <fs>-t_styl     = lt_styl[].
    ENDCASE.

  ENDMETHOD.

  METHOD check_values .
*    IMPORTING is_data TYPE /dsl/hr80_s005
*              i_gjahr type i_gjahr
*                          EXCEPTIONS no_begda
*                                     no_endda
*                                     big_begda
*                                     ne_year  .
    DATA : ls_pers TYPE /dsl/hr80_ddl002.

    IF is_data-begda IS INITIAL .
      RAISE no_begda.RETURN.
    ELSEIF is_data-endda IS INITIAL .
      RAISE no_endda.RETURN.
    ELSEIF is_data-begda GT is_data-endda.
      RAISE big_begda.RETURN.
    ENDIF.

    IF i_gjahr NOT BETWEEN is_data-begda(4) AND is_data-endda(4) .
      RAISE ne_year.RETURN.
    ENDIF.

    IF is_data-zcount IS INITIAL AND is_data-mode EQ 'N'.
      RAISE no_count.RETURN.
    ENDIF.

    IF is_data-mode EQ 'N'.
      SELECT SINGLE * FROM /dsl/hr80_ddl002 INTO CORRESPONDING
              FIELDS OF ls_pers
        WHERE pernr   EQ is_data-rfper
          AND grpid   IN s_grpid[]
          AND vrsid   IN s_vrsid[]
          AND gjahr   IN s_gjahr[]
          AND molga   EQ p_molga.
      IF sy-subrc NE 0.
        RAISE no_refpr.RETURN.
      ENDIF.
    ENDIF.
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

  METHOD get_excel.
    PERFORM get_excel  USING file_name .
  ENDMETHOD.

  METHOD template_file.
    PERFORM template_file .
  ENDMETHOD.

  METHOD get_text.
    DATA : lt_1000      TYPE TABLE OF hrp1000 .
    DATA : lt_tka01     TYPE TABLE OF tka01 .
    DATA : lt_cskt      TYPE TABLE OF cskt .
    DATA : lt_t001      TYPE TABLE OF t001 .
    DATA : lt_t500p     TYPE TABLE OF t500p .
    DATA : lt_t001p     TYPE TABLE OF t001p .
    DATA : lt_t501t     TYPE TABLE OF t501t .
    DATA : lt_t503t     TYPE TABLE OF t503t .
    DATA : lt_t510a     TYPE TABLE OF t510a .
    DATA : lt_t510g     TYPE TABLE OF t510g .
    DATA : lt_t529t     TYPE TABLE OF t529t .
    DATA : lt_t530t     TYPE TABLE OF t530t .
    DATA : ls_main      LIKE go_alv->gs_main.


    SELECT  * FROM hrp1000 INTO TABLE lt_1000
        FOR ALL ENTRIES IN go_alv->gt_main
          WHERE plvar EQ '01'
            AND otype EQ 'O'
            AND objid EQ go_alv->gt_main-orgeh
            AND langu EQ sy-langu
            AND endda GE sy-datum .
*
    SELECT  * FROM hrp1000 APPENDING TABLE lt_1000
        FOR ALL ENTRIES IN go_alv->gt_main
          WHERE plvar EQ '01'
            AND otype EQ 'S'
            AND objid EQ go_alv->gt_main-plans
            AND langu EQ sy-langu
            AND endda GE sy-datum .

    SELECT  * FROM hrp1000 APPENDING TABLE lt_1000
        FOR ALL ENTRIES IN go_alv->gt_main
          WHERE plvar EQ '01'
            AND otype EQ 'C'
            AND objid EQ go_alv->gt_main-stell
            AND langu EQ sy-langu
            AND endda GE sy-datum .

    SELECT * FROM t001 INTO TABLE lt_t001
        FOR ALL ENTRIES IN go_alv->gt_main
      WHERE bukrs EQ go_alv->gt_main-bukrs  .

    SELECT * FROM tka01 INTO TABLE lt_tka01
        FOR ALL ENTRIES IN go_alv->gt_main
        WHERE kokrs EQ go_alv->gt_main-kokrs .

    SELECT * FROM cskt INTO TABLE lt_cskt
      FOR ALL ENTRIES IN lt_tka01
        WHERE kokrs EQ lt_tka01-kokrs
          AND spras EQ sy-langu
          AND datbi GE sy-datum.

    SELECT * FROM t500p INTO TABLE lt_t500p
        WHERE molga EQ p_molga.
    SELECT * FROM t001p INTO TABLE lt_t001p
      FOR ALL ENTRIES IN lt_t500p
        WHERE werks EQ lt_t500p-persa
          AND molga EQ lt_t500p-molga.
    SELECT * FROM t501t INTO TABLE lt_t501t WHERE sprsl EQ sy-langu .
    SELECT * FROM t503t INTO TABLE lt_t503t WHERE sprsl EQ sy-langu .
    SELECT * FROM t510a INTO TABLE lt_t510a WHERE molga EQ p_molga .
    SELECT * FROM t510g INTO TABLE lt_t510g WHERE molga EQ p_molga .
    SELECT * FROM t529t INTO TABLE lt_t529t WHERE sprsl EQ sy-langu .
    SELECT * FROM t530t INTO TABLE lt_t530t WHERE sprsl EQ sy-langu .

    SORT lt_1000  ASCENDING BY otype objid begda.
    SORT lt_tka01 ASCENDING .
    SORT lt_cskt  ASCENDING .
    SORT lt_t001  ASCENDING .
    SORT lt_t500p ASCENDING .
    SORT lt_t001p ASCENDING .
    SORT lt_t501t ASCENDING .
    SORT lt_t503t ASCENDING .
    SORT lt_t510a ASCENDING .
    SORT lt_t510g ASCENDING .
    SORT lt_t529t ASCENDING .
    SORT lt_t530t ASCENDING .

    DELETE ADJACENT DUPLICATES FROM lt_1000 .
    DELETE ADJACENT DUPLICATES FROM lt_tka01.
    DELETE ADJACENT DUPLICATES FROM lt_cskt .
    DELETE ADJACENT DUPLICATES FROM lt_t001 .
    DELETE ADJACENT DUPLICATES FROM lt_t500p.
    DELETE ADJACENT DUPLICATES FROM lt_t001p.
    DELETE ADJACENT DUPLICATES FROM lt_t501t.
    DELETE ADJACENT DUPLICATES FROM lt_t503t.
    DELETE ADJACENT DUPLICATES FROM lt_t510a.
    DELETE ADJACENT DUPLICATES FROM lt_t510g.
    DELETE ADJACENT DUPLICATES FROM lt_t529t.
    DELETE ADJACENT DUPLICATES FROM lt_t530t.


    LOOP AT lt_1000 INTO DATA(ls_1000).
      CASE ls_1000-otype.
        WHEN 'O'.
          ASSIGN COMPONENT 'ORGEH_T' OF STRUCTURE ls_main TO
              FIELD-SYMBOL(<stext>).
          <stext> = ls_1000-stext.
          MODIFY go_alv->gt_main FROM ls_main
            TRANSPORTING orgeh_t
              WHERE orgeh EQ ls_1000-objid.
        WHEN 'S'.
          ASSIGN COMPONENT 'PLANS_T' OF STRUCTURE ls_main TO <stext>.
          <stext> = ls_1000-stext.
          MODIFY go_alv->gt_main FROM ls_main
            TRANSPORTING plans_t
              WHERE plans EQ ls_1000-objid.
        WHEN 'C'.
          ASSIGN COMPONENT 'STELL_T' OF STRUCTURE ls_main TO <stext>.
          <stext> = ls_1000-stext.
          MODIFY go_alv->gt_main FROM ls_main
            TRANSPORTING stell_t
              WHERE stell EQ ls_1000-objid.
      ENDCASE.
      UNASSIGN : <stext>.
    ENDLOOP.

    LOOP AT lt_tka01 INTO DATA(ls_tka01).
      ASSIGN COMPONENT 'KOKRS_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_tka01-bezei.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING kokrs_t
          WHERE kokrs EQ ls_tka01-kokrs.
      UNASSIGN <stext>.
      LOOP AT lt_cskt INTO DATA(ls_cskt) WHERE kokrs EQ ls_tka01-kokrs.
        ASSIGN COMPONENT 'KOKRS_T' OF STRUCTURE ls_main TO <stext>.
        <stext> = ls_cskt-ltext.
        MODIFY go_alv->gt_main FROM ls_main
          TRANSPORTING kokrs_t
            WHERE kokrs EQ ls_tka01-kokrs
              AND kostl EQ ls_cskt-kostl.

      ENDLOOP.
      UNASSIGN <stext> .
    ENDLOOP.

    LOOP AT lt_t001 INTO DATA(ls_t001).
      ASSIGN COMPONENT 'BUKRS_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t001-butxt.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING bukrs_t
          WHERE bukrs EQ ls_t001-bukrs.
      UNASSIGN <stext>.
    ENDLOOP.

    LOOP AT lt_t500p INTO DATA(ls_t500p).
      ASSIGN COMPONENT 'WERKS_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t500p-name1.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING werks_t
          WHERE werks EQ ls_t500p-persa.
      UNASSIGN <stext>.
    ENDLOOP.

    LOOP AT lt_t001p INTO DATA(ls_t001p).
      ASSIGN COMPONENT 'BTRTL_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t001p-btext.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING btrtl_t
          WHERE btrtl EQ ls_t001p-btrtl.
      UNASSIGN <stext>.
    ENDLOOP.

    LOOP AT lt_t501t INTO DATA(ls_t501t).
      ASSIGN COMPONENT 'PTEXT_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t501t-ptext.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING ptext_t
          WHERE persg EQ ls_t501t-persg.
      UNASSIGN <stext>.

      ASSIGN COMPONENT 'PERSG_NEW_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t501t-ptext.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING persg_new_t
          WHERE persg_new EQ ls_t501t-persg.
      UNASSIGN <stext>.
    ENDLOOP.

    LOOP AT lt_t503t INTO DATA(ls_t503t)  .
      ASSIGN COMPONENT 'PERSK_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t503t-ptext.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING persk_t
          WHERE persk EQ ls_t503t-persk.
      UNASSIGN <stext>.

      ASSIGN COMPONENT 'PERSK_NEW_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t503t-ptext.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING persg_new_t
          WHERE persk_new EQ ls_t503t-persk.
      UNASSIGN <stext>.
    ENDLOOP.


    LOOP AT lt_t510a INTO DATA(ls_t510a).
      ASSIGN COMPONENT 'TRFAR_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t510a-tartx.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING trfar_t
          WHERE trfar EQ ls_t510a-trfar.
      UNASSIGN <stext>.

      ASSIGN COMPONENT 'TRFAR_NEW_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t510a-tartx.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING trfar_new_t
          WHERE trfar_new EQ ls_t510a-trfar.
      UNASSIGN <stext>.
    ENDLOOP.

    LOOP AT lt_t510g INTO DATA(ls_t510g).
      ASSIGN COMPONENT 'TRFGB_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t510g-tgbtx.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING trfgb_t
          WHERE trfgb EQ ls_t510g-trfgb.
      UNASSIGN <stext>.

      ASSIGN COMPONENT 'TRFGB_NEW_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t510g-tgbtx.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING trfgb_new_t
          WHERE trfgb_new EQ ls_t510g-trfgb.
      UNASSIGN <stext>.
    ENDLOOP.

    LOOP AT lt_t529t INTO DATA(ls_t529t).
      ASSIGN COMPONENT 'MASSN_T' OF STRUCTURE ls_main TO <stext>.
      <stext> = ls_t529t-mntxt.
      MODIFY go_alv->gt_main FROM ls_main
        TRANSPORTING massn_t
          WHERE massn EQ ls_t529t-massn.
      UNASSIGN <stext>.

      LOOP AT lt_t530t INTO DATA(ls_t530t) WHERE massn EQ ls_t529t-massn.
        ASSIGN COMPONENT 'MASSG_T' OF STRUCTURE ls_main TO <stext>.
        <stext> = ls_t530t-mgtxt.
        MODIFY go_alv->gt_main FROM ls_main
          TRANSPORTING massg_t
            WHERE massn EQ ls_t529t-massn.
        UNASSIGN <stext>.
      ENDLOOP.
    ENDLOOP.
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
      gmc_dummy = gmc_dummy - 1 .
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

    CASE e_ucomm.
      WHEN 'COPY'.
        PERFORM row_record USING 'N' lt_rows .

      WHEN 'INSERT'.
        PERFORM change_record USING 'I' .

      WHEN 'CHANGE'.
        PERFORM change_record USING 'C' .

      WHEN 'MODFIY'.
        PERFORM modify_record .

      WHEN 'DELETE'.
        PERFORM delete_record .

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
    IF go_alv->gs_t003-statu EQ '0' OR go_alv->gs_t003-statu EQ '1'.
      INSERT VALUE #( butn_type = 0
                      function  = 'COPY'
                      icon      = icon_new_employee
                      disabled  = space
                      text      = 'DUMMY Personel Oluştur'
                      )
              INTO TABLE e_object->mt_toolbar.

      INSERT VALUE #( butn_type = 0
                      function  = 'INSERT'
                      icon      = icon_insert_row
                      disabled  = space
                      text      = 'Personel Ekle'
                      )
              INTO TABLE e_object->mt_toolbar.

      INSERT VALUE #( butn_type = 0
                      function  = 'CHANGE'
                      icon      = icon_change
                      disabled  = space
                      text      = 'Personel Değiştir'
                      )
              INTO TABLE e_object->mt_toolbar.
      INSERT VALUE #( butn_type = 0
                      function  = 'DELETE'
                      icon      = icon_delete_row
                      disabled  = space
                      text      = 'Personel Sil'
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

    ENDIF.


    INSERT VALUE #( butn_type = 0
                    function  = 'STATU'
                    icon      = icon_set_state
                    disabled  = space
                    text      = 'Versiyon durumu değiştir'
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
