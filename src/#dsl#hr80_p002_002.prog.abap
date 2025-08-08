*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_P002_002
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  METHOD constructor.
*    go_alv = NEW #( ).

  ENDMETHOD.

  METHOD check_paramaters.
    IF s_grpid[] IS INITIAL .
      MESSAGE i001 DISPLAY LIKE 'E'.
      cv_check = 'X'.
    ENDIF.
  ENDMETHOD.

  METHOD at_selection_screen.

    CASE sscrfields-ucomm.
      WHEN 'PB_GRPID'. create_grpid( ).
      WHEN OTHERS.
    ENDCASE.

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
      go_alv->create_fcat( ).
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
    DATA : lmc_de TYPE raw4 .


    SELECT
          t1~molga
          t3~gjahr
          t3~grpid
          t1~grpid_t
          t3~vrsid
          t3~vrsid_t
          t3~statu
          t3~begda
          t3~endda
          t3~fpbeg
          t3~fpend
          t3~uname
          t3~datum
          t3~uzeit
            FROM /dsl/hr80_t003 AS t3
      INNER JOIN /dsl/hr80_t002 AS t2
                    ON    t2~grpid  EQ t3~grpid
      INNER JOIN /dsl/hr80_t001 AS t1
                    ON    t1~grpid  EQ t2~grpid
                      AND t1~molga  EQ t2~molga
      INTO CORRESPONDING
              FIELDS OF TABLE go_alv->gt_main
      WHERE t3~grpid   IN s_grpid[]
        AND t3~vrsid   IN s_vrsid[]
        AND t3~gjahr   IN s_gjahr[]
        AND t1~molga   EQ p_molga
        AND t3~statu   IN s_statu[]  .
*0
*1  Planlanıyor
*2  Hesaplama yapılıyor
*3  Tamamlandı
*4  BPC aktarıldı
    LOOP AT go_alv->gt_main ASSIGNING FIELD-SYMBOL(<ls_main>).
      CASE <ls_main>-statu.
        WHEN '1' OR '0'.
                    lmc_de = cl_gui_alv_grid=>mc_style_enabled.
        WHEN OTHERS.lmc_de = cl_gui_alv_grid=>mc_style_disabled.
      ENDCASE.
      INSERT VALUE #( fieldname = 'VRSID_T'
                      style     = lmc_de )
                     INTO TABLE <ls_main>-t_styl .
      INSERT VALUE #( fieldname = 'STATU'
                      style     = cl_gui_alv_grid=>mc_style_enabled )
                     INTO TABLE <ls_main>-t_styl .
      INSERT VALUE #( fieldname = 'BEGDA'
                      style     = lmc_de )
                     INTO TABLE <ls_main>-t_styl .
      INSERT VALUE #( fieldname = 'ENDDA'
                      style     = lmc_de )
                     INTO TABLE <ls_main>-t_styl .
      INSERT VALUE #( fieldname = 'FPBEG'
                      style     = lmc_de )
                     INTO TABLE <ls_main>-t_styl .
      INSERT VALUE #( fieldname = 'FPEND'
                      style     = lmc_de )
                     INTO TABLE <ls_main>-t_styl .
    ENDLOOP.

    SORT go_alv->gt_main ASCENDING BY gjahr begda endda .
  ENDMETHOD.

  METHOD save_main.
    DATA : ls_t003 TYPE /dsl/hr80_t003.
    DATA : lmc_de TYPE raw4 .

    LOOP AT go_alv->gt_main ASSIGNING FIELD-SYMBOL(<ls_report>)
          WHERE oprtn = 'N' OR oprtn = 'C'.
      CLEAR ls_t003.
      <ls_report>-uname = sy-uname.
      <ls_report>-datum = sy-datum.
      <ls_report>-uzeit = sy-uzeit.
      IF <ls_report>-vrsid IS INITIAL OR <ls_report>-oprtn EQ 'N'.
        go_main->number_get_next(
          EXPORTING
            nrnr     = CONV #( 'VR' )
          IMPORTING
            number   = <ls_report>-vrsid
          EXCEPTIONS
            no_range = 1
        ).
        CLEAR <ls_report>-oprtn.
      ENDIF.

      MOVE-CORRESPONDING <ls_report> TO ls_t003.


      CASE <ls_report>-statu.
        WHEN '1' OR '0'.
                    lmc_de = cl_gui_alv_grid=>mc_style_enabled.
        WHEN OTHERS.lmc_de = cl_gui_alv_grid=>mc_style_disabled.
      ENDCASE.
      REFRESH <ls_report>-t_styl.
      INSERT VALUE #( fieldname = 'VRSID_T'
                    style     = lmc_de )
                     INTO TABLE <ls_report>-t_styl .
      INSERT VALUE #( fieldname = 'STATU'
                    style     = cl_gui_alv_grid=>mc_style_enabled )
                     INTO TABLE <ls_report>-t_styl .
      INSERT VALUE #( fieldname = 'BEGDA'
                      style     = lmc_de )
                     INTO TABLE <ls_report>-t_styl .
      INSERT VALUE #( fieldname = 'ENDDA'
                      style     = lmc_de )
                     INTO TABLE <ls_report>-t_styl .
      INSERT VALUE #( fieldname = 'FPBEG'
                      style     = lmc_de )
                     INTO TABLE <ls_report>-t_styl .
      INSERT VALUE #( fieldname = 'FPEND'
                      style     = lmc_de )
                     INTO TABLE <ls_report>-t_styl .


      CLEAR <ls_report>-oprtn.
      MODIFY /dsl/hr80_t003 FROM ls_t003.
    ENDLOOP.
    IF sy-subrc EQ 0 .
      MESSAGE s020 DISPLAY LIKE 'I' .
      CLEAR go_alv->record_check .
      COMMIT WORK AND WAIT .
    ENDIF.

    LOOP AT go_alv->gt_delete INTO DATA(ls_delete).
      MOVE-CORRESPONDING ls_delete TO ls_t003.
      DELETE  /dsl/hr80_t003 FROM ls_t003.
    ENDLOOP.
    IF sy-subrc EQ 0 .
      MESSAGE s022 DISPLAY LIKE 'I' .
      CLEAR go_alv->record_check .
      REFRESH go_alv->gt_delete.
      COMMIT WORK AND WAIT .
    ENDIF.

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
      MESSAGE e021 WITH nrnr DISPLAY LIKE 'I'. RAISE no_range.
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

  METHOD create_grpid.
    FIELD-SYMBOLS <fs_struc> TYPE any .
    DATA : ls_t001 TYPE /dsl/hr80_t001.
    DATA : ls_t002 TYPE /dsl/hr80_t002.
    DATA: ivals  TYPE TABLE OF sval.
    DATA: xvals  TYPE sval.
    DATA : returncode .
    DATA : lv_grpid TYPE /dsl/hr80_t001-grpid.

*        Normal koyuluk derecesi girişe hazır
*01	açık renkli girişe hazır
*02	Normal koyuluk derecesi girişe hazır değil
*03	açık renkli girişe hazır değil
*04	Görüntüleme!

    DEFINE add_vals.
      CLEAR xvals .
      xvals-tabname       = &1.
      xvals-fieldname     = &2.
      xvals-field_attr    = &3.
*      IF &4 IS NOT INITIAL .
        xvals-value         = &4.
*      ENDIF.
      xvals-field_obl     = &5.
      IF &6 IS NOT INITIAL .
        xvals-fieldtext     = &6.
      ENDIF.
      APPEND xvals TO ivals.
    END-OF-DEFINITION.


    me->number_simul(
      EXPORTING
        nrnr     = CONV #( 'GR' )
      IMPORTING
        number   = lv_grpid
      EXCEPTIONS
        no_range = 1
    ).
    IF sy-subrc <> 0.
      CHECK 1 = 2 .
    ENDIF.

    add_vals:
    '/DSL/HR80_T001' 'MOLGA'    '03'  p_molga   abap_false space,
    '/DSL/HR80_T001' 'GRPID'    '03'  lv_grpid  abap_false space ,
    '/DSL/HR80_T001' 'GRPID_T'  space ''        abap_true  space ,

    '/DSL/HR80_T002' 'BUKRS'    space ''        abap_true  space ,
    '/DSL/HR80_T002' 'WERKS'    space ''        abap_false  space ,
    '/DSL/HR80_T002' 'BTRTL'    space ''        abap_false  space ,
    '/DSL/HR80_T002' 'ABKRS'    space ''        abap_false  space .

    CLEAR : ls_t001,ls_t002.
    DO.
      CLEAR returncode .

      CALL FUNCTION 'POPUP_GET_VALUES_SET_MAX_FIELD'
        EXPORTING
          number_of_fields = '40'
        EXCEPTIONS
          out_of_range     = 1
        .
      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
           popup_title     = 'Bütçe Grubu Girişi'
        IMPORTING
           returncode      = returncode
        TABLES
           fields          = ivals
        EXCEPTIONS
           error_in_fields = 1
           OTHERS          = 2.
      IF returncode IS INITIAL.
        LOOP AT ivals INTO xvals.
          CASE xvals-tabname.
            WHEN '/DSL/HR80_T001'. ASSIGN ls_t001 TO <fs_struc>.
            WHEN '/DSL/HR80_T002'. ASSIGN ls_t002 TO <fs_struc>.
          ENDCASE.
          ASSIGN COMPONENT xvals-fieldname OF STRUCTURE <fs_struc>
                TO FIELD-SYMBOL(<fs_f>).
          CHECK <fs_f> IS ASSIGNED .
          <fs_f> = xvals-value.
        ENDLOOP.

        IF ls_t002 IS NOT INITIAL AND ls_t001 IS NOT INITIAL .
          check_grpid(
            EXPORTING
              s_t001           = ls_t001
              s_t002           = ls_t002
            EXCEPTIONS
              no_molga         = 014
              no_bukrs         = 015
              no_werks         = 016
              no_btrtl         = 017
              no_abkrs         = 018
              record_available = 002
           ).
          IF sy-subrc NE 0 .
            CASE sy-subrc.
              WHEN 014. MESSAGE i014 DISPLAY LIKE 'E'.
              WHEN 015. MESSAGE i015 DISPLAY LIKE 'E'.
              WHEN 016. MESSAGE i016 DISPLAY LIKE 'E'.
              WHEN 017. MESSAGE i017 DISPLAY LIKE 'E'.
              WHEN 018. MESSAGE i018 DISPLAY LIKE 'E'.
              WHEN 002. MESSAGE i002 DISPLAY LIKE 'E' .
              WHEN OTHERS. MESSAGE i002 DISPLAY LIKE 'E'.
            ENDCASE.
          ELSE.
            DATA : answer.
            go_main->popup_to_confirm(
              EXPORTING
                titlebar       = 'Bilgi'
                text_question  = 'Bütçe grubu saklansın mı? '
                text_button_1  = 'Evet'
                text_button_2  = 'Hayır'
              IMPORTING
                answer         = answer
              EXCEPTIONS
                text_not_found = 1
            ).

            IF answer EQ '1'. " EVET
              me->number_get_next(
                EXPORTING
                  nrnr     = CONV #( 'GR' )
                IMPORTING
                  number   = ls_t001-grpid
                EXCEPTIONS
                  no_range = 1
              ).
              IF sy-subrc <> 0.
                MESSAGE i019 DISPLAY LIKE 'I' .
                CHECK 1 = 2 ." buraya düşmesini beklemiyorum
              ELSE.
                ls_t002-molga = ls_t001-molga.
                ls_t002-grpid = ls_t001-grpid.
                ls_t002-uname = sy-uname.
                ls_t002-datum = sy-datum.
                ls_t002-uzeit = sy-uzeit.

                ls_t001-uname = sy-uname.
                ls_t001-datum = sy-datum.
                ls_t001-uzeit = sy-uzeit.

                MODIFY /dsl/hr80_t001 FROM ls_t001.
                MODIFY /dsl/hr80_t002 FROM ls_t002.
                MESSAGE i020 DISPLAY LIKE 'I' .
                EXIT.
              ENDIF.
            ELSE. " Hayır
              EXIT.
            ENDIF.

          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD create_vrsid.
    DATA: ivals  TYPE TABLE OF sval.
    DATA: xvals  TYPE sval.
    DATA : returncode .
    DATA : ls_main TYPE /dsl/hr80_s002.
    DATA : s_t003 TYPE /dsl/hr80_t003 .
    FIELD-SYMBOLS <fs_f> TYPE any .


    ls_main-molga   = p_molga.
    ls_main-grpid   = s_grpid-low.
    SELECT SINGLE grpid_t FROM /dsl/hr80_t001 INTO ls_main-grpid_t
          WHERE grpid EQ ls_main-grpid.
    ls_main-statu = '1'.

*        Normal koyuluk derecesi girişe hazır
*01	açık renkli girişe hazır
*02	Normal koyuluk derecesi girişe hazır değil
*03	açık renkli girişe hazır değil
*04	Görüntüleme!

    DEFINE add_vals.
      xvals-tabname       = &1.
      xvals-fieldname     = &2.
      xvals-field_attr    = &3.
*      IF &4 IS NOT INITIAL .
        xvals-value         = &4.
*      ENDIF.
      xvals-field_obl     = &5.
      IF &6 IS NOT INITIAL .
        xvals-fieldtext     = &6.
      ENDIF.
      APPEND xvals TO ivals.
    END-OF-DEFINITION.

    REFRESH ivals.
    me->number_simul(
      EXPORTING
        nrnr     = CONV #( 'VR' )
      IMPORTING
        number   = ls_main-vrsid
      EXCEPTIONS
        no_range = 1
    ).
    IF sy-subrc <> 0.
      CHECK 1 = 2 .
    ENDIF.

    add_vals:
    '/DSL/HR80_T001' 'MOLGA'   '03'  ls_main-molga   abap_false space,
    '/DSL/HR80_T003' 'GRPID'   '03'  ls_main-grpid   abap_false space ,
    '/DSL/HR80_T001' 'GRPID_T' '03'  ls_main-grpid_t abap_false space ,
    '/DSL/HR80_T003' 'GJAHR'   space sy-datum(4)     abap_true space ,
    '/DSL/HR80_T003' 'VRSID'   '03'  ls_main-vrsid   abap_false space ,
    '/DSL/HR80_T003' 'VRSID_T' space ' '             abap_true  space ,

    '/DSL/HR80_T003' 'STATU'   space ls_main-statu   abap_true space ,
    '/DSL/HR80_T003' 'BEGDA'   space ' '             abap_true TEXT-beg,
    '/DSL/HR80_T003' 'ENDDA'   space ' '             abap_true TEXT-end,
    '/DSL/HR80_T003' 'FPBEG'   space ' '             abap_true TEXT-fpb,
    '/DSL/HR80_T003' 'FPEND'   space ' '             abap_true TEXT-fpe.

    CALL FUNCTION 'POPUP_GET_VALUES_SET_MAX_FIELD'
      EXPORTING
        number_of_fields = '40'
      EXCEPTIONS
        out_of_range     = 1
      .
    DO.
      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
           popup_title     = TEXT-001
        IMPORTING
           returncode      = returncode
        TABLES
           fields          = ivals
        EXCEPTIONS
           error_in_fields = 1
           OTHERS          = 2.
      IF returncode IS INITIAL.
        LOOP AT ivals INTO xvals.
          UNASSIGN <fs_f>.
          ASSIGN COMPONENT xvals-fieldname
              OF STRUCTURE ls_main TO <fs_f>.
          CHECK <fs_f> IS ASSIGNED .
          <fs_f> = xvals-value.
        ENDLOOP.

        IF ls_main IS NOT INITIAL .
          MOVE-CORRESPONDING ls_main TO s_t003.
          CLEAR : ls_main-vrsid.
          check_vrsid(
            EXPORTING
              s_t003    = s_t003
            EXCEPTIONS
              no_statu  = 007
              no_begda  = 008
              no_endda  = 009
              big_begda = 010
              no_fpbeg  = 011
              no_fpend  = 012
              big_fpbeg = 013
              ne_year   = 023
          ).
          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 007.MESSAGE i007 DISPLAY LIKE 'E' .
              WHEN 008.MESSAGE i008 DISPLAY LIKE 'E' .
              WHEN 009.MESSAGE i009 DISPLAY LIKE 'E' .
              WHEN 010.MESSAGE i010 DISPLAY LIKE 'E' .
              WHEN 011.MESSAGE i011 DISPLAY LIKE 'E' .
              WHEN 012.MESSAGE i012 DISPLAY LIKE 'E' .
              WHEN 013.MESSAGE i013 DISPLAY LIKE 'E' .
              WHEN 023.MESSAGE i023 DISPLAY LIKE 'E' .
              WHEN OTHERS.
            ENDCASE.
            CLEAR go_alv->record_check." DEğişiklik kontrolü için
          ELSE.


            INSERT VALUE #( fieldname = 'VRSID_T'
                            style     = cl_gui_alv_grid=>mc_style_enabled )
                 INTO TABLE ls_main-t_styl .
            INSERT VALUE #( fieldname = 'STATU'
                            style     = cl_gui_alv_grid=>mc_style_enabled )
                 INTO TABLE ls_main-t_styl .
            INSERT VALUE #( fieldname = 'BEGDA'
                            style     = cl_gui_alv_grid=>mc_style_enabled )
                 INTO TABLE ls_main-t_styl .
            INSERT VALUE #( fieldname = 'ENDDA'
                            style     = cl_gui_alv_grid=>mc_style_enabled )
                 INTO TABLE ls_main-t_styl .
            INSERT VALUE #( fieldname = 'FPBEG'
                            style     = cl_gui_alv_grid=>mc_style_enabled )
                 INTO TABLE ls_main-t_styl .
            INSERT VALUE #( fieldname = 'FPEND'
                            style     = cl_gui_alv_grid=>mc_style_enabled )
                 INTO TABLE ls_main-t_styl .

            go_alv->record_check = 'C'." DEğişiklik kontrolü için
            ls_main-oprtn = 'N'.
            APPEND ls_main TO go_alv->gt_main.
            EXIT.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
        CLEAR go_alv->record_check. " DEğişiklik kontrolü için
        CLEAR ls_main.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD check_grpid.
    DATA : ls_t001 TYPE /dsl/hr80_t001.
    DATA : ls_t002 TYPE /dsl/hr80_t002.
    DATA : l_t001   TYPE t001  ,
           l_t500p  TYPE t500p ,
           l_t001p  TYPE t001p ,
           l_t549a  TYPE t549a ,
           l_t500l  TYPE t500l .

    SELECT SINGLE * FROM t500l INTO l_t500l
      WHERE molga = s_t001-molga.
    IF sy-subrc NE 0.
      RAISE no_molga.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM t001 INTO l_t001
      WHERE bukrs = s_t002-bukrs.
    IF sy-subrc NE 0.
      RAISE no_bukrs.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM t500p INTO l_t500p
      WHERE persa = s_t002-werks
        AND molga = s_t001-molga.
    IF sy-subrc NE 0.
      RAISE no_werks.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM t001p INTO l_t001p
      WHERE werks = s_t002-werks
        AND btrtl = s_t002-btrtl
        AND molga = s_t001-molga.
    IF sy-subrc NE 0.
      RAISE no_btrtl.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM t549a INTO l_t549a
      WHERE abkrs = s_t002-abkrs.
    IF sy-subrc NE 0.
      RAISE no_abkrs.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM /dsl/hr80_t002
        INTO ls_t002
      WHERE grpid  EQ s_t001-grpid
        AND bukrs  EQ s_t002-bukrs
        AND werks  EQ s_t002-werks
        AND btrtl  EQ s_t002-btrtl
        AND abkrs  EQ s_t002-abkrs
        AND molga  EQ s_t001-molga.
    IF sy-subrc EQ 0.
      RAISE record_available.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD check_vrsid.
    DATA : l_dd07t TYPE dd07t.

    SELECT SINGLE * FROM dd07t INTO l_dd07t
      WHERE domname    = '/DSL/HR80_STATU'
        AND ddlanguage = sy-langu
        AND domvalue_l = s_t003-statu.
    IF sy-subrc NE 0 .
      RAISE no_statu.RETURN.
    ENDIF.

    IF s_t003-begda(4) NE s_t003-gjahr OR
       s_t003-endda(4) NE s_t003-gjahr OR
       s_t003-fpbeg(4) NE s_t003-gjahr OR
       s_t003-fpend(4) NE s_t003-gjahr .
      RAISE ne_year.RETURN.
    ENDIF.

    IF s_t003-begda IS INITIAL .
      RAISE no_begda.RETURN.
    ELSEIF s_t003-endda IS INITIAL .
      RAISE no_endda.RETURN.
    ELSEIF s_t003-begda GT s_t003-endda.
      RAISE big_begda.RETURN.
    ELSEIF s_t003-fpbeg IS INITIAL .
      RAISE no_fpbeg.RETURN.
    ELSEIF s_t003-fpend IS INITIAL .
      RAISE no_fpend.RETURN.
    ELSEIF s_t003-fpbeg GT s_t003-fpend.
      RAISE big_fpbeg.RETURN.
    ENDIF.

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
  ENDMETHOD.

  METHOD create_fcat.
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
    modify_target_value( fname = 'MARK'    targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value( fname = 'MANDT'   targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value( fname = 'MOLGA'   targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value( fname = 'GRPID'   targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value( fname = 'GRPID_T' targt  = 'NO_OUT' zvalue = 'X' ).

    modify_target_value( fname = 'GJAHR'   targt  = 'TEXT'   zvalue = TEXT-gja ).
    modify_target_value( fname = 'GRPID'   targt  = 'TEXT'   zvalue = TEXT-grp ).
    modify_target_value( fname = 'GRPID_T' targt  = 'TEXT'   zvalue = TEXT-grp ).
    modify_target_value( fname = 'VRSID'   targt  = 'TEXT'   zvalue = TEXT-vrs ).
*    modify_target_value( fname = 'MOLGA'   targt  = 'TEXT'   zvalue = TEXT-mol ).
    modify_target_value( fname = 'VRSID_T' targt  = 'TEXT'   zvalue = TEXT-vrs ).
*    modify_target_value( fname = 'STATU'   targt  = 'TEXT'   zvalue = TEXT-sta ).
    modify_target_value( fname = 'BEGDA'   targt  = 'TEXT'   zvalue = TEXT-beg ).
    modify_target_value( fname = 'ENDDA'   targt  = 'TEXT'   zvalue = TEXT-end ).
    modify_target_value( fname = 'FPBEG'   targt  = 'TEXT'   zvalue = TEXT-fpb ).
    modify_target_value( fname = 'FPEND'   targt  = 'TEXT'   zvalue = TEXT-fpe ).
    modify_target_value( fname = 'UNAME'   targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value( fname = 'DATUM'   targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value( fname = 'UZEIT'   targt  = 'NO_OUT' zvalue = 'X' ).
    modify_target_value( fname = 'OPRTN'   targt  = 'NO_OUT' zvalue = 'X' ).


    modify_target_value( fname = 'VRSID_T' targt = 'EDIT' zvalue = 'X' ).
    modify_target_value( fname = 'STATU'   targt = 'EDIT' zvalue = 'X' ).
    modify_target_value( fname = 'BEGDA'   targt = 'EDIT' zvalue = 'X' ).
    modify_target_value( fname = 'ENDDA'   targt = 'EDIT' zvalue = 'X' ).
    modify_target_value( fname = 'FPBEG'   targt = 'EDIT' zvalue = 'X' ).
    modify_target_value( fname = 'FPEND'   targt = 'EDIT' zvalue = 'X' ).


  ENDMETHOD.

  METHOD modify_target_value.
    READ TABLE mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>)
        WITH KEY fieldname  = fname.
    IF sy-subrc EQ 0 .
      CASE targt .
        WHEN 'TEXT'.
          modify_target_value( fname  = fname targt = 'SCRTEXT_L' zvalue = zvalue ).
          modify_target_value( fname  = fname targt = 'SCRTEXT_M' zvalue = zvalue ).
          modify_target_value( fname  = fname targt = 'SCRTEXT_S' zvalue = zvalue ).
          modify_target_value( fname  = fname targt = 'REPTEXT'   zvalue = zvalue ).
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
    DATA :  lt_rows	TYPE lvc_t_row,
            lt_rown	TYPE lvc_t_roid.
    DATA : answer.



    CASE e_ucomm.
      WHEN 'INSERT'.
        add_new_record( ).

      WHEN 'MODFIY'.
        check_changed_data( ).
        IF record_check EQ 'C'." DEğişiklik kontrolü için
          go_main->save_main( ).
        ENDIF.

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
                    text      = TEXT-ins
                    )
            INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( butn_type = 0
                    function  = 'DELETE'
                    icon      = icon_delete_row
                    disabled  = space
                    text      = TEXT-del
                    )
           INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( butn_type = 0
                    function  = 'MODFIY'
                    icon      = icon_system_save
                    quickinfo = TEXT-t01
                    disabled  = space
                    text      = TEXT-mod
                    )
            INTO TABLE e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_data_changed.
    DATA : s_t003 TYPE /dsl/hr80_t003.
    DATA : lmc_de TYPE raw4 .

    LOOP AT er_data_changed->mt_good_cells ASSIGNING
          FIELD-SYMBOL(<ls_cells>).
      READ TABLE me->gt_main ASSIGNING FIELD-SYMBOL(<ls_report>)
            INDEX <ls_cells>-row_id.
      CHECK sy-subrc = 0.
      " hata varsa değişiklik var mesajı gelmemesi için
      IF record_check NE 'E'.
        record_check = 'C'. " DEğişiklik kontrolü için
        ASSIGN COMPONENT <ls_cells>-fieldname OF STRUCTURE <ls_report>
            TO FIELD-SYMBOL(<fs>).
        <fs> = <ls_cells>-value.
        <ls_report>-oprtn = 'C'.
      ENDIF.

      <ls_report>-uname = sy-uname.
      <ls_report>-datum = sy-datum.
      <ls_report>-uzeit = sy-uzeit.

      MOVE-CORRESPONDING <ls_report> TO s_t003.
      go_main->check_vrsid(
        EXPORTING
          s_t003    = s_t003
        EXCEPTIONS
          no_statu  = 007
          no_begda  = 008
          no_endda  = 009
          big_begda = 010
          no_fpbeg  = 011
          no_fpend  = 012
          big_fpbeg = 013
          ne_year   = 023
      ).
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 007.MESSAGE i007 DISPLAY LIKE 'E'.
          WHEN 008.MESSAGE i008 DISPLAY LIKE 'E'.
          WHEN 009.MESSAGE i009 DISPLAY LIKE 'E'.
          WHEN 010.MESSAGE i010 DISPLAY LIKE 'E'.
          WHEN 011.MESSAGE i011 DISPLAY LIKE 'E'.
          WHEN 012.MESSAGE i012 DISPLAY LIKE 'E'.
          WHEN 013.MESSAGE i013 DISPLAY LIKE 'E'.
          WHEN 023.MESSAGE i023 DISPLAY LIKE 'E'.
          WHEN OTHERS.
        ENDCASE.
        record_check = 'E'. " DEğişiklik kontrolü için
      ELSE.

*  1  Planlanıyor
*  2  Hesaplama yapılıyor
*  3  Tamamlandı
*  4  BPC aktarıldı
          CASE <ls_report>-statu.
            WHEN '1' OR '0'.
                        lmc_de = cl_gui_alv_grid=>mc_style_enabled.
            WHEN OTHERS.lmc_de = cl_gui_alv_grid=>mc_style_disabled.
          ENDCASE.
          REFRESH <ls_report>-t_styl.
          INSERT VALUE #( fieldname = 'VRSID_T'
                        style     = lmc_de )
               INTO TABLE <ls_report>-t_styl .
          INSERT VALUE #( fieldname = 'STATU'
                        style     = cl_gui_alv_grid=>mc_style_enabled )
               INTO TABLE <ls_report>-t_styl .
          INSERT VALUE #( fieldname = 'BEGDA'
                          style     = lmc_de )
               INTO TABLE <ls_report>-t_styl .
          INSERT VALUE #( fieldname = 'ENDDA'
                          style     = lmc_de )
               INTO TABLE <ls_report>-t_styl .
          INSERT VALUE #( fieldname = 'FPBEG'
                          style     = lmc_de )
               INTO TABLE <ls_report>-t_styl .
          INSERT VALUE #( fieldname = 'FPEND'
                          style     = lmc_de )
               INTO TABLE <ls_report>-t_styl .
      ENDIF.
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
    DATA : BEGIN OF ls_header,
            bukrs	TYPE /dsl/hr80_t002-bukrs,
            butxt	TYPE t001-butxt,
            werks	TYPE /dsl/hr80_t002-werks,
            name1	TYPE t500p-name1,
            btrtl	TYPE /dsl/hr80_t002-btrtl,
            btext	TYPE t001p-btext,
            abkrs	TYPE /dsl/hr80_t002-abkrs,
            atext	TYPE t549t-atext,
           END OF ls_header.


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

    SELECT SINGLE * FROM /dsl/hr80_t001 INTO ls_t001
          WHERE molga EQ p_molga
            AND grpid IN s_grpid .


    SELECT SINGLE
        t1~bukrs
        t2~butxt
        t1~werks
        t3~name1
        t1~btrtl
        t4~btext
        t1~abkrs
        t5~atext
              FROM /dsl/hr80_t002 AS t1
        INNER JOIN t001           AS t2
              ON    t2~bukrs EQ t1~bukrs
        INNER JOIN t500p          AS t3
              ON    t3~persa EQ t1~werks
        INNER JOIN t001p          AS t4
              ON    t4~werks EQ t1~werks
                AND t4~btrtl EQ t1~btrtl
        INNER JOIN t549t          AS t5
              ON    t5~abkrs EQ t1~abkrs
                AND t5~sprsl EQ sy-langu
          INTO CORRESPONDING
              FIELDS OF ls_header
          WHERE t1~molga EQ p_molga
            AND t1~grpid EQ ls_t001-grpid .


    mo_top_page = mo_splitter->get_container(
      row    = 1
      column = 1 ).

    mo_document = NEW #( style = 'ALV_GRID' ).

    IF mo_document IS BOUND.

      CALL METHOD mo_document->add_gap
        EXPORTING
          width = 120.

*      CALL METHOD mo_document->add_picture
*        EXPORTING
*          picture_id = 'ENJOYSAP_LOGO'.

      CALL METHOD mo_document->new_line.

      mo_document->add_text( text = CONV #( sy-title )
          sap_style    = cl_dd_area=>heading
          sap_fontsize = cl_dd_area=>large
                              ).
      mo_document->new_line( 1  ).
      SHIFT ls_t001-grpid LEFT DELETING LEADING '0'.
      CONCATENATE ls_t001-grpid
                  ls_t001-grpid_t
            INTO ls_t001-grpid_t SEPARATED BY '-'.
      add_line : TEXT-mol  ls_t001-molga.
      add_line : TEXT-bgr  ls_t001-grpid_t.
      DATA(lv_bukrs) = ls_header-bukrs && ' - ' && ls_header-butxt .
      DATA(lv_name1) = ls_header-werks && ' - ' && ls_header-name1 .
      DATA(lv_btext) = ls_header-btrtl && ' - ' && ls_header-btext .
      DATA(lv_atext) = ls_header-abkrs && ' - ' && ls_header-atext .
      add_line : 'Şirket kodu:'  lv_bukrs.
      add_line : 'Personel alanı :'  lv_name1.
      add_line : 'Personel alt alanı :'  lv_btext.
      add_line : 'Bordro alt birimi :'  lv_atext.
      mo_document->display_document( parent = mo_top_page ).
    ENDIF.
  ENDMETHOD.

  METHOD add_new_record.
    go_main->create_vrsid( ).
  ENDMETHOD.


ENDCLASS.                 " lcl_alv IMPLEMENTATION
