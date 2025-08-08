*&---------------------------------------------------------------------*
*& Include          ZNCPRH_P006_I002
*&---------------------------------------------------------------------*

CLASS alv_class DEFINITION.
  PUBLIC SECTION.
    METHODS : get_filename,
      at_sel_scr,
      end_of_sel,
      get_data,
      call_alv IMPORTING iv_table TYPE STANDARD TABLE,
      fill_fcat IMPORTING iv_table    TYPE STANDARD TABLE
                          iv_fcatname TYPE text20,
      top_of_header ,
      modify_fcat,
      download_template,
      alv IMPORTING iv_tabname     TYPE any
                    iv_fcatname    TYPE any
                    iv_layname     TYPE any
                    iv_guiname     TYPE any
                    iv_commandname TYPE any,
      batch_ise_alim,
      batch_isten_cikis,
      hr_maintain_masterdata IMPORTING
                                       iv_pernr    TYPE persno OPTIONAL
                                       iv_massn    TYPE massn
                                       iv_actio    TYPE actio
                                       iv_tclass   TYPE tclas
                                       iv_begda    TYPE begda
                                       iv_endda    TYPE endda
                             CHANGING  ch_return   TYPE bapireturn
                                       ch_return1  TYPE bapireturn1
                                       ch_hrreturn TYPE hrhrmm_msg
                                       ch_prop_tab TYPE STANDARD TABLE.
*      hr_enque


ENDCLASS.

CLASS alv_class IMPLEMENTATION.
  METHOD at_sel_scr.
    IF sy-ucomm EQ gc_fc03.
      gr_alv->download_template( ).
    ENDIF.
  ENDMETHOD.
  METHOD get_filename.
    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        field_name = 'P_FILE'
      IMPORTING
        file_name  = p_file.
  ENDMETHOD.
  METHOD get_data.

    DATA : ld_index   TYPE i,
           lv_length  TYPE i,
           lv_type    TYPE c,
           lv_string1 TYPE string,
           lv_string2 TYPE string,
           lv_string3 TYPE string,
           lv_fname   TYPE rlgrap-filename,
           lt_data    LIKE TABLE OF gt_ise_alim.

    DATA : p_scol TYPE i VALUE '1',
           p_srow TYPE i VALUE '1',
           p_ecol TYPE i VALUE '256',
           p_erow TYPE i VALUE '65536'.

    DATA : lv_decimal TYPE p LENGTH 10 DECIMALS 2,
           lt_intern  TYPE TABLE OF kcde_cells.

    FIELD-SYMBOLS : <fs_data> LIKE gs_ise_alim_excel.
    lv_fname = p_file.
*    CASE abap_true.
*      WHEN rb1."işe alım
    CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
      EXPORTING
        filename                = lv_fname
        i_begin_col             = p_scol
        i_begin_row             = p_srow
        i_end_col               = p_ecol
        i_end_row               = p_erow
      TABLES
        intern                  = lt_intern
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.

    IF lt_intern[] IS INITIAL.
      CHECK 1 EQ 2.
    ELSE.
      SORT lt_intern BY row col.
      DELETE lt_intern WHERE row EQ 1.

      LOOP AT lt_intern ASSIGNING FIELD-SYMBOL(<f1>).
        MOVE <f1>-col TO ld_index.
        IF rb1 EQ 'X'.
          ASSIGN COMPONENT ld_index OF STRUCTURE gs_ise_alim_excel TO <fs>.
        ELSE.
          ASSIGN COMPONENT ld_index OF STRUCTURE gs_isten_cikis_excel TO <fs>.
        ENDIF.

        DESCRIBE FIELD <fs> TYPE lv_type.
        CASE lv_type.
          WHEN 'D'.
            IF <f1>-value IS NOT INITIAL OR
               <f1>-value+0(8) NE '0000000'.


              SPLIT <f1>-value AT '.' INTO lv_string1 lv_string2 lv_string3.

              lv_length = strlen( lv_string1 ).
              IF lv_length EQ 1.

                CONCATENATE '0' lv_string1  INTO lv_string1.

              ENDIF.
              CLEAR lv_length.

              lv_length = strlen( lv_string2 ).
              IF lv_length EQ 1.

                CONCATENATE '0' lv_string2  INTO lv_string2.

              ENDIF.

              CONCATENATE lv_string1 lv_string2 lv_string3 INTO <f1>-value.


              CONCATENATE <f1>-value+4(4)   <f1>-value+2(2)
                           <f1>-value+0(2) INTO <fs>.
            ENDIF.

          WHEN 'P'.
            CONDENSE <f1>-value.
            CALL FUNCTION 'MOVE_CHAR_TO_NUM'
              EXPORTING
                chr             = <f1>-value
              IMPORTING
                num             = lv_decimal
              EXCEPTIONS
                convt_no_number = 1
                convt_overflow  = 2
                OTHERS          = 3.
            MOVE lv_decimal TO <fs>.
*                CONDENSE <fs>.
          WHEN OTHERS.

            IF rb1 EQ 'X'.
              IF ld_index EQ 19." Cinsiyet
                IF <f1>-value EQ 'Erkek'.
                  <fs> = '1'.
                ELSEIF <f1>-value EQ 'Kadın'.
                  <fs> = '2'.
                ELSE.
                  CLEAR <fs>.
                ENDIF.
              ELSEIF ld_index EQ 24."medeni hal
                IF <f1>-value EQ 'Bekâr' OR <f1>-value EQ 'Bekar'.
                  <fs> = '0'.
                ELSEIF <f1>-value EQ 'Evli'.
                  <fs> = '1'.
                ELSEIF <f1>-value EQ 'Dul'.
                  <fs> = '2'.
                ELSE.
                  CLEAR <fs>.
                ENDIF.
              ELSE.
                MOVE <f1>-value TO <fs>.
              ENDIF.
            ELSE.
              MOVE <f1>-value TO <fs>.
            ENDIF.

        ENDCASE.
        AT END OF row.
          IF rb1 EQ 'X'.
            APPEND gs_ise_alim_excel TO gt_ise_alim_excel.
            CLEAR gs_ise_alim_excel.
          ELSE.
            APPEND gs_isten_cikis_excel TO gt_isten_cikis_excel.
            CLEAR gs_isten_cikis_excel.
          ENDIF.
        ENDAT.
      ENDLOOP.
    ENDIF.

    MOVE-CORRESPONDING gt_ise_alim_excel    TO gt_ise_alim.
    MOVE-CORRESPONDING gt_isten_cikis_excel TO gt_isten_cikis.

    SELECT * FROM t530t INTO TABLE @DATA(lt_t530t)
      WHERE sprsl EQ @sy-langu
      AND   massn EQ '06'.

    LOOP AT gt_isten_cikis ASSIGNING FIELD-SYMBOL(<fs_cikis>).

      IF <fs_cikis>-massg IS NOT INITIAL.
        READ TABLE lt_t530t INTO DATA(ls_t530t)
                            WITH KEY massg = <fs_cikis>-massg.
        IF sy-subrc EQ 0.
          <fs_cikis>-mgtxt = ls_t530t-mgtxt.
        ENDIF.
      ENDIF.

    ENDLOOP.

*
*      WHEN rb2."işten çıkarma
*    ENDCASE.

  ENDMETHOD.
  METHOD call_alv.
    fill_fcat(
      EXPORTING
        iv_table    = iv_table
        iv_fcatname = 'GT_FCAT[]'
    ).

    top_of_header( ).

    gs_layout-zebra             = 'X'    .
*    gs_layout-colwidth_optimize = 'X'    .
*       gs_layout-coltab_fieldname  = 'COLOR'.
    gs_layout-box_fieldname     = 'MARK' .

    modify_fcat( ).
  ENDMETHOD.
  METHOD top_of_header.
    DATA : ls_top LIKE LINE OF gt_top.
    ls_top-typ  = 'H'                          .
    IF rb1 EQ 'X'.
      ls_top-info = 'İşe Alım Aktarımı'   .
    ELSE.
      ls_top-info = 'İşten Çıkış Aktarımı'.
    ENDIF.
    APPEND ls_top TO gt_top                    .
    CLEAR ls_top                               .
    ls_top-typ  = 'S'                          .
    ls_top-key  = 'Kullanıcı'                  .
    ls_top-info = sy-uname                     .
    APPEND ls_top TO gt_top                    .
    CLEAR ls_top                               .
    ls_top-typ  = 'S'                          .
    ls_top-key  = 'Tarih'                      .
    WRITE sy-datum TO ls_top-info              .
    APPEND ls_top TO gt_top                    .
    CLEAR ls_top                               .
    ls_top-typ  = 'S'                          .
    ls_top-key  = 'Saat'                       .
    WRITE sy-uzeit TO ls_top-info              .
    APPEND ls_top TO gt_top                    .
    CLEAR ls_top.
  ENDMETHOD.
  METHOD fill_fcat.
    ASSIGN :   (iv_fcatname)   TO <fcatname>.
    CLEAR : gt_fcat, gt_fcat[].
    DATA : lv_tabname TYPE tabname.
    IF rb1 EQ 'X'.
      lv_tabname = '/DSL/HR80_S_ALIM'.
    ELSE.
      lv_tabname = '/DSL/HR80_S_CIKIS'.
    ENDIF.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = sy-repid
*       i_internal_tabname     = lv_tabname
        i_structure_name       = lv_tabname
        i_inclname             = sy-repid
        i_client_never_display = 'X'
        i_bypassing_buffer     = 'X'
      CHANGING
        ct_fieldcat            = <fcatname>
      EXCEPTIONS
        OTHERS                 = 3.
    IF sy-subrc NE 0.     EXIT.   ENDIF.

  ENDMETHOD.
  METHOD modify_fcat.
    LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      <fs_fcat>-outputlen = 15.
      CASE <fs_fcat>-fieldname.
        WHEN 'MARK' .
          <fs_fcat>-no_out = 'X'.
        WHEN 'ILKOD'.
          <fs_fcat>-seltext_s = <fs_fcat>-seltext_m =
          <fs_fcat>-reptext_ddic = <fs_fcat>-seltext_l =  'İlkodu'.
        WHEN 'ICON'.
          <fs_fcat>-icon = 'X'.
          <fs_fcat>-seltext_s = <fs_fcat>-seltext_m =
          <fs_fcat>-reptext_ddic = <fs_fcat>-seltext_l =  'Ikon'.
        WHEN 'DAT01' OR 'DAT02'.
          <fs_fcat>-seltext_s = <fs_fcat>-seltext_m =
          <fs_fcat>-reptext_ddic = <fs_fcat>-seltext_l =  'Tarih Türü'.

        WHEN OTHERS.
          <fs_fcat>-seltext_s = <fs_fcat>-seltext_m =
          <fs_fcat>-reptext_ddic = <fs_fcat>-seltext_l.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD download_template.
    DATA: ld_filename TYPE string,
          ld_path     TYPE string,
          ld_fullpath TYPE string,
          ld_result   TYPE i.
    DATA : lv_def_name TYPE string.
    DATA : lt_signat TYPE TABLE OF bapisignat.


    IF rb1 EQ 'X'.
      me->call_alv( iv_table = gt_ise_alim[] ).
    ELSE.
      me->call_alv( iv_table = gt_isten_cikis[] ).
    ENDIF.

    REFRESH : file[].

    LOOP AT gt_fcat INTO gs_fcat WHERE fieldname NE 'MARK'
                                   AND fieldname NE 'ICON'
                                   AND fieldname NE 'MGTXT'
                                   AND fieldname NE 'MESSAGE'
                                   AND no_out NE 'X'.
      IF sy-tabix EQ 1.
        file-field = gs_fcat-seltext_l.
      ELSE.
        CONCATENATE file-field
                    gs_fcat-seltext_l
                 INTO file-field SEPARATED BY con_tab .
      ENDIF.
    ENDLOOP.
    APPEND file TO file.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
*       window_title      = ' '
        default_extension = 'XLS'
        default_file_name = lv_def_name
        initial_directory = 'C:\'
      CHANGING
        filename          = ld_filename
        path              = ld_path
        fullpath          = ld_fullpath
        user_action       = ld_result.
    CHECK ld_result EQ '0'.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = ld_fullpath
        filetype              = 'ASC'
*       APPEND                = 'X'
        write_field_separator = 'X'
        confirm_overwrite     = 'X'
      TABLES
        data_tab              = file[]  "need to declare and
      EXCEPTIONS
        file_open_error       = 1
        file_write_error      = 2
        OTHERS                = 3.

  ENDMETHOD.
  METHOD alv.
    ASSIGN : (iv_tabname)  TO <tabname>,
         (iv_fcatname) TO <fcatname>,
         (iv_layname)  TO <layname>.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'GUI'
        i_callback_top_of_page   = 'TOP_OF_PAGE'
        i_callback_user_command  = iv_commandname
        is_layout                = <layname>
        it_fieldcat              = <fcatname>
      TABLES
        t_outtab                 = <tabname>
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
    ENDIF.
    FREE  : <fcatname>.
    FREE  : <tabname>.
  ENDMETHOD.
  METHOD batch_ise_alim.

    DATA : l_return  TYPE bapireturn,
           l_return1 TYPE bapireturn1,
           hr_return TYPE hrhrmm_msg,
           ls_pspar  TYPE pspar.

    DEFINE fill_proposed_value .
*
      CLEAR ls_proposed_values      .
      ls_proposed_values-infty = &1 .
      ls_proposed_values-fname = &2 .
      ls_proposed_values-fval  = &3 .
      CONDENSE ls_proposed_values-fval.
      APPEND ls_proposed_values TO lt_proposed_values     .

    END-OF-DEFINITION .
    DATA  BEGIN OF ls_proposed_values      .                "XDPK139847
    INCLUDE TYPE pprop     .                                "XDPK139847
    DATA  END OF ls_proposed_values            .
    DATA lt_proposed_values LIKE TABLE OF ls_proposed_values.

    LOOP AT gt_ise_alim ASSIGNING FIELD-SYMBOL(<fs_data>)
         WHERE mark EQ 'X'.
      REFRESH lt_proposed_values.
      fill_proposed_value :
        '0000' 'P0000-BEGDA' <fs_data>-begda,
        '0000' 'P0000-MASSN' '01'          ,
        '0000' 'P0000-ENDDA' '99991231'   ,
        '0000' 'PSPAR-WERKS' <fs_data>-werks ,
        '0000' 'PSPAR-PERSG' <fs_data>-persg ,
        '0000' 'PSPAR-PERSK' <fs_data>-persk ,
        '0000' 'PSPAR-PLANS' <fs_data>-plans ,

        '0001' 'P0001-ORGEH' <fs_data>-orgeh,
        '0001' 'P0001-STELL' <fs_data>-stell,
        '0001' 'P0001-BUKRS' <fs_data>-bukrs,
        '0001' 'P0001-WERKS' <fs_data>-werks,
        '0001' 'P0001-BTRTL' <fs_data>-btrtl,
        '0001' 'P0001-GSBER' <fs_data>-gsber,
        '0001' 'P0001-ABKRS' <fs_data>-abkrs,
*-
        '0002' 'P0002-GESCH' <fs_data>-gesch,
        '0002' 'P0002-VORNA' <fs_data>-vorna,
        '0002' 'P0002-NACHN' <fs_data>-nachn,
        '0002' 'P0002-GBORT' <fs_data>-gbort,
        '0002' 'P0002-GBDAT' <fs_data>-gbdat,
        '0002' 'P0002-FAMST' <fs_data>-famst,
        '0002' 'P0002-NATIO' 'TR',
        '0002' 'P0002-ANRED' <fs_data>-gesch.


*        IF <fs_data>-schkz IS NOT INITIAL .
*           fill_proposed_value :
*            '0007' 'P0007-SCHKZ' <fs_data>-schkz   ,
*            '0007' 'P0007-ZTERF' '0'            .
*        ENDIF.
*
*       IF <fs_data>-trfar IS NOT INITIAL AND
*          <fs_data>-trfgb IS NOT INITIAL AND
*          <fs_data>-lga01 IS NOT INITIAL AND
*          <fs_data>-bet01 IS NOT INITIAL   .
*        fill_proposed_value :
*          '0008' 'P0008-TRFAR' <fs_data>-trfar,
*          '0008' 'P0008-TRFGB' <fs_data>-trfgb,
*          '0008' 'P0008-LGA01' <fs_data>-lga01,
*          '0008' 'P0008-BET01' <fs_data>-bet01,
*          '0008' 'P0008-TRFGR' '1'.
*       ENDIF.
*
**-
*      IF <fs_data>-dat01 IS NOT INITIAL .
*      fill_proposed_value :
*        '0041' 'P0041-DAR01' '01',
*        '0041' 'P0041-DAT01' <fs_data>-dat01 .
*      ENDIF.
*
*      IF <fs_data>-dat01 IS NOT INITIAL .
*        fill_proposed_value :
*          '0041' 'P0041-DAR02' '02',
*          '0041' 'P0041-DAT02' <fs_data>-dat02 .
*      ENDIF.
**-
**-
*      IF <fs_data>-bkodu IS NOT INITIAL AND
*         <fs_data>-sskno IS NOT INITIAL .
*        fill_proposed_value :
*          '0769' 'P0769-BKODU' <fs_data>-bkodu,
*          '0769' 'P0769-SSKNO' <fs_data>-sskno.
*      ENDIF.
*
**-
*       IF <fs_data>-merni IS NOT INITIAL .
*        fill_proposed_value :
*          '0770' 'P0770-ICTYP' '01',
*          '0770' 'P0770-MERNI' <fs_data>-merni.
*       ENDIF.
*
**-
*       IF <fs_data>-mslks IS NOT INITIAL AND
*          <fs_data>-cttyp IS NOT INITIAL .
*        fill_proposed_value :
*          '0771' 'P0771-MSLKS' <fs_data>-mslks,
*          '0771' 'P0771-CTTYP' <fs_data>-cttyp .
*       ENDIF.

      hr_maintain_masterdata(
        EXPORTING

*            iv_pernr    =
          iv_massn    = '01'
          iv_actio    = 'INS'
          iv_tclass   = 'A'
          iv_begda    = <fs_data>-begda
          iv_endda    = '99991231'
        CHANGING
          ch_return   = l_return
          ch_return1  = l_return1
          ch_hrreturn = hr_return
          ch_prop_tab = lt_proposed_values
      ).

      IF l_return IS INITIAL.
        CALL FUNCTION 'HR_INFOTYPE_GETBUFFER'
          IMPORTING
            es_pspar = ls_pspar.

        <fs_data>-message = 'Kayıt Başarılı! Personel No : '
                            && | | && ls_pspar-pernr.
        <fs_data>-icon = icon_green_light.
      ELSE.
        <fs_data>-icon = icon_red_light.
        <fs_data>-message  = l_return-message.
      ENDIF.

      CLEAR :l_return,l_return1,hr_return,ls_pspar.
    ENDLOOP.


  ENDMETHOD.
  METHOD batch_isten_cikis.
    DATA : l_return  TYPE bapireturn,
           l_return1 TYPE bapireturn1,
           hr_return TYPE hrhrmm_msg,
           ls_pspar  TYPE pspar.

    DEFINE fill_proposed_value .
*
      CLEAR ls_proposed_values      .
      ls_proposed_values-infty = &1 .
      ls_proposed_values-fname = &2 .
      ls_proposed_values-fval  = &3 .
      CONDENSE ls_proposed_values-fval.
      APPEND ls_proposed_values TO lt_proposed_values     .

    END-OF-DEFINITION .
    DATA  BEGIN OF ls_proposed_values      .                "XDPK139847
    INCLUDE TYPE pprop     .                                "XDPK139847
    DATA  END OF ls_proposed_values            .
    DATA lt_proposed_values LIKE TABLE OF ls_proposed_values.

    LOOP AT gt_isten_cikis ASSIGNING FIELD-SYMBOL(<fs_data>)
                           WHERE mark EQ 'X'.
      REFRESH lt_proposed_values.
      fill_proposed_value :
        '0000' 'PSPAR-PERNR' <fs_data>-pernr ,
        '0000' 'P0000-BEGDA' <fs_data>-endda,
        '0000' 'P0000-MASSN' <fs_data>-massn           ,
        '0000' 'P0000-MASSG' <fs_data>-massg ,
        '0000' 'P0000-ENDDA' '99991231'    .

      hr_maintain_masterdata(
       EXPORTING
         iv_pernr    = <fs_data>-pernr
         iv_massn    = '06'
         iv_actio    = 'INS'
         iv_tclass   = 'A'
         iv_begda    = <fs_data>-endda
         iv_endda    = '99991231'
       CHANGING
         ch_return   = l_return
         ch_return1  = l_return1
         ch_hrreturn = hr_return
         ch_prop_tab = lt_proposed_values
     ).

      IF l_return IS INITIAL.
        <fs_data>-message = 'İşten Çıkış Başarılı ! '.
        <fs_data>-icon = icon_green_light.
      ELSE.
        <fs_data>-icon = icon_red_light.
        <fs_data>-message  = l_return-message.
      ENDIF.

      CLEAR :l_return,l_return1,hr_return,ls_pspar.
    ENDLOOP.
  ENDMETHOD.

  METHOD hr_maintain_masterdata.
    DATA : modified_keys TYPE TABLE OF pskey .
    DATA : dialog_mode VALUE '0'.
*    DATA : modified_keys LIKE pskey OCCURS 0 WITH HEADER LINE .
*
    CALL FUNCTION 'HR_MAINTAIN_MASTERDATA'
      EXPORTING
        pernr              = iv_pernr
        massn              = iv_massn
        actio              = iv_actio
        tclas              = iv_tclass
        begda              = iv_begda
        endda              = iv_endda
        dialog_mode        = dialog_mode
        no_existence_check = 'X'
      IMPORTING
        return             = ch_return
        return1            = ch_return1
        hr_return          = ch_hrreturn
      TABLES
        proposed_values    = ch_prop_tab
        modified_keys      = modified_keys.
  ENDMETHOD.
  METHOD end_of_sel.
    IF rb1 EQ 'X'.
      me->call_alv( iv_table = gt_ise_alim[] ).
      me->alv(
        EXPORTING
          iv_tabname     = 'GT_ISE_ALIM[]'
          iv_fcatname    = 'GT_FCAT[]'
          iv_layname     = 'GS_LAYOUT'
          iv_guiname     = 'GUI'
          iv_commandname = 'USER_COMMAND'
      ).
    ELSE.
      me->call_alv( iv_table = gt_isten_cikis[] ).
      me->alv(
       EXPORTING
         iv_tabname     = 'GT_ISTEN_CIKIS[]'
         iv_fcatname    = 'GT_FCAT[]'
         iv_layname     = 'GS_LAYOUT'
         iv_guiname     = 'GUI'
         iv_commandname = 'USER_COMMAND'
     ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
