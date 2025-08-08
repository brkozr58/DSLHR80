*----------------------------------------------------------------------*
***INCLUDE /DSL/HR80_P005_003.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_PARAMATERS
*&---------------------------------------------------------------------*
FORM check_paramaters  CHANGING cv_check.
  IF s_grpid[] IS INITIAL .
    MESSAGE i001 DISPLAY LIKE 'E'.
    cv_check = 'X'.
  ENDIF.

  IF s_vrsid[] IS INITIAL .
    MESSAGE i003 DISPLAY LIKE 'E'.
    cv_check = 'X'.
  ENDIF.

  CLEAR :go_alv->gs_t003.
  SELECT SINGLE * FROM /dsl/hr80_t003 INTO go_alv->gs_t003
    WHERE grpid IN s_grpid[]
      AND vrsid IN s_vrsid[]
*      AND gjahr IN s_gjahr[]
      AND statu IN s_statu[].
  IF sy-subrc NE 0 .
    MESSAGE i005 DISPLAY LIKE 'E'.
    cv_check = 'X'.
  ENDIF.

  IF s_gjahr[] IS INITIAL .
    s_gjahr = 'IEQ'.
    s_gjahr-low = go_alv->gs_t003-gjahr.
    APPEND s_gjahr.
    LOOP AT s_gjahr.  ENDLOOP.
  ENDIF.

  IF s_gjahr[] IS INITIAL .
    MESSAGE i027 DISPLAY LIKE 'E'.
    cv_check = 'X'.
  ENDIF.

  IF r_rd2 EQ 'X' . " Toplu aktarım
    IF p_file IS INITIAL .
      MESSAGE i000 DISPLAY LIKE 'E'
          WITH 'Aktarım dosyası seçiniz'.
      cv_check = 'X'.
    ENDIF..
  ENDIF.

  IF go_alv->gs_t003-statu EQ '0' OR go_alv->gs_t003-statu EQ '1'.
    go_alv->gmc_de = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    go_alv->gmc_de = cl_gui_alv_grid=>mc_style_disabled.
    IF r_rd2 EQ 'X' AND p_file IS NOT INITIAL .
      MESSAGE i000 DISPLAY LIKE 'E'
          WITH 'Bütçe düzenlemesi için versiyon durumu'
               '"Planlanıyor" olmalıdır. Sistem yöneticiniz ile'
               'iletişime geçiniz.'.
      cv_check = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM at_selection_screen .
  go_alv->create_fcat( ).

  CASE 'X'.
    WHEN r_rd1 . " Rapor
      sscrfields-functxt_01 = ''.

    WHEN r_rd2 . " Toplu aktarım
      sscrfields-functxt_01 = '@J2@Şablon İndir'.
    WHEN OTHERS.
  ENDCASE.

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      go_main->template_file( ).
    WHEN 'RD' OR space .
      LOOP AT SCREEN.
        CHECK screen-group1 EQ 'RD2'.
        CASE 'X'.
          WHEN r_rd1 . " Rapor
            screen-active = 0 .
          WHEN r_rd2 . " Toplu aktarım
            screen-active = 1 .
          WHEN OTHERS.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.

    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM get_data .

  SELECT * FROM /dsl/hr80_ddl001 INTO CORRESPONDING
              FIELDS OF TABLE go_alv->gt_main
    WHERE grpid       IN s_grpid[]
      AND vrsid       IN s_vrsid[]
      AND gjahr       IN s_gjahr[]
      AND molga       EQ p_molga
      AND pernr       IN s_pernr[]
      AND werks       IN s_werks[]
      AND btrtl       IN s_btrtl[]
      AND persk       IN s_persk[]
      AND persg       IN s_persg[]
      AND orgeh       IN s_orgeh[]
      AND plans       IN s_plans[]
      AND stell       IN s_stell[]
      AND pa1_begda   LE s_datum-low
      AND pa1_endda   GE s_datum-low
*      AND datum   IN s_datum[]
 .
  IF sy-subrc NE 0 .
    SELECT * FROM /dsl/hr80_ddl002 INTO CORRESPONDING
                FIELDS OF TABLE go_alv->gt_main
      WHERE grpid       IN s_grpid[]
        AND vrsid       IN s_vrsid[]
        AND gjahr       IN s_gjahr[]
        AND molga       EQ p_molga
        AND pernr       IN s_pernr[]
        AND werks       IN s_werks[]
        AND btrtl       IN s_btrtl[]
        AND persk       IN s_persk[]
        AND persg       IN s_persg[]
        AND orgeh       IN s_orgeh[]
        AND plans       IN s_plans[]
        AND stell       IN s_stell[]
        AND pa1_begda   LE s_datum-low
        AND pa1_endda   GE s_datum-low
      .
    go_alv->record_check = 'C'." DEğişiklik kontrolü için
  ENDIF.

  SORT go_alv->gt_main ASCENDING BY pernr pa1_begda.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ROW_RECORD
*&---------------------------------------------------------------------*
FORM row_record  USING mode TYPE char1
                       pt_rows TYPE lvc_t_row .
  CLEAR : /dsl/hr80_s005.
  CASE mode.
    WHEN 'N'.
      /dsl/hr80_s005-mode = mode.
      /dsl/hr80_s005-gjahr = s_gjahr-low.
      /dsl/hr80_s005-grpid = s_grpid-low.
      /dsl/hr80_s005-vrsid = s_vrsid-low.
      /dsl/hr80_s005-molga = p_molga.
      CONCATENATE /dsl/hr80_s005-gjahr 'Bütçe Personeli'
          INTO /dsl/hr80_s005-ename SEPARATED BY '_'.

    WHEN 'C' ."OR 'I'.
      READ TABLE pt_rows INTO DATA(ls_rows) INDEX 1 .
      READ TABLE go_alv->gt_main INTO DATA(ls_main) INDEX ls_rows-index.
      MOVE-CORRESPONDING ls_main TO /dsl/hr80_s005.
      /dsl/hr80_s005-begda = ls_main-pa1_begda.
      /dsl/hr80_s005-endda = ls_main-pa1_endda.
      /dsl/hr80_s005-zindex = ls_rows-index.
      /dsl/hr80_s005-mode = mode.

    WHEN 'I'.
      /dsl/hr80_s005-mode = mode.
      /dsl/hr80_s005-gjahr = s_gjahr-low.
      /dsl/hr80_s005-grpid = s_grpid-low.
      /dsl/hr80_s005-vrsid = s_vrsid-low.
      /dsl/hr80_s005-molga = p_molga.


    WHEN OTHERS.
  ENDCASE.
  CHECK /dsl/hr80_s005 IS NOT INITIAL .
  CALL SCREEN '2000' STARTING AT 5 5 .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_RECORD
*&---------------------------------------------------------------------*
FORM modify_record .
  DATA : answer.

  go_alv->check_changed_data( ).
  IF go_alv->record_check EQ 'C'." DEğişiklik kontrolü için
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
      CLEAR : go_alv->record_check.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_RECORD
*&---------------------------------------------------------------------*
FORM change_record USING mode.
  DATA lt_rows TYPE lvc_t_row.
  DATA lt_rown TYPE lvc_t_roid.
  DATA : answer.

  go_alv->mo_grid->get_selected_rows(
    IMPORTING
      et_index_rows = lt_rows
      et_row_no     = lt_rown
  ).

  PERFORM row_record USING mode lt_rows .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_RECORD
*&---------------------------------------------------------------------*
FORM delete_record .
  DATA lt_rows TYPE lvc_t_row.
  DATA lt_rown TYPE lvc_t_roid.
  DATA : lt_delete LIKE TABLE OF go_alv->gs_main.
  DATA : answer.

  go_alv->mo_grid->get_selected_rows(
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
      go_alv->record_check = 'C'." DEğişiklik kontrolü için
      LOOP AT lt_rown ASSIGNING FIELD-SYMBOL(<ls_rown>).
        go_alv->gt_main[ <ls_rown>-row_id ]-mark = abap_true.
      ENDLOOP.

      lt_delete = VALUE #( FOR ls IN  go_alv->gt_main
            WHERE ( mark = 'X' )  ( ls ) ).
      APPEND LINES OF lt_delete TO go_alv->gt_delete .

      DELETE go_alv->gt_main WHERE mark = abap_true.
    ENDIF.
  ELSE.
    MESSAGE s006 DISPLAY LIKE 'E' .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_MAIN
*&---------------------------------------------------------------------*
FORM save_main .

  DATA : ls_t010 TYPE /dsl/hr80_t010 .
  DATA : number TYPE numc10.


  LOOP AT go_alv->gt_delete INTO DATA(ls_delete).
    MOVE-CORRESPONDING ls_delete TO ls_t010.
    ls_t010-begda = ls_delete-pa1_begda.
    ls_t010-endda = ls_delete-pa1_endda.

    DELETE  /dsl/hr80_t010 FROM ls_t010.
  ENDLOOP.
  IF sy-subrc EQ 0 .
    MESSAGE s022 DISPLAY LIKE 'I' .
    CLEAR go_alv->record_check .
    REFRESH go_alv->gt_delete.
    COMMIT WORK AND WAIT .
  ENDIF.


  LOOP AT go_alv->gt_main ASSIGNING FIELD-SYMBOL(<fs_main>)
        WHERE oprtn = 'N' OR oprtn = 'C'.
    CLEAR ls_t010.

    IF <fs_main>-oprtn EQ 'N' AND <fs_main>-pernr GT 90000000.
      go_main->number_get_next(
        EXPORTING
          nrnr     = CONV #( TEXT-nrn )
        IMPORTING
          number   = number
        EXCEPTIONS
          no_range = 1
      ).

      IF sy-subrc <> 0.
        CHECK 1 = 2 .
      ENDIF.
      <fs_main>-pernr = number+2(8).
    ENDIF.

    MOVE-CORRESPONDING <fs_main> TO ls_t010.
    CLEAR : <fs_main>-oprtn.
    ls_t010-begda = <fs_main>-pa1_begda.
    ls_t010-endda = <fs_main>-pa1_endda.
    ls_t010-uname = sy-uname.
    ls_t010-datum = sy-datum.
    ls_t010-uzeit = sy-uzeit.
    CLEAR <fs_main>-oprtn.
    CLEAR <fs_main>-color.
    MODIFY /dsl/hr80_t010 FROM ls_t010.
  ENDLOOP.
  IF sy-subrc EQ 0 .
    MESSAGE s020 DISPLAY LIKE 'I' .
    CLEAR go_alv->record_check .
    COMMIT WORK AND WAIT .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FCAT
*&---------------------------------------------------------------------*
FORM modify_fcat .

  go_alv->modify_target_value(  fname  = 'MANDT'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'MARK'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'OPRTN'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'COLOR'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'STATU'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'GJAHR'    targt  = 'TEXT'   zvalue = TEXT-gja ).
  go_alv->modify_target_value(  fname  = 'GRPID'    targt  = 'TEXT'   zvalue = TEXT-grp ).
  go_alv->modify_target_value(  fname  = 'VRSID'    targt  = 'TEXT'   zvalue = TEXT-vrs ).

*  go_alv->modify_target_value(  fname  = 'MOLGA'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'GJAHR'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'GRPID'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'VRSID'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'GRPID_T'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'VRSID_T'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'BG_BEGDA'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'BG_ENDDA'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'STATU'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'T_STYL'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'WAERS'    targt  = 'NO_OUT' zvalue = 'X' ).

  go_alv->modify_target_value(  fname  = 'UNAME'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'DATUM'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'UZEIT'    targt  = 'NO_OUT' zvalue = 'X' ).

  go_alv->modify_target_value(  fname  = 'KOMOK'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'STAT2'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'ANSVH'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'ABKRS'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'MGART'    targt  = 'NO_OUT' zvalue = 'X' ).
  go_alv->modify_target_value(  fname  = 'PROZT'    targt  = 'NO_OUT' zvalue = 'X' ).

  go_alv->modify_target_value(  fname  = 'ENAME'    targt  = 'TEXT' zvalue = 'Adı Soyadı' ).
  go_alv->modify_target_value(  fname  = 'ORGEH_T'    targt  = 'TEXT' zvalue = 'Organizasyon birimi tanımı' ).
  go_alv->modify_target_value(  fname  = 'PLASN_T'    targt  = 'TEXT' zvalue = 'Pozisyon tanımı' ).
  go_alv->modify_target_value(  fname  = 'STELL_T'    targt  = 'TEXT' zvalue = 'İş tanımı' ).

  LOOP AT go_alv->mt_fcat INTO DATA(ls_fcat) .

    CASE ls_fcat-fieldname.
      WHEN 'PERNR' OR 'ENAME'.
        go_alv->modify_target_value(  fname  = ls_fcat-fieldname
                              targt  = 'KEY'
                              zvalue = 'X' ).
      WHEN 'WERKS_NEW' OR
           'BTRTL_NEW' OR
           'PERSG_NEW' OR
           'PERSK_NEW' OR
           'TRFAR_NEW' OR
           'TRFGB_NEW' OR
           'TRFGR_NEW' OR
           'TRFST_NEW' OR
           'LGA01_NEW' OR
           'SALRY_NEW' OR
           'RFPER'     OR
           'ORGEH'     OR
           'PLANS'     OR
           'STELL'     OR
           'KOKRS'     OR
           'KOSTL'
        .
        go_alv->modify_target_value(  fname  = ls_fcat-fieldname
                              targt  = 'EDIT'
                              zvalue = 'X' ).
    ENDCASE.
    go_alv->modify_target_value(  fname  = ls_fcat-fieldname
                          targt  = 'COLDDICTXT'
                          zvalue = 'L' ).

    go_alv->modify_target_value(  fname  = ls_fcat-fieldname
                          targt  = 'COL_OPT'
                          zvalue = 'X' ).
  ENDLOOP.
  SORT go_alv->mt_fcat ASCENDING BY col_pos.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM handle_data_changed
      USING  er_data_changed  TYPE REF TO cl_alv_changed_data_protocol
             e_onf4           TYPE  char01
             e_onf4_before    TYPE  char01
             e_onf4_after     TYPE  char01 .


  LOOP AT er_data_changed->mt_good_cells
          ASSIGNING FIELD-SYMBOL(<ls_cells>).
    READ TABLE go_alv->gt_main ASSIGNING FIELD-SYMBOL(<ls_main>)
            INDEX <ls_cells>-row_id.
    CHECK sy-subrc = 0.
    IF go_alv->record_check NE 'E'. " hata varsa değişiklik var mesajı gelmemesi için
      go_alv->record_check = 'C'. " DEğişiklik kontrolü için
      <ls_main>-oprtn = 'C'.
      ASSIGN COMPONENT <ls_cells>-fieldname OF STRUCTURE <ls_main>
          TO FIELD-SYMBOL(<fs>).
      <fs> = <ls_cells>-value.

      CASE <ls_cells>-fieldname.
        WHEN 'RFPER'.
          CHECK <fs> IS NOT INITIAL .
          PERFORM get_rfper_values USING <fs> CHANGING <ls_main> .

        WHEN OTHERS.
      ENDCASE.

      <ls_main>-color      = 'C310'.

    ENDIF.
  ENDLOOP.
  IF sy-subrc EQ 0.
    go_main->get_text( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_EXCEL
*&---------------------------------------------------------------------*
FORM get_excel  USING file_name TYPE any .

  DATA : l_file_name TYPE ibipparms-path.
  DATA : filename  TYPE  rlgrap-filename.

  FIELD-SYMBOLS: <fs> TYPE any.
  DATA : ld_index     TYPE sy-index.
  DATA : lv_fname     TYPE rlgrap-filename.
  DATA : p_scol  TYPE i VALUE '1',
         p_srow  TYPE i VALUE '1',
         p_ecol  TYPE i VALUE '256',
         p_erow  TYPE i VALUE '65536',
         lv_type TYPE c.
  DATA : lv_string1 TYPE string,
         lv_string2 TYPE string,
         lv_string3 TYPE string.
  DATA : lv_length TYPE i.

  DATA : lv_decimal TYPE p LENGTH 10 DECIMALS 2.

  DATA : lt_intern TYPE  TABLE OF kcde_cells .
  FIELD-SYMBOLS <f1> TYPE kcde_cells .
  DATA : lt_table TYPE TABLE OF /dsl/hr80_s007.
  DATA : ls_table TYPE /dsl/hr80_s007.

  IF p_file IS INITIAL .
    CALL FUNCTION 'F4_FILENAME'
      IMPORTING
        file_name = l_file_name.

    filename = l_file_name.
  ELSE.
    filename = file_name.
  ENDIF.


*Note: Alternative function module - 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      filename                = filename
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
  IF sy-subrc <> 0.
    FORMAT COLOR COL_BACKGROUND INTENSIFIED.
    WRITE:/ 'Dosya Yüklenirken Hata !'.
    EXIT.
  ENDIF.
  IF lt_intern[] IS INITIAL.
    CHECK 1 EQ 2.
  ELSE.
    SORT lt_intern BY row col.
    DELETE lt_intern WHERE row EQ 1.
  ENDIF.


  LOOP AT lt_intern ASSIGNING <f1>.
    MOVE : <f1>-col TO ld_index.
    UNASSIGN <fs> .
    ASSIGN COMPONENT ld_index OF STRUCTURE ls_table TO <fs> .
    DESCRIBE FIELD <fs> TYPE lv_type.

    CASE lv_type.
      WHEN 'D'.
        IF <f1>-value IS NOT INITIAL OR
           <f1>-value+0(8) NE '0000000'.
          SPLIT <f1>-value AT '.' INTO lv_string1 lv_string2 lv_string3.
          IF sy-subrc EQ 0 .
            lv_length = strlen( lv_string1 ).
            IF lv_length EQ 1.
              CONCATENATE '0' lv_string1  INTO lv_string1.
            ENDIF.
            CLEAR lv_length.

            lv_length = strlen( lv_string2 ).
            IF lv_length EQ 1.
              CONCATENATE '0' lv_string2  INTO lv_string2.
            ENDIF.
            CONCATENATE lv_string1 lv_string2 lv_string3
                  INTO <f1>-value .
            CONCATENATE <f1>-value+4(4)   <f1>-value+2(2)
                         <f1>-value+0(2) INTO <fs>.
          ELSE.
            <fs> = <f1>-value.
          ENDIF.

        ENDIF.
      WHEN 'P'.
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
      WHEN OTHERS.
        MOVE <f1>-value TO <fs>.
    ENDCASE.

    AT END OF row.
      APPEND ls_table TO lt_table.
      CLEAR ls_table.
    ENDAT.
  ENDLOOP.

  go_alv->gt_main = VALUE #( FOR ls_dyn IN lt_table
              (
        oprtn       = 'N'                 persg       = ls_dyn-persg
        molga       = p_molga             persg_new   = ls_dyn-persg_new
        grpid       = s_grpid-low         persk       = ls_dyn-persk
        vrsid       = s_vrsid-low         persk_new   = ls_dyn-persk_new
        rfper       = ls_dyn-rfper        ssgrp       = ls_dyn-ssgrp
        pernr       = ls_dyn-pernr        kanun       = ls_dyn-kanun
        ename       = ls_dyn-ename        trfar       = ls_dyn-trfar
        pa1_begda   = ls_dyn-pa1_begda    trfar_new   = ls_dyn-trfar_new
        pa1_endda   = ls_dyn-pa1_endda    trfgb       = ls_dyn-trfgb
        bukrs       = ls_dyn-bukrs        trfgb_new   = ls_dyn-trfgb_new
        orgeh       = ls_dyn-orgeh        trfgr       = ls_dyn-trfgr
        plans       = ls_dyn-plans        trfgr_new   = ls_dyn-trfgr_new
        stell       = ls_dyn-stell        trfst       = ls_dyn-trfst
        kokrs       = ls_dyn-kokrs        trfst_new   = ls_dyn-trfst_new
        kostl       = ls_dyn-kostl        lga01       = ls_dyn-lga01
        werks       = ls_dyn-werks        salry       = ls_dyn-salry
        werks_new   = ls_dyn-werks_new    lga01_new   = ls_dyn-lga01_new
        btrtl       = ls_dyn-btrtl        salry_new   = ls_dyn-salry_new
        btrtl_new   = ls_dyn-btrtl_new    massn       = ls_dyn-massn
                                          massg       = ls_dyn-massg
              )
            ).
  IF go_alv->gt_main[] IS NOT INITIAL ..
    go_alv->record_check = 'C'.
  ENDIF.
  PERFORM modify_editible .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TEMPLATE_FILE
*&---------------------------------------------------------------------*
FORM template_file .

  DATA : BEGIN OF ls_head,
       header(80),
     END OF ls_head,
     lt_head LIKE TABLE OF ls_head.
  DATA: ld_filename TYPE string,
        ld_path     TYPE string,
        ld_fullpath TYPE string,
        ld_result   TYPE i.
  FIELD-SYMBOLS <fs>          TYPE any .

  DATA : ls_data TYPE /dsl/hr80_s007 .
  DATA : lt_data TYPE TABLE OF /dsl/hr80_s007 .


  DEFINE add_head.
    ls_head-header = &1.
    APPEND ls_head TO lt_head.
    CLEAR  ls_head .
  END-OF-DEFINITION.

  " dosyanın oluşturulması
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
*     window_title      = ' '
      default_extension = 'XLS'
      default_file_name = 'Örnek_şablon.xls'
      initial_directory = 'C:\'
    CHANGING
      filename          = ld_filename
      path              = ld_path
      fullpath          = ld_fullpath
      user_action       = ld_result.

  IF sy-subrc <> 0.
    MESSAGE i000 DISPLAY LIKE 'E' WITH 'Dizin seçilemedi' .
  ELSE.
    DATA : lt_fcat      TYPE lvc_t_fcat.
    lt_fcat = go_alv->mt_fcat.
    DELETE lt_fcat WHERE no_out EQ 'X'.
    DELETE lt_fcat WHERE fieldname CP '*_T'.
    APPEND VALUE #( molga       = '47'
                    rfper       = ''
                    pernr       = '340'
                    ename       = 'Bütçe Personeli'
                    pa1_begda   = '20250101'
                    pa1_endda   = '99991231'
                    bukrs       = '2000'
                    orgeh       = '50000553'
                    plans       = '50000828'
                    stell       = '50000563'
                    kokrs       = '2000'
                    kostl       = ''
                    werks       = '2000'
                    werks_new   = ''
                    btrtl       = '2001'
                    btrtl_new   = ''
                    persg       = '2'
                    persg_new   = ''
                    persk       = '20'
                    persk_new   = ''
                    ssgrp       = '1'
                    kanun       = ''
                    trfar       = '47'
                    trfar_new   = '47'
                    trfgb       = '47'
                    trfgb_new   = '47'
                    trfgr       = ''
                    trfgr_new   = ''
                    trfst       = ''
                    trfst_new   = ''
                    lga01       = '1000'
                    salry       = '123000.00'
                    lga01_new   = '1000'
                    salry_new   = '234000.00'
                    massn       = '1'
                    massg       = ''
                      ) TO lt_data.

    LOOP AT lt_fcat INTO DATA(ls_fcat)  .
      add_head : ls_fcat-scrtext_l .
    ENDLOOP.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = ld_fullpath
        filetype              = 'ASC'
        write_field_separator = 'X'
        confirm_overwrite     = 'X'
        dat_mode              = 'D'
      TABLES
        data_tab              = lt_data[]
*        data_tab              = <dyn_table>[]
        fieldnames            = lt_head[]
      EXCEPTIONS
        file_open_error       = 1
        file_write_error      = 2
        OTHERS                = 3.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_STATU
*&---------------------------------------------------------------------*
FORM change_statu .
  DATA : ivals       TYPE TABLE OF sval.
  DATA : xvals       TYPE sval.
  DATA : returncode .
  DATA : ls_t003     TYPE /dsl/hr80_t003.

*      Normal koyuluk derecesi girişe hazır
*01	açık renkli girişe hazır
*02	Normal koyuluk derecesi girişe hazır değil
*03	açık renkli girişe hazır değil
*04	Görüntüleme!

  DEFINE add_vals.
    CLEAR xvals .
    xvals-tabname       = &1.
    xvals-fieldname     = &2.
    xvals-field_attr    = &3.
    xvals-value         = &4.
    xvals-field_obl     = &5.
    APPEND xvals TO ivals.
  END-OF-DEFINITION.


  SELECT SINGLE * FROM /dsl/hr80_t003 INTO ls_t003
      WHERE vrsid IN s_vrsid[]
        AND grpid IN s_grpid[]
        AND gjahr IN s_gjahr[]
        AND molga EQ p_molga .

  CHECK sy-subrc EQ 0 .
  add_vals: '/DSL/HR80_T003' 'VRSID'   '02' ls_t003-vrsid   abap_false .
  add_vals: '/DSL/HR80_T003' 'VRSID_T' '02' ls_t003-vrsid_t abap_false .
  add_vals: '/DSL/HR80_T003' 'BEGDA'   '02' ls_t003-begda   abap_false .
  add_vals: '/DSL/HR80_T003' 'ENDDA'   '02' ls_t003-endda   abap_false .
  add_vals: '/DSL/HR80_T003' 'STATU'   '01' ls_t003-statu   abap_false .

  CALL FUNCTION 'POPUP_GET_VALUES_SET_MAX_FIELD'
    EXPORTING
      number_of_fields = '40'
    EXCEPTIONS
      out_of_range     = 1
    .
  DO.
    CLEAR returncode .
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
         popup_title     = 'Bütçe versiyonu durum değişikliği'
         start_column    = 15
         start_row       = 10
      IMPORTING
         returncode      = returncode
      TABLES
         fields          = ivals
      EXCEPTIONS
         error_in_fields = 1
         OTHERS          = 2.
    IF returncode IS INITIAL.
      LOOP AT ivals INTO xvals.
        ASSIGN COMPONENT xvals-fieldname OF STRUCTURE ls_t003
            TO FIELD-SYMBOL(<fs>).
        CHECK <fs> IS ASSIGNED .
        SHIFT xvals-value LEFT DELETING LEADING space.
        <fs> = xvals-value.
        UNASSIGN <fs>.
      ENDLOOP.

      IF ( go_alv->gs_t003-statu EQ '0' OR go_alv->gs_t003-statu EQ '1' )
          AND ls_t003-statu NE go_alv->gs_t003-statu.
        DATA answer  .
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
        IF answer EQ '2'. " HAYIR
          EXIT.
        ENDIF.
      ENDIF.

      ls_t003-uname = sy-uname.
      ls_t003-datum = sy-datum.
      ls_t003-uzeit = sy-uzeit.

      MODIFY /dsl/hr80_t003 FROM ls_t003.
      MESSAGE s036   .
      MOVE-CORRESPONDING ls_t003 TO go_alv->gs_t003.
      EXIT.
    ELSE. " Hayır
      EXIT.
    ENDIF.
  ENDDO.

  PERFORM modify_editible .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_EDITIBLE
*&---------------------------------------------------------------------*
FORM modify_editible .

  DATA : lmc_de TYPE raw4.
  DATA : lt_styl  TYPE lvc_t_styl.
  FIELD-SYMBOLS <t_styl> TYPE lvc_t_styl .


  IF go_alv->gs_t003-statu EQ '0' OR go_alv->gs_t003-statu EQ '1'.
    go_alv->gmc_de = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    go_alv->gmc_de = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  lt_styl = VALUE #( FOR ls IN go_alv->mt_fcat
                      WHERE ( no_out NE 'X'
                        AND edit   EQ 'X'
                        AND fieldname NE 'RFPER' )
        ( fieldname = ls-fieldname
          style     = go_alv->gmc_de ) ).

  LOOP AT go_alv->gt_main ASSIGNING FIELD-SYMBOL(<ls_main>).
    <ls_main>-t_styl = lt_styl.
    CASE go_alv->gs_t003-statu.
      WHEN '1' OR '0'.
        lmc_de = cl_gui_alv_grid=>mc_style_enabled.
*        IF <ls_main>-pernr GT '8999999'.
        IF <ls_main>-pernr GT go_alv->gmc_dummy.
          INSERT VALUE #( fieldname = 'RFPER' style = lmc_de )
                INTO TABLE <ls_main>-t_styl .
        ELSEIF <ls_main>-pernr EQ '00000000'.
          INSERT VALUE #( fieldname = 'RFPER' style = lmc_de )
                INTO TABLE <ls_main>-t_styl .
        ELSE.
          INSERT VALUE #( fieldname = 'RFPER' style =
                cl_gui_alv_grid=>mc_style_disabled )
                      INTO TABLE <ls_main>-t_styl .
        ENDIF.
      WHEN OTHERS.
        INSERT VALUE #( fieldname = 'RFPER' style = go_alv->gmc_de )
            INTO TABLE <ls_main>-t_styl .
    ENDCASE.

    IF go_alv->record_check = 'C'." DEğişiklik kontrolü için
      <ls_main>-oprtn       = 'C'.
      <ls_main>-color       = 'C300'.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_RFPER_VALUES
*&---------------------------------------------------------------------*
FORM get_rfper_values  USING pv_pernr
                    CHANGING ps_main.

  SELECT SINGLE
      bukrs     persg        trfgr
      bukrs_t   ptext_t      trfgr_new
      orgeh     persg_new    trfst
      orgeh_t   persg_new_t  trfst_new
      plans     persk        lga01
      plans_t   persk_t      salry
      stell     persk_new    lga01_new
      stell_t   persk_new_t  salry_new
      kokrs     ssgrp        komok
      kokrs_t   kanun        mgart
      kostl     prozt        massn
      kostl_t   trfar        massn_t
      werks     trfar_t      massg
      werks_t   trfar_new    massg_t
      werks_new trfar_new_t  stat2
      btrtl     trfgb        ansvh
      btrtl_t   trfgb_t      abkrs
      btrtl_new trfgb_new    waers
                trfgb_new_t  statu
    FROM /dsl/hr80_ddl001 INTO CORRESPONDING
              FIELDS OF ps_main
    WHERE grpid       IN s_grpid[]
      AND vrsid       IN s_vrsid[]
      AND gjahr       IN s_gjahr[]
      AND molga       EQ p_molga
      AND pernr       EQ pv_pernr
      AND pa1_begda   LE s_datum-low
      AND pa1_endda   GE s_datum-low
 .
  CHECK sy-subrc NE 0 .
    SELECT SINGLE
      bukrs     persg       massn
      bukrs_t   ptext_t     massn_t
      orgeh     persk       massg
      orgeh_t   persk_t     massg_t
      plans     ssgrp       statu
      plans_t   kanun
      stell     trfar
      stell_t   trfar_t
      kokrs     trfgb
      kokrs_t   trfgb_t
      kostl     trfgr
      kostl_t   trfst
      werks     lga01
      werks_t   salry
      stat2     ansvh
      btrtl     abkrs
      btrtl_t   waers
     FROM /dsl/hr80_ddl002 INTO CORRESPONDING
                FIELDS OF ps_main
      WHERE grpid       IN s_grpid[]
        AND vrsid       IN s_vrsid[]
        AND gjahr       IN s_gjahr[]
        AND molga       EQ p_molga
        AND pernr       EQ pv_pernr
        AND pa1_begda   LE s_datum-low
        AND pa1_endda   GE s_datum-low
      .

ENDFORM.
