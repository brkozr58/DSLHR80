*----------------------------------------------------------------------*
***INCLUDE /DSL/HR80_P003_003.
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

  DATA : ls_t003 TYPE /dsl/hr80_t003.
  SELECT SINGLE * FROM /dsl/hr80_t003 INTO ls_t003
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
    s_gjahr-low = ls_t003-gjahr.
    APPEND s_gjahr.
    LOOP AT s_gjahr.  ENDLOOP.
  ENDIF.


  IF s_gjahr[] IS INITIAL .
    MESSAGE i027 DISPLAY LIKE 'E'.
    cv_check = 'X'.
  ENDIF.

*
*  SELECT SINGLE * FROM /dsl/hr80_t003 INTO ls_t003
*    WHERE grpid IN s_grpid[]
*      AND vrsid IN s_vrsid[]
*      AND gjahr IN s_gjahr[]
*      AND statu IN s_statu[].
*  IF sy-subrc NE 0 .
*    MESSAGE i005 DISPLAY LIKE 'E'.
*    cv_check = 'X'.
*  ENDIF.


  IF r_rd2 EQ 'X' . " Toplu aktarım
    IF p_file IS INITIAL .
      MESSAGE i000 DISPLAY LIKE 'E'
          WITH 'Aktarım dosyası seçiniz'.
      cv_check = 'X'.
    ENDIF..
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

  DATA : lmc_de TYPE raw4 .
  DATA : lt_styl  TYPE lvc_t_styl.

  lt_styl = VALUE #( FOR ls IN go_alv->mt_fcat WHERE ( no_out NE 'X'
                                                   AND edit   EQ 'X' )
        ( fieldname = ls-fieldname
          style     = cl_gui_alv_grid=>mc_style_enabled ) ).
*  go_alv->gt_delete = VALUE #( FOR ls IN  go_alv->gt_main WHERE ( mark = 'X' )  ( ls ) ).
*  LOOP AT go_alv->mt_fcat INTO DATA(ls_fcat)
*        WHERE no_out NE 'X' AND edit EQ 'X'.
*    INSERT VALUE #( fieldname = ls_fcat-fieldname
*                    style     = cl_gui_alv_grid=>mc_style_enabled )
*                   INTO TABLE lt_styl .
*  ENDLOOP.

  SELECT *    FROM /dsl/hr80_t004 AS t4
        INNER JOIN /dsl/hr80_t003 AS t3
          ON    t3~grpid EQ t4~grpid
            AND t3~vrsid EQ t4~vrsid
        INNER JOIN /dsl/hr80_t001 AS t1
          ON    t1~grpid EQ t3~grpid
    INTO CORRESPONDING
   FIELDS OF TABLE go_alv->gt_main
    WHERE t4~grpid   IN s_grpid[]
      AND t4~vrsid   IN s_vrsid[]
      AND t3~gjahr   IN s_gjahr[]
      AND t1~molga   EQ p_molga .

  LOOP AT go_alv->gt_main ASSIGNING FIELD-SYMBOL(<ls_main>).
    lmc_de = cl_gui_alv_grid=>mc_style_enabled.
    <ls_main>-t_styl = lt_styl[].
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_EXCEL
*&---------------------------------------------------------------------*
FORM get_excel USING file_name TYPE any
            CHANGING  data      TYPE table.

  DATA : l_file_name TYPE ibipparms-path.
  DATA : filename  TYPE  rlgrap-filename.

  FIELD-SYMBOLS: <fs> TYPE any.
  DATA: dref1 TYPE REF TO data.
  DATA: dref2 TYPE REF TO data.
  FIELD-SYMBOLS <table> LIKE data.
  FIELD-SYMBOLS <wa>    TYPE any.

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


  IF p_file IS INITIAL .
    CALL FUNCTION 'F4_FILENAME'
      IMPORTING
        file_name = l_file_name.

    filename = l_file_name.
  ELSE.
    filename = file_name.
  ENDIF.

  CREATE DATA dref1 LIKE data.
  ASSIGN dref1->* TO <table>.

  CREATE DATA dref2 LIKE LINE OF <table>.
  ASSIGN dref2->* TO <wa>.

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
  FIELD-SYMBOLS <f1> TYPE kcde_cells .
  LOOP AT lt_intern ASSIGNING <f1>.
    MOVE : <f1>-col TO ld_index.
    UNASSIGN <fs> .
    ASSIGN COMPONENT ld_index OF STRUCTURE <wa> TO <fs> .
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
         CONCATENATE lv_string1 lv_string2 lv_string3 INTO <f1>-value .
          CONCATENATE <f1>-value+4(4)   <f1>-value+2(2)
                       <f1>-value+0(2) INTO <fs>.
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
      ASSIGN COMPONENT 'OPRTN' OF STRUCTURE <wa> TO FIELD-SYMBOL(<oprtn>).
      IF <oprtn> IS ASSIGNED .
        <oprtn> = 'N'.
      ENDIF.
      go_alv->record_check = 'C'.

      ASSIGN COMPONENT 'MOLGA' OF STRUCTURE <wa> TO FIELD-SYMBOL(<molga>).
      IF <molga> IS ASSIGNED .<molga>   = p_molga..ENDIF.

      ASSIGN COMPONENT 'GRPID' OF STRUCTURE <wa> TO FIELD-SYMBOL(<grpid>).
      IF <grpid> IS ASSIGNED .<grpid>   = s_grpid-low.ENDIF.

      ASSIGN COMPONENT 'VRSID' OF STRUCTURE <wa> TO FIELD-SYMBOL(<vrsid>).
      IF <vrsid> IS ASSIGNED .<vrsid>   = s_vrsid-low.ENDIF.

      ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <wa> TO FIELD-SYMBOL(<gjahr>).
      IF <gjahr> IS ASSIGNED .<gjahr>   = s_gjahr-low.ENDIF.

      APPEND <wa> TO data.
      CLEAR <wa>.
    ENDAT.
  ENDLOOP.
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

  DATA : lt_main TYPE /dsl/hr80_tt004 ,
         ls_main LIKE LINE OF lt_main.

  FIELD-SYMBOLS:<dyn_table>   TYPE STANDARD TABLE,
                <dyn_struc>   TYPE any,
                <fs>          TYPE any .
  DATA : itab TYPE REF TO data ,
         line TYPE REF TO data.

  DEFINE add_head.
    ls_head-header = &1.
    APPEND ls_head TO lt_head.
    CLEAR  ls_head .
  END-OF-DEFINITION.

  DEFINE move_value .
    UNASSIGN <fs>.
    ASSIGN COMPONENT &1 OF STRUCTURE <dyn_struc> TO <fs>.
    CHECK <fs> IS ASSIGNED .
    <fs> = &2.
  END-OF-DEFINITION.


*  APPEND ls_main TO lt_main.

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

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog           = lt_fcat[]
      IMPORTING
        ep_table                  = itab
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.
    ASSIGN itab->* TO <dyn_table>.
    CREATE DATA line LIKE LINE OF <dyn_table>.
    ASSIGN line->* TO <dyn_struc>.

    "<<-------- örnek bir sicil verisi alsın diye ------>>
    move_value : 'BUKRS' '2000',
                 'KOSTL' '0000001001',
                 'ANSVH' '',
                 'WERKS' '2000',
                 'BTRTL' '2001',
                 'PERSG' '1',
                 'PERSK' '01',
                 'ORGEH' '50000553',
                 'STELL' '50000828',
                 'ABKRS' '10',
                 'PERNR' '' .
*
    LOOP AT lt_fcat INTO DATA(ls_fcat) WHERE fieldname(3) EQ 'BET' OR
                                             fieldname(3) EQ 'ANZ' OR
                                             fieldname(3) EQ 'RAT' .
      ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE  <dyn_struc> TO <fs>.
      CASE ls_fcat-fieldname(3).
        WHEN 'BET'.  <fs> = 20.
        WHEN 'ANZ'.  <fs> = 15.
        WHEN 'RAT'.  <fs> = 3.
      ENDCASE.
    ENDLOOP.

    "<<--------END CODE------>>

    APPEND <dyn_struc> TO <dyn_table>.

    LOOP AT lt_fcat INTO ls_fcat  .
      add_head : ls_fcat-scrtext_l .
    ENDLOOP.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = ld_fullpath
        filetype              = 'ASC'
        write_field_separator = 'X'
        confirm_overwrite     = 'X'
      TABLES
        data_tab              = <dyn_table>[]
        fieldnames            = lt_head[]
      EXCEPTIONS
        file_open_error       = 1
        file_write_error      = 2
        OTHERS                = 3.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ROW_RECORD
*&---------------------------------------------------------------------*
FORM row_record  USING mode TYPE char1
                       pt_rows TYPE lvc_t_row .
  FIELD-SYMBOLS <fs_f> TYPE any .
  DATA: ivals  TYPE TABLE OF sval.
  DATA: xvals  TYPE sval.
  DATA : ls_s004 TYPE /dsl/hr80_s004.
  DATA : ls_t003 TYPE /dsl/hr80_t003.
  DATA : ls_stm LIKE go_alv->gs_main.
  DATA : ls_t004 TYPE /dsl/hr80_t004.
  DATA : returncode .
  DATA : ls_data TYPE /dsl/hr80_s003.
  DATA : lv_attr  TYPE spo_fattr .

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
*    IF &4 IS NOT INITIAL .
      xvals-value         = &4.
*    ENDIF.
    xvals-field_obl     = &5.
    IF &6 IS NOT INITIAL .
      xvals-fieldtext     = &6.
    ENDIF.
    APPEND xvals TO ivals.
  END-OF-DEFINITION.

  DATA : lmc_de TYPE raw4 .
  DATA : lt_styl  TYPE lvc_t_styl.

  SELECT SINGLE * FROM /dsl/hr80_t003 INTO ls_t003
      WHERE vrsid IN s_vrsid[]
        AND grpid IN s_grpid[]
        AND gjahr IN s_gjahr[]
        AND molga EQ p_molga

    .

  REFRESH ivals.

  CASE mode.
    WHEN 'I'.
      CLEAR : ls_data.
      CLEAR : lv_attr.
    WHEN 'C'.
      lv_attr = '03'.
      READ TABLE pt_rows INTO DATA(ls_rows) INDEX 1 .
      READ TABLE go_alv->gt_main INTO DATA(ls_main) INDEX ls_rows-index.
      MOVE-CORRESPONDING ls_main TO ls_data .
    WHEN OTHERS.
  ENDCASE.


  ls_data-begda = ls_t003-gjahr && '0101'.
  ls_data-endda = ls_t003-gjahr && '1231'.

  add_vals: '/DSL/HR80_S003' 'BUKRS' lv_attr  ls_data-bukrs abap_false space.
  add_vals: '/DSL/HR80_S003' 'KOSTL' lv_attr  ls_data-kostl abap_false space.
  add_vals: '/DSL/HR80_S003' 'ANSVH' lv_attr  ls_data-ansvh abap_false space.
  add_vals: '/DSL/HR80_S003' 'WERKS' lv_attr  ls_data-werks abap_false space.
  add_vals: '/DSL/HR80_S003' 'BTRTL' lv_attr  ls_data-btrtl abap_false space.
  add_vals: '/DSL/HR80_S003' 'PERSG' lv_attr  ls_data-persg abap_false space.
  add_vals: '/DSL/HR80_S003' 'PERSK' lv_attr  ls_data-persk abap_false space.
  add_vals: '/DSL/HR80_S003' 'ORGEH' lv_attr  ls_data-orgeh abap_false space.
  add_vals: '/DSL/HR80_S003' 'STELL' lv_attr  ls_data-stell abap_false space.
  add_vals: '/DSL/HR80_S003' 'ABKRS' lv_attr  ls_data-abkrs abap_false space.
  add_vals: '/DSL/HR80_S003' 'PERNR' lv_attr  ls_data-pernr abap_false space.
  add_vals: '/DSL/HR80_S003' 'BEGDA' space  ls_data-begda abap_false space.
  add_vals: '/DSL/HR80_S003' 'ENDDA' space  ls_data-endda abap_false space.
  add_vals: '/DSL/HR80_S003' 'RAT01' space  space         abap_false space.
  add_vals: '/DSL/HR80_S003' 'ANZ01' space  space         abap_false space.
  add_vals: '/DSL/HR80_S003' 'BET01' space  space         abap_false space.
  add_vals: '/DSL/HR80_S003' 'WAERS' space  space         abap_false space.


  lt_styl = VALUE #( FOR ls IN go_alv->mt_fcat WHERE ( no_out NE 'X'
                                                   AND edit   EQ 'X'
                                                    )
        ( fieldname = ls-fieldname
          style     = cl_gui_alv_grid=>mc_style_enabled ) ).

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
         popup_title     = 'Bütçe Parametresi Girişi'
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
      CLEAR : ls_s004.
      LOOP AT ivals INTO xvals.
        UNASSIGN <fs_f>.
        ASSIGN COMPONENT xvals-fieldname OF STRUCTURE ls_data TO <fs_f>.
        CHECK <fs_f> IS ASSIGNED .
        SHIFT xvals-value LEFT DELETING LEADING space.
        <fs_f> = xvals-value.
      ENDLOOP.

      CASE mode.
        WHEN 'C'.
          MOVE-CORRESPONDING ls_main TO ls_s004 .
        WHEN OTHERS.
      ENDCASE.

      go_main->param_move_field(
         EXPORTING
            bukrs           = ls_data-bukrs
            kostl           = ls_data-kostl
            ansvh           = ls_data-ansvh
            werks           = ls_data-werks
            btrtl           = ls_data-btrtl
            persg           = ls_data-persg
            persk           = ls_data-persk
            orgeh           = ls_data-orgeh
            stell           = ls_data-stell
            abkrs           = ls_data-abkrs
            pernr           = ls_data-pernr
            begda           = ls_data-begda
            endda           = ls_data-endda
            ratxx           = ls_data-rat01
            anzxx           = ls_data-anz01
            betxx           = ls_data-bet01
            gjahr           = ls_t003-gjahr
        IMPORTING
            ps_s004         = ls_s004
        EXCEPTIONS
          big_begda         = 010
          ne_year           = 023
          no_bukrs          = 024
          no_date           = 025
          record_available  = 1          ).

      IF ls_s004 IS NOT INITIAL AND sy-subrc EQ 0.

        ls_s004-vrsid = ls_t003-vrsid.
        ls_s004-grpid = ls_t003-grpid.
        ls_s004-gjahr = ls_t003-gjahr.
        ls_s004-molga = p_molga.
        ls_s004-waers = ls_data-waers.

        MOVE-CORRESPONDING ls_s004 TO ls_stm.
        go_alv->record_check = 'C'." DEğişiklik kontrolü için
        ls_stm-t_styl = lt_styl[].
        CASE mode.
          WHEN 'I'.
            ls_stm-oprtn = 'N'.
            APPEND ls_stm TO go_alv->gt_main.
          WHEN 'C'.
            ls_stm-oprtn = 'C'.
*            ls_stm-color = 'C310'.
            MODIFY go_alv->gt_main FROM ls_stm INDEX ls_rows-index.
        ENDCASE.
        EXIT.
      ELSE.
        CASE sy-subrc.
          WHEN 1.  MESSAGE i028 DISPLAY LIKE 'E'.
          WHEN 010.MESSAGE i010 DISPLAY LIKE 'E'.
          WHEN 023.MESSAGE i023 DISPLAY LIKE 'E'.
          WHEN 024.MESSAGE i024 DISPLAY LIKE 'E'.
          WHEN 025.MESSAGE i025 DISPLAY LIKE 'E'.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.
