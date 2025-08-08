*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_P005_001
*&---------------------------------------------------------------------*


  TABLES : /dsl/hr80_s005.



SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-001 .
SELECT-OPTIONS :  s_pernr FOR /dsl/hr80_t010-pernr NO INTERVALS ,
                  s_werks FOR /dsl/hr80_t010-werks NO INTERVALS ,
                  s_btrtl FOR /dsl/hr80_t010-btrtl NO INTERVALS ,
                  s_persk FOR /dsl/hr80_t010-persk NO INTERVALS ,
                  s_persg FOR /dsl/hr80_t010-persg NO INTERVALS ,
                  s_orgeh FOR /dsl/hr80_t010-orgeh NO INTERVALS ,
                  s_plans FOR /dsl/hr80_t010-plans NO INTERVALS ,
                  s_stell FOR /dsl/hr80_t010-stell NO INTERVALS ,
                  s_datum FOR /dsl/hr80_t010-begda NO-EXTENSION
                                                   NO INTERVALS
                                                   DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK bl2.


  SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE TEXT-002  .
    PARAMETERS : r_rd1 RADIOBUTTON GROUP rd DEFAULT 'X' USER-COMMAND rd,
                 r_rd2 RADIOBUTTON GROUP rd .
    PARAMETERS :p_file TYPE rlgrap-filename MODIF ID rd2.

    SELECTION-SCREEN : FUNCTION KEY 1 .

  SELECTION-SCREEN END OF BLOCK bl3.


*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION   .

  PUBLIC SECTION.
     DATA : mo_splitter   TYPE REF TO cl_gui_splitter_container,
            mo_cont       TYPE REF TO cl_gui_custom_container,
            mo_top_cont   TYPE REF TO cl_gui_container,
            mo_alv_cont   TYPE REF TO cl_gui_container.

    METHODS :
          constructor  ,
          check_paramaters
                        CHANGING cv_check TYPE flag,
          at_selection_screen,
          template_file,
          get_data,
          create_alv_grid ,
          save_main,
          exc_func,
          get_text,
          get_excel IMPORTING file_name TYPE any   ,
          split_container
                        IMPORTING scrfname  TYPE scrfname
                                  screen    TYPE sydynnr,
          number_simul
                        IMPORTING nrnr TYPE nrnr
                        EXPORTING number TYPE numc10
                        EXCEPTIONS no_range,
          number_get_next
                        IMPORTING nrnr TYPE nrnr
                        EXPORTING number TYPE numc10
                        EXCEPTIONS no_range,
          popup_to_confirm
                        IMPORTING titlebar      TYPE any
                                  text_question TYPE any
                                  text_button_1 TYPE any
                                  text_button_2 TYPE any
                        EXPORTING answer TYPE any
                        EXCEPTIONS text_not_found ,
          process_itab_data
                          IMPORTING is_data TYPE /dsl/hr80_s005
                                    mode    TYPE char1
                                    index   TYPE lvc_index
                          EXCEPTIONS
                                     no_refpr
                                     no_range,

          check_values IMPORTING is_data TYPE /dsl/hr80_s005
                                 i_gjahr   TYPE gjahr
                          EXCEPTIONS no_begda
                                     no_endda
                                     big_begda
                                     ne_year
                                     no_count
                                     no_refpr.
*
  PROTECTED SECTION.


  PRIVATE SECTION.

ENDCLASS.                    "lcl_report DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION   .

  PUBLIC SECTION.
    DATA : BEGIN OF gs_main .
      INCLUDE TYPE /dsl/hr80_ddl001.
    DATA :    t_styl TYPE  lvc_t_styl,
              oprtn  TYPE char1,
              color(4) TYPE c,                      "<----Color field
              mark,
           END OF gs_main,
           gt_main LIKE TABLE OF gs_main.
    DATA : gt_delete LIKE TABLE OF gs_main .

    DATA : gs_t003 TYPE /dsl/hr80_t003.
    DATA : gmc_de TYPE raw4 .
    DATA : gmc_dummy TYPE persno . " Dummy personel numarası tanımı

    DATA : record_check.

    DATA : mo_grid      TYPE REF TO cl_gui_alv_grid,
           mo_document  TYPE REF TO cl_dd_document,
           mo_top_page  TYPE REF TO cl_gui_container,
           mo_prot      TYPE REF TO cl_alv_changed_data_protocol,
           mt_fcat      TYPE lvc_t_fcat,
           mt_exclude   TYPE ui_functions,
           ms_layout    TYPE lvc_s_layo,
           mo_popup     TYPE REF TO cl_salv_table.

    METHODS :
           constructor      ,
           set_layout       ,
           create_fcat      ,
           modify_fcat      ,
           set_events       ,
           exclude_buttons  ,
           check_changed_data,
           set_top_of_page
            CHANGING mo_splitter TYPE REF TO cl_gui_splitter_container,
           display_alv_grid
            CHANGING
                it_main TYPE ANY TABLE .

      METHODS :
            modify_target_value
                   IMPORTING
                      fname  TYPE lvc_fname
                      targt  TYPE name_komp
                      zvalue TYPE any .

    METHODS :
      refresh_alv,

      handle_user_command
          FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm ,

      handle_hotspot_click
          FOR EVENT hotspot_click OF cl_gui_alv_grid
            IMPORTING e_row_id
                      e_column_id
                      es_row_no,

      handle_toolbar
          FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object
                      e_interactive,

      handle_data_changed
          FOR EVENT data_changed OF cl_gui_alv_grid
            IMPORTING er_data_changed
                      e_onf4
                      e_onf4_before
                      e_onf4_after .

  PROTECTED SECTION.
    CONSTANTS : gc_strname TYPE strname VALUE '/DSL/HR80_DDL001'.
*
*
  PRIVATE SECTION.



ENDCLASS.                    "lcl_alv DEFINITION
