*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_P003_001
*&---------------------------------------------------------------------*

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
          get_data,
          get_excel IMPORTING file_name TYPE any
                    CHANGING  data      TYPE table,
          create_alv_grid ,
          exc_func ,
          template_file ,
          save_main,
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
           row_record IMPORTING mode TYPE char1
                                pt_rows TYPE lvc_t_row OPTIONAL ,
          param_move_field IMPORTING bukrs TYPE /dsl/hr80_s004-bukrs
                                     kostl TYPE /dsl/hr80_s004-kostl
                                     ansvh TYPE /dsl/hr80_s004-ansvh
                                     werks TYPE /dsl/hr80_s004-werks
                                     btrtl TYPE /dsl/hr80_s004-btrtl
                                     persg TYPE /dsl/hr80_s004-persg
                                     persk TYPE /dsl/hr80_s004-persk
                                     orgeh TYPE /dsl/hr80_s004-orgeh
                                     stell TYPE /dsl/hr80_s004-stell
                                     abkrs TYPE /dsl/hr80_s004-abkrs
                                     pernr TYPE /dsl/hr80_s004-pernr
                                     begda TYPE begda
                                     endda TYPE endda
                                     ratxx TYPE /dsl/hr80_s004-rat01
                                     anzxx TYPE /dsl/hr80_s004-anz01
                                     betxx TYPE /dsl/hr80_s004-bet01
                                     gjahr TYPE /dsl/hr80_t003-gjahr
                          EXPORTING ps_s004 TYPE /dsl/hr80_s004
                          EXCEPTIONS big_begda
                                     ne_year
                                     no_bukrs
                                     no_date
                                     record_available.
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
      INCLUDE TYPE /dsl/hr80_s004.
    DATA :    t_styl TYPE  lvc_t_styl,
              oprtn  TYPE char1,
              color(3) TYPE c,                      "<----Color field
           END OF gs_main,
           gt_main LIKE TABLE OF gs_main.
    DATA : gt_delete LIKE TABLE OF gs_main .

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
              CHANGING mo_splitter TYPE REF TO cl_gui_splitter_container   ,
           display_alv_grid
            CHANGING
                it_main TYPE ANY TABLE .

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
    CONSTANTS : gc_strname TYPE strname VALUE '/DSL/HR80_S004'.
*
*
  PRIVATE SECTION.
      METHODS :
            modify_target_value
                   IMPORTING
                      fname  TYPE lvc_fname
                      targt  TYPE name_komp
                      zvalue TYPE any .



ENDCLASS.                    "lcl_alv DEFINITION
