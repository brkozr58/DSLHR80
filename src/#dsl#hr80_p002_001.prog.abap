*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_P002_001
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
          create_alv_grid ,
          split_container
                        IMPORTING scrfname  TYPE scrfname
                                  screen    TYPE sydynnr,
          create_grpid,
          create_vrsid,
          check_grpid
                        IMPORTING   s_t001 TYPE /dsl/hr80_t001
                                    s_t002 TYPE /dsl/hr80_t002
                        EXCEPTIONS  no_molga
                                    no_bukrs
                                    no_werks
                                    no_btrtl
                                    no_abkrs
                                    record_available,
          check_vrsid
                        IMPORTING   s_t003 TYPE /dsl/hr80_t003
                        EXCEPTIONS  no_statu
                                    no_begda
                                    no_endda
                                    big_begda
                                    no_fpbeg
                                    no_fpend
                                    big_fpbeg
                                    ne_year,
          save_main,
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
                        EXCEPTIONS text_not_found .
*
  PROTECTED SECTION.


  PRIVATE SECTION.

ENDCLASS.                    "lcl_report DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION   .

  PUBLIC SECTION.
    DATA : gt_main TYPE /dsl/hr80_tt003 .
    DATA : gt_delete TYPE /dsl/hr80_tt003 .

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
           add_new_record,
           set_top_of_page
              CHANGING mo_splitter TYPE REF TO cl_gui_splitter_container   ,
           display_alv_grid
            CHANGING
                it_main TYPE /dsl/hr80_tt003 .

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
    CONSTANTS : gc_strname TYPE strname VALUE '/DSL/HR80_S002'.
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
