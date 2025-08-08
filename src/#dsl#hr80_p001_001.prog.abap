*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_P001_001
*&---------------------------------------------------------------------*


* types ..
TYPE-POOLS : slis .

DATA : BEGIN OF gt_out OCCURS 0    ,
          grpid     LIKE /dsl/hr80_tgrp-grpid    ,
          grpid_t   LIKE /dsl/hr80_tgrp-grpid_t  ,
          grp_icon  LIKE /dsl/hr80_tgrp-icon     ,
          algrp     LIKE /dsl/hr80_tagrp-algrp   ,
          algtx     LIKE /dsl/hr80_tagrp-algtx   ,
          trnsc     LIKE /dsl/hr80_tagrp-trnsc   ,
          alt_icon  LIKE /dsl/hr80_tagrp-icon    ,
          zdesc     LIKE /dsl/hr80_tagrp-zdesc   ,
       END OF gt_out ,
       gs_out LIKE LINE OF gt_out.

DATA : gt_tgrp TYPE TABLE OF /dsl/hr80_tgrp WITH HEADER LINE.
DATA : gt_tagrp TYPE TABLE OF /dsl/hr80_tagrp WITH HEADER LINE.


DATA : BEGIN OF gt_alv_tree OCCURS 0 ,
         details TYPE tv_image       ,
       END OF gt_alv_tree            .

* Definitions For The Alv_Tree
DATA : splitter         TYPE REF TO cl_gui_splitter_container,
       gr_alv_tree      TYPE REF TO cl_gui_alv_tree ,
       gr_toolbar       TYPE REF TO cl_gui_toolbar,
*        container       TYPE REF TO cl_gui_docking_container ,
       container        TYPE REF TO cl_gui_custom_container ,
       container2       TYPE REF TO cl_gui_custom_container ,
       container_1      TYPE REF TO cl_gui_container         ,
       container_2      TYPE REF TO cl_gui_container         ,
       gt_toolbar       TYPE ui_functions ,
       go_picture       TYPE REF TO cl_gui_picture,
       gv_url           TYPE char255,
       gv_fullpath      TYPE string..

DATA :  gt_objec  LIKE objec OCCURS 0 WITH HEADER LINE,
        gt_struc  LIKE struc OCCURS 0 WITH HEADER LINE.
DATA : gs_header LIKE treev_hhdr ,
       gt_fcat   TYPE lvc_t_fcat WITH HEADER LINE .


DATA : gv_repid TYPE sy-repid VALUE sy-repid.
DATA : gv_dynnr TYPE sy-dynnr VALUE '0101'.


*---------------------------------------------------------------------*
*       CLASS lcl_tree_event_receiver DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.
    DATA : gt_objec   TYPE TABLE OF objec  ,
           gt_struc   TYPE TABLE OF struc  ,
           gs_header  TYPE treev_hhdr ,
           gt_fcat    TYPE lvc_t_fcat ,
           gt_tgrp    TYPE TABLE OF /dsl/hr80_tgrp ,
           gt_tagrp   TYPE TABLE OF /dsl/hr80_tagrp.


    METHODS :
              initialization        ,
              create_instances      ,
              generate_first_display,
              create_tree_nodes     ,
              register_events       ,
              add_tree_button       ,
              add_node_folder
                IMPORTING if_relat TYPE lvc_nkey
                          if_text  TYPE lvc_value
                          if_icon  TYPE tv_image
                          ps_out   LIKE gs_out
                EXPORTING p2_key   TYPE lvc_nkey
                  ,
              add_node
                IMPORTING if_relat    TYPE lvc_nkey
                          if_text     TYPE lvc_value
                          if_icon     TYPE tv_image
                          ps_out      LIKE gs_out
                EXPORTING p2_key      TYPE lvc_nkey,


      handle_node_double_click
            FOR EVENT node_double_click OF cl_gui_alv_tree
            IMPORTING node_key
                      sender,

      handle_item_double_click
            FOR EVENT item_double_click OF cl_gui_alv_tree
            IMPORTING node_key
                      fieldname
                      sender ,
      handle_link_click
            FOR EVENT link_click OF cl_gui_alv_tree
            IMPORTING node_key
                      fieldname ,
      handle_on_function_selected
            FOR EVENT function_selected OF cl_gui_toolbar
            IMPORTING fcode.

ENDCLASS.                    "lcl_tree_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_tree_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_tree_event_receiver IMPLEMENTATION.

  METHOD initialization      .
    SELECT * FROM /dsl/hr80_tgrp INTO TABLE gt_tgrp.
    SELECT * FROM /dsl/hr80_tagrp INTO TABLE gt_tagrp.
    SORT gt_tgrp ASCENDING BY grpid  .
    SORT gt_tagrp ASCENDING BY grpid algrp .
  ENDMETHOD.

  METHOD create_instances      .
    DATA: lr_mime_rep TYPE REF TO if_mr_api.
    DATA: lv_content  TYPE xstring.
    DATA: p_path TYPE string VALUE 'SAP/PUBLIC/BUDGET2.PNG'.
    DATA: lt_data TYPE STANDARD TABLE OF x255.
    DATA: BEGIN OF graphic_table ,
            line(255) TYPE x,
          END OF graphic_table.
    DATA: gt_line        LIKE TABLE OF graphic_table,
          url(255)       TYPE c,
          l_graphic_conv TYPE i,
          l_graphic_offs TYPE i,
          graphic_size   TYPE i,
          l_graphic_xstr TYPE xstring.


*     create container for alv-tree
      CREATE OBJECT container
        EXPORTING
          container_name = 'CONT' .



      CREATE OBJECT splitter
        EXPORTING
          parent  = container
          rows    = 1
          columns = 2.

      CALL METHOD splitter->set_column_width
        EXPORTING
          id     = 1  " 1. kolonu ifade eder
          width  = 50.
*          width  = 25.

      CALL METHOD splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
          RECEIVING
          container = container_1.

      CALL METHOD splitter->get_container
        EXPORTING
          row       = 1
          column    = 2
          RECEIVING
          container = container_2.
*
      CREATE OBJECT gr_alv_tree
        EXPORTING
          parent              = container_1
          node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
          item_selection      = 'X'
          no_html_header      = 'X'
          no_toolbar          = ''
        .

      " Picture control nesnesini oluştur
      CREATE OBJECT go_picture
        EXPORTING
          parent = container_2.

      lr_mime_rep = cl_mime_repository_api=>if_mr_api~get_api( ).

      lr_mime_rep->get(
        EXPORTING
          i_url      = p_path
        IMPORTING
          e_content = lv_content
        EXCEPTIONS
          not_found = 3 ).

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = lv_content
        TABLES
          binary_tab = lt_data.

      CALL FUNCTION 'DP_CREATE_URL'
        EXPORTING
          type    = 'IMAGE'
          subtype = 'BMP'
        TABLES
          data    = lt_data
        CHANGING
          url     = url.

      go_picture->load_picture_from_url_async(
        EXPORTING
          url   = url
        EXCEPTIONS
          error = 1
      ).

      CALL METHOD go_picture->set_display_mode
        EXPORTING
          display_mode = cl_gui_picture=>display_mode_fit_center.
  ENDMETHOD.

  METHOD generate_first_display.

  ENDMETHOD.

  METHOD create_tree_nodes     .
    DATA: l_key  TYPE lvc_nkey.
    DATA: l_last TYPE lvc_nkey.
    DATA: lt_node_keys TYPE STANDARD TABLE OF lvc_nkey,
          lv_node_key  TYPE lvc_nkey.


    LOOP AT gt_tgrp INTO DATA(ls_tgrp).
      CLEAR: gs_out.
      gs_out-grpid      = ls_tgrp-grpid     .
      gs_out-grpid_t    = ls_tgrp-grpid_t   .
      gs_out-grp_icon   = ls_tgrp-icon.     .


      add_node_folder(
        EXPORTING
          if_relat = CONV lvc_nkey( '' )
          if_text  = CONV lvc_value( ls_tgrp-grpid_t )
          if_icon  = CONV tv_image( ls_tgrp-icon )
          ps_out   = gs_out
        IMPORTING
          p2_key   = l_key
      ).
      APPEND l_key TO lt_node_keys.


      LOOP AT gt_tagrp INTO DATA(ls_tagrp)
            WHERE grpid EQ ls_tgrp-grpid.
        CLEAR: gs_out.
        gs_out-grpid      = ls_tgrp-grpid     .
        gs_out-grpid_t    = ls_tgrp-grpid_t   .
        gs_out-grp_icon   = ls_tgrp-icon.     .
        gs_out-algrp      = ls_tagrp-algrp   .
        gs_out-algtx      = ls_tagrp-algtx   .
        gs_out-trnsc      = ls_tagrp-trnsc   .
        gs_out-alt_icon   = ls_tagrp-icon    .
        gs_out-zdesc      = ls_tagrp-zdesc    .


        add_node(
          EXPORTING
            if_relat = l_key
            if_text  = CONV lvc_value( ls_tagrp-algtx )
            if_icon  = CONV tv_image( ls_tagrp-icon )
            ps_out   = gs_out
          IMPORTING
            p2_key   = l_last
        ).
        APPEND l_last TO lt_node_keys.

      ENDLOOP.

    ENDLOOP.

    " Tüm düğümleri genişlet
    LOOP AT lt_node_keys INTO lv_node_key.
      CALL METHOD gr_alv_tree->expand_node
        EXPORTING
          i_node_key = lv_node_key.
    ENDLOOP.


    CALL METHOD gr_alv_tree->frontend_update.
    CALL METHOD cl_gui_cfw=>flush.
  ENDMETHOD.

  METHOD register_events       .

    DATA: lt_events TYPE cntl_simple_events,
          l_event   TYPE cntl_simple_event ,
          l_event_receiver TYPE REF TO lcl_tree_event_receiver.
    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
    l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
    APPEND l_event TO lt_events.
    l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
    APPEND l_event TO lt_events.
    l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
    APPEND l_event TO lt_events.
    l_event-eventid = cl_gui_column_tree=>eventid_header_click.
    APPEND l_event TO lt_events.

    CALL METHOD gr_alv_tree->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.
  ENDMETHOD.

  METHOD add_node_folder.
    DATA g_handle_tree TYPE i.
    DATA: l_node_text TYPE lvc_value .
    DATA: lt_item_layout TYPE lvc_t_layi,
          ls_item_layout TYPE lvc_s_layi,
          ls_node_layout TYPE lvc_s_layn.

    ls_node_layout-n_image   = if_icon  .
    ls_node_layout-exp_image = if_icon  .
    ls_item_layout-fieldname = gr_alv_tree->c_hierarchy_column_name.
    APPEND ls_item_layout TO lt_item_layout.
    l_node_text =  if_text.
    ls_node_layout-dragdropid = g_handle_tree.


    CALL METHOD gr_alv_tree->add_node
      EXPORTING
        i_relat_node_key = if_relat
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
        is_node_layout   = ls_node_layout
        is_outtab_line   = ps_out
        it_item_layout   = lt_item_layout
      IMPORTING
        e_new_node_key   = p2_key.
  ENDMETHOD.


  METHOD add_node.
    DATA ls_layout   TYPE  lvc_s_layn .
    DATA l_text      TYPE lvc_value .
    DATA g_handle_tree TYPE i.

    ls_layout-n_image   = if_icon  .
    ls_layout-exp_image = if_icon  .
    ls_layout-isfolder  = 'X' .
    l_text = if_text .
    ls_layout-dragdropid = g_handle_tree.

    CALL METHOD gr_alv_tree->add_node
      EXPORTING
        i_relat_node_key = CONV lvc_nkey( if_relat )
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = CONV lvc_value( l_text )
        is_outtab_line   = ps_out
        is_node_layout   = ls_layout
      IMPORTING
        e_new_node_key   = p2_key
        .

  ENDMETHOD.


  METHOD handle_node_double_click.
    CHECK NOT node_key IS INITIAL.
    PERFORM info USING node_key.
  ENDMETHOD.                    "handle_node_double_click

  METHOD handle_link_click.

  ENDMETHOD.

  METHOD handle_item_double_click.

  ENDMETHOD.                    "handle_item_double_click

  METHOD handle_on_function_selected.
    CASE fcode.
      WHEN 'GRPID'.
        CALL TRANSACTION '/DSL/HR80_MENU'.
    ENDCASE.
  ENDMETHOD.

  METHOD add_tree_button.

    CALL METHOD gr_alv_tree->get_toolbar_object
      IMPORTING
        er_toolbar = gr_toolbar.

    CHECK gr_toolbar IS NOT INITIAL.


    CALL METHOD gr_toolbar->add_button
      EXPORTING
        fcode     = 'GRPID'
        icon      = '@0Y@'
        butn_type = '0'
        text      = 'Menü Uyarlaması'
        quickinfo = 'Menü Uyarlaması'.
  ENDMETHOD.

ENDCLASS.                    "lcl_tree_event_receiver IMPLEMENTATION


DATA: lc_tree TYPE REF TO lcl_tree_event_receiver.


*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
 SET PF-STATUS 'GUI'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR  'CANC'.
      LEAVE TO SCREEN 0 .
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_TREE OUTPUT
*&---------------------------------------------------------------------*
MODULE create_tree OUTPUT.

  IF container IS INITIAL.
    CREATE OBJECT lc_tree.
    lc_tree->initialization( ).
    lc_tree->create_instances( ).
*    lc_tree->generate_first_display( ).
    PERFORM generate_first_display.
    lc_tree->create_tree_nodes( ).
    lc_tree->register_events( ).
    lc_tree->add_tree_button( ).
    SET HANDLER lc_tree->handle_node_double_click FOR gr_alv_tree.
    SET HANDLER lc_tree->handle_item_double_click FOR gr_alv_tree.
    SET HANDLER lc_tree->handle_link_click FOR gr_alv_tree.
    SET HANDLER lc_tree->handle_on_function_selected FOR gr_toolbar .
    CALL METHOD gr_alv_tree->frontend_update.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Form INFO
*&---------------------------------------------------------------------*
FORM info  USING    pv_key.
  DATA: ls_gt_p LIKE gs_out,
        lt_children TYPE lvc_t_nkey WITH HEADER LINE,
        ls_tstc TYPE tstc.

  CALL METHOD gr_alv_tree->get_outtab_line
    EXPORTING
      i_node_key    = pv_key
    IMPORTING
      e_outtab_line = ls_gt_p.

*gv_repid
*gv_dynnr

  IF ls_gt_p IS INITIAL.
  ELSE.
    LOOP AT gt_out  INTO gs_out
              WHERE grpid  = ls_gt_p-grpid
                AND algrp  = ls_gt_p-algrp
                AND trnsc IS NOT INITIAL      .
*      SELECT SINGLE * FROM tstc INTO ls_tstc
*          WHERE tcode EQ gs_out-trnsc.
*
*      IF ls_tstc-pgmna IS NOT INITIAL .
*        gv_repid = ls_tstc-pgmna.
*        gv_dynnr = ls_tstc-dypno.
*      ELSE.
        CALL TRANSACTION gs_out-trnsc .
*      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GENERATE_FIRST_DISPLAY
*&---------------------------------------------------------------------*
FORM generate_first_display .

    DATA : gv_ndimc   TYPE tv_image          ,
           lv_heading TYPE tv_heading        ,
           lv_nodekey TYPE tv_nodekey        . .
    DATA : alv_fieldcat         TYPE slis_t_fieldcat_alv  .
    DATA : ls_fcat TYPE lvc_s_fcat.

*   ** Tree Başlık Özellikleri
     gs_header-width_pix = ' '             .
     lv_heading                   = 'Bütçe Menüsü' .
     gs_header-heading   = lv_heading      .
     gs_header-tooltip   = lv_heading      .
     gv_ndimc                     = '@3M@'          .
     gs_header-width     = 62              .
     gs_header-t_image   = gv_ndimc        .

*    ** Fieldcat Özellikleri
      CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
        EXPORTING
          i_program_name         = sy-repid
          i_internal_tabname     = 'GT_OUT'
*          i_structure_name       = 'GS_OUT'
          i_inclname             = sy-repid
          i_bypassing_buffer     = 'X'
        CHANGING
          ct_fieldcat            = alv_fieldcat[]
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.

      LOOP AT alv_fieldcat INTO DATA(ls_alvf) .
        MOVE-CORRESPONDING ls_alvf TO ls_fcat.
        ls_fcat-scrtext_l = ls_alvf-seltext_l.
        ls_fcat-scrtext_m = ls_alvf-seltext_m.
        ls_fcat-scrtext_s = ls_alvf-seltext_s .

        IF ls_fcat-fieldname NE 'TRNSC'
          AND ls_fcat-fieldname NE 'ZDESC'
          .
          ls_fcat-no_out = 'X' .
        ELSEIF ls_fcat-fieldname EQ 'ZDESC'.
          ls_fcat-outputlen = 70.
        ELSE.
          ls_fcat-outputlen = 30.
        ENDIF.
*        ls_fcat-col_opt = 'X' .

        APPEND ls_fcat TO gt_fcat.
      ENDLOOP.

*    ** Menüden bazı fonksiyonların çıkartılması
      APPEND : cl_gui_alv_tree=>mc_fc_print_back      TO gt_toolbar ,
               cl_gui_alv_tree=>mc_fc_save_variant    TO gt_toolbar ,
               cl_gui_alv_tree=>mc_fc_current_variant TO gt_toolbar ,
               cl_gui_alv_tree=>mc_fc_load_variant    TO gt_toolbar ,
               cl_gui_alv_tree=>mc_fc_calculate       TO gt_toolbar .


      CALL METHOD gr_alv_tree->set_table_for_first_display
        EXPORTING
          is_hierarchy_header  = gs_header
          it_toolbar_excluding = gt_toolbar[]
        CHANGING
          it_fieldcatalog      = gt_fcat[]
          it_outtab            = gt_out[].


ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0101 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

*  SET PF-STATUS 'GUI'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

*  CASE sy-ucomm.
*    WHEN 'BACK' OR 'EXIT' OR  'CANC'.
*      LEAVE TO SCREEN 0 .
*    WHEN OTHERS.
*  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_IMAGE OUTPUT
*&---------------------------------------------------------------------*
MODULE set_image OUTPUT.

  DATA: lr_mime_rep TYPE REF TO if_mr_api.
  DATA: lv_content  TYPE xstring.
  DATA: p_path TYPE string VALUE 'SAP/PUBLIC/BUDGET2.PNG'.
  DATA: lt_data TYPE STANDARD TABLE OF x255.
  DATA: BEGIN OF graphic_table ,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: gt_line        LIKE TABLE OF graphic_table,
        url(255)       TYPE c,
        l_graphic_conv TYPE i,
        l_graphic_offs TYPE i,
        graphic_size   TYPE i,
        l_graphic_xstr TYPE xstring.

    CHECK container2 IS INITIAL .
*   create container for alv-tree
    CREATE OBJECT container2
      EXPORTING
        container_name = 'CONT_IMG' .

    CREATE OBJECT splitter
      EXPORTING
        parent  = container2
        rows    = 1
        columns = 1.
*        columns = 2.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
        RECEIVING
        container = container_2.
*
    " Picture control nesnesini oluştur
    CREATE OBJECT go_picture
      EXPORTING
        parent = container_2.

    lr_mime_rep = cl_mime_repository_api=>if_mr_api~get_api( ).

    lr_mime_rep->get(
      EXPORTING
        i_url      = p_path
      IMPORTING
        e_content = lv_content
      EXCEPTIONS
        not_found = 3 ).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = lv_content
      TABLES
        binary_tab = lt_data.

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type    = 'IMAGE'
        subtype = 'BMP'
      TABLES
        data    = lt_data
      CHANGING
        url     = url.

    go_picture->load_picture_from_url_async(
      EXPORTING
        url   = url
      EXCEPTIONS
        error = 1
    ).

    CALL METHOD go_picture->set_display_mode
      EXPORTING
        display_mode = cl_gui_picture=>display_mode_fit_center.
ENDMODULE.
