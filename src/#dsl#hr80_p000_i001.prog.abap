*&---------------------------------------------------------------------*
*& Include          ZNCPRH_P006_I001
*&---------------------------------------------------------------------*
CLASS alv_class DEFINITION DEFERRED.
TABLES : icon,sscrfields.
DATA : gv_subrc TYPE sy-subrc.
FIELD-SYMBOLS : <fs>       TYPE any,
                <fs2>      TYPE any,
                <tabname>  TYPE table,
                <fcatname> TYPE table,
                <layname>  TYPE any.


DATA : BEGIN OF gs_ise_alim.
         INCLUDE STRUCTURE /dsl/hr80_s_alim.
       DATA: END OF gs_ise_alim,
       gt_ise_alim LIKE TABLE OF gs_ise_alim.

DATA: gt_ise_alim_excel TYPE TABLE OF /dsl/hr80_s_alim,
      gs_ise_alim_excel TYPE /dsl/hr80_s_alim.

DATA : BEGIN OF gs_isten_cikis.
         INCLUDE STRUCTURE /dsl/hr80_s_cikis.
       DATA: END OF gs_isten_cikis,
       gt_isten_cikis LIKE TABLE OF gs_isten_cikis.

DATA: gt_isten_cikis_excel TYPE TABLE OF /dsl/hr80_s_cikis,
      gs_isten_cikis_excel TYPE /dsl/hr80_s_cikis.


DATA : gt_fcat   TYPE  slis_t_fieldcat_alv  WITH HEADER LINE,
       gs_fcat   LIKE  gt_fcat,
       gs_layout TYPE  slis_layout_alv,
       gt_top    TYPE  slis_t_listheader.

DATA : gr_alv TYPE REF TO alv_class.

CONSTANTS : gc_fc03 TYPE sy-ucomm VALUE 'FC03'.
CONSTANTS : con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
            con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

DATA : BEGIN OF  file OCCURS 0 ,
         field(400) ,
       END OF file .

DATA : gt_intern  LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_file  LIKE rlgrap-filename  .
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
PARAMETERS : rb1 TYPE char1 RADIOBUTTON GROUP rb DEFAULT 'X',
             rb2 TYPE char1 RADIOBUTTON GROUP rb.
SELECTION-SCREEN END OF BLOCK b2.
