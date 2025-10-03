FUNCTION /dsl/hr80_fg002_03.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA : ls_shlp_tab        TYPE shlp_descr.
  DATA : ls_selopt          TYPE ddshselopt.
  DATA : record_tab_temp    TYPE TABLE OF seahlpres,
         l_record_tab_line  LIKE seahlpres.
  DATA : lt_t001p TYPE TABLE OF t001p WITH HEADER LINE .
  DATA: BEGIN OF gt_data OCCURS 0,
          molga       TYPE molga    ,
          sprsl       TYPE spras  ,
          lgart       TYPE lgart  ,
          lgart_t     TYPE lgtxt  ,
          infty       TYPE infty    ,
          begda       TYPE begda  ,
          endda       TYPE endda  ,
        END OF gt_data.
  RANGES : lr_molga       FOR t001p-molga   ,
           lr_infty       FOR t512z-infty   ,
           lr_moabw       FOR t001p-moabw   ,
           lr_lgart       FOR t512z-lgart   ,
           lr_lgart_t     FOR t512t-lgtxt   ,
           lr_begda       FOR t554s-begda   ,
           lr_endda       FOR t554s-endda   .
  DATA : lv_spras TYPE spras .
  DATA : lv_molga TYPE molga .

  lv_spras = CONV spras( sy-langu ) .

* EXIT immediately, if you do not want to handle this step
  IF callcontrol-step <> 'SELONE' AND
     callcontrol-step <> 'SELECT' AND
     " AND SO ON
     callcontrol-step <> 'DISP'.
     EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP SELONE  (Select one of the elementary searchhelps)
*"----------------------------------------------------------------------
* This step is only called for collective searchhelps. It may be used
* to reduce the amount of elementary searchhelps given in SHLP_TAB.
* The compound searchhelp is given in SHLP.
* If you do not change CALLCONTROL-STEP, the next step is the
* dialog, to select one of the elementary searchhelps.
* If you want to skip this dialog, you have to return the selected
* elementary searchhelp in SHLP and to change CALLCONTROL-STEP to
* either to 'PRESEL' or to 'SELECT'.
  IF callcontrol-step = 'SELONE'.
*   PERFORM SELONE .........
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF callcontrol-step = 'PRESEL'.
*   PERFORM PRESEL ..........
    EXIT.
  ENDIF.
*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpfull in this
* step.
  IF callcontrol-step = 'SELECT' OR callcontrol-step = 'DISP'.
    READ TABLE shlp_tab INTO ls_shlp_tab WITH KEY shlpname = '/DSL/HR80_LGART'.
    REFRESH lr_infty.


    lr_molga[] = VALUE #( FOR selopt IN ls_shlp_tab-selopt
              WHERE ( shlpfield EQ 'MOLGA' )
                    ( sign    = 'I'
                      option  = 'EQ'
                      low     = selopt-low
                      high    = selopt-high ) ).
    IF lr_molga[] IS NOT INITIAL .
      READ TABLE lr_molga INDEX 1 .
      lv_molga = lr_molga-low .
    ENDIF.

    SELECT * FROM t001p INTO TABLE lt_t001p WHERE molga IN lr_molga[].

    lr_moabw[] = VALUE #( FOR ls_t001p IN lt_t001p
              WHERE ( molga IN lr_molga[] )
                    ( sign    = 'I'
                      option  = 'EQ'
                      low     = ls_t001p-moabw
                      high    = ls_t001p-moabw ) ).
    SORT lr_moabw ASCENDING .
    DELETE ADJACENT DUPLICATES FROM lr_moabw .

    lr_infty[] = VALUE #( FOR selopt IN ls_shlp_tab-selopt
              WHERE ( shlpfield EQ 'INFTY' )
                    ( sign    = 'I'
                      option  = 'EQ'
                      low     = selopt-low
                      high    = selopt-high ) ).
    IF lr_infty[] IS INITIAL .
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = '0014' ) TO lr_infty.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = '0015' ) TO lr_infty.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = '2010' ) TO lr_infty.
      APPEND VALUE #( sign = 'I' option = 'EQ'  low = '2001' ) TO lr_infty.
    ENDIF.

    lr_lgart[] = VALUE #( FOR selopt IN ls_shlp_tab-selopt
              WHERE ( shlpfield EQ 'LGART' )
                    ( sign    = 'I'
                      option  = 'EQ'
                      low     = selopt-low
                      high    = selopt-high ) ).


    SELECT t1~molga
           t2~sprsl
           t1~infty
           t1~lgart
           t2~lgtxt AS lgart_t
           t1~begda
           t1~endda
          FROM t512z AS t1
          INNER JOIN t512t AS t2
              ON    t2~molga EQ t1~molga
                AND t2~lgart EQ t1~lgart
        INTO CORRESPONDING FIELDS OF TABLE gt_data
        WHERE t1~molga IN lr_molga[]
          AND t2~sprsl EQ lv_spras
          AND t1~infty IN lr_infty[]
          AND t1~lgart IN lr_lgart[]
          AND t1~begda LE sy-datum
          AND t1~endda GE sy-datum.

    SELECT
          FROM t554s AS t1
          INNER JOIN t554t AS t2
              ON    t2~awart EQ t1~subty
      FIELDS
           t2~sprsl,
            t1~subty AS lgart     ,
           t2~atext AS lgart_t   ,
           CAST( '2001' AS CHAR( 4 ) )  AS infty,
           t1~begda              ,
           t1~endda
        WHERE t1~subty IN @lr_lgart[]
          AND t1~moabw IN @lr_moabw[]
          AND t2~sprsl EQ @lv_spras
          AND t1~begda LE @sy-datum
          AND t1~endda GE @sy-datum
        APPENDING CORRESPONDING FIELDS OF TABLE @gt_data.

    SORT gt_data ASCENDING BY infty lgart .
    CLEAR gt_data .
    gt_data-molga = lv_molga.
    MODIFY gt_data TRANSPORTING molga WHERE molga = space.

    DELETE ADJACENT DUPLICATES FROM gt_data COMPARING infty lgart.
    DELETE gt_data WHERE sprsl NE lv_spras .

    DATA : l_fname TYPE dfies-lfieldname,
           w_fields TYPE dfies .
    LOOP AT shlp_tab.
      LOOP AT shlp_tab-fielddescr INTO w_fields.
        l_fname = w_fields-fieldname.
        CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
          EXPORTING
            parameter               = w_fields-fieldname
           fieldname               = l_fname
          TABLES
            shlp_tab                = shlp_tab
            record_tab              = record_tab
            source_tab              = gt_data[]
          CHANGING
            shlp                    = shlp
            callcontrol             = callcontrol
         EXCEPTIONS
           parameter_unknown       = 1
           OTHERS                  = 2
                  .
      ENDLOOP.

    ENDLOOP.

    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
* Standard function modules F4UT_PARAMETER_VALUE_GET and
* F4UT_PARAMETER_RESULTS_PUT may be very helpfull in this step.
  IF callcontrol-step = 'DISP'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB SHLP_TAB
*                           CHANGING SHLP CALLCONTROL.
    EXIT.
  ENDIF.
ENDFUNCTION.
