FUNCTION /dsl/hr80_fg002_02.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PAYROLL_AREA) TYPE  ABKRS OPTIONAL
*"     VALUE(PAYROLL_PERIOD) TYPE  PABRP OPTIONAL
*"     VALUE(PAYROLL_YEAR) TYPE  PABRJ OPTIONAL
*"     VALUE(PAYROLL_TYPE) TYPE  PAYTY OPTIONAL
*"     VALUE(PAYROLL_ID) TYPE  PAYID OPTIONAL
*"     VALUE(PAYROLL_DATE) TYPE  BONDT OPTIONAL
*"     VALUE(PAYROLL_OCRSN) TYPE  PAY_OCRSN OPTIONAL
*"     VALUE(PROGRAM_NAME) TYPE  PROGNAME OPTIONAL
*"     VALUE(OFF_CYCLE) TYPE  PAYTY DEFAULT SPACE
*"     VALUE(LOG_MEM_KEY) OPTIONAL
*"     VALUE(COUNTRY_GRP) TYPE  MOLGA OPTIONAL
*"     VALUE(TST_ON) TYPE  TST_ON DEFAULT 'X'
*"     VALUE(IMP_BUFF) TYPE  EXP_BUFF DEFAULT SPACE
*"     VALUE(OBJ_IPREL) TYPE REF TO  CL_HRPAY99_FILL_INFOTYPES OPTIONAL
*"     VALUE(COSTPLANNING) TYPE  SW_COSTPLAN DEFAULT SPACE
*"     VALUE(PRT_PROT) TYPE  PRT_PROT DEFAULT SPACE
*"     VALUE(SEL_ALL_AREAS) TYPE  RP_XFELD DEFAULT SPACE
*"     VALUE(SCHEMA) TYPE  SCHEM OPTIONAL
*"     VALUE(RUECK_AB) TYPE  STAG_RP OPTIONAL
*"     VALUE(PA03_OFF) TYPE  XFELD DEFAULT SPACE
*"     VALUE(I_MOLGA) TYPE  /DSL/HR80_T001-MOLGA
*"     VALUE(I_GRPID) TYPE  /DSL/HR80_T003-GRPID
*"     VALUE(I_VRSID) TYPE  /DSL/HR80_T003-VRSID
*"     VALUE(I_STATU) TYPE  /DSL/HR80_T003-STATU OPTIONAL
*"     VALUE(I_PERNR) TYPE  /DSL/HR80_T010-PERNR OPTIONAL
*"     VALUE(I_RFPER) TYPE  /DSL/HR80_T010-RFPER OPTIONAL
*"  TABLES
*"      EMPLOYEE_NUMBERS TYPE  PAY99_T_PAY_SIM_PERNR
*"      ADVANCE_PERIODS OPTIONAL
*"      BUFFER OPTIONAL
*"      BUFFER_DIRECTORY OPTIONAL
*"      DELETE_PCLX OPTIONAL
*"      PS_OPER OPTIONAL
*"      PT_BEFORE_IMAGE OPTIONAL
*"      PT_BEFORE_IMAGE_PCLX OPTIONAL
*"      PT_PAYR_BUFFER OPTIONAL
*"  EXCEPTIONS
*"      PROGRAM_NOT_EXIST
*"      VARIANT_NOT_EXIST
*"      MISSING_PARAMETER
*"      WRONG_PARAMETER
*"      WRONG_COUNTRY_GROUP
*"----------------------------------------------------------------------
*

  TYPES: BEGIN OF ty_s_sim_key,
           repid     TYPE progname,
           ident(10) TYPE c,
         END OF ty_s_sim_key.

  CONSTANTS: lc_calc_prefix(4) TYPE c VALUE 'CALC', "defines all payroll drivers
             lc_initial_retcd  TYPE sysubrc VALUE 99.

  STATICS:   st_whitelist TYPE string_hashed_table.

  DATA: lo_molga           TYPE REF TO cl_hr_molga ##NEEDED,
        lx_molga_exception TYPE REF TO cx_hr_molga,
        lx_area_exception  TYPE REF TO cx_hrpy_payroll_area,
        lo_payarea         TYPE REF TO cl_hr_payroll_area,
        lv_calc_date       TYPE datum,
        lv_calcname        TYPE programm,
        lv_sname           TYPE smodn,
        lv_sname_search    TYPE smodn,
        lv_num_of_sname    TYPE i,
        lt_sname           TYPE TABLE OF smodn,
        lt_repid           TYPE TABLE OF string,
        lv_lprog_security  TYPE secu,
        ls_params          TYPE rsparams,
        lt_params          TYPE TABLE OF rsparams.

  DATA: lt_empty_pnppernr  TYPE RANGE OF p_pernr,
        wa_employee_number LIKE LINE OF employee_numbers,
        lt_lrange_db_index TYPE RANGE OF p_pernr,
        wa_db_index_range  LIKE LINE OF lt_lrange_db_index,
        lt_lrange_pay_area TYPE RANGE OF abkrs,
        wa_pay_area_range  LIKE LINE OF lt_lrange_pay_area,
        lt_sim_pernr       TYPE TABLE OF pay_sim_pernr,
        ls_sim_key         TYPE ty_s_sim_key,
        lv_pnptimr9        TYPE timr9,
        lv_pnptimra        TYPE timra,
        lv_pnptimed        TYPE pcce_eval_period,
        is_ce              TYPE flag VALUE abap_false,
        lt_head            TYPE TABLE OF rhead,
        wa_head            LIKE LINE OF lt_head,
        lv_empty           TYPE string,
        lv_country_grp     TYPE molga,
        lv_test_molga      TYPE molga.

  "objects for infotype simulation
  DATA: lo_send_it TYPE REF TO cl_hrpay99_send_infotypes,
        lt_iprel   TYPE hrpay99_iprel_table.

*##--BEGIN--##
  CLEAR lt_empty_pnppernr.

* Check the consitency of the input parameters
  IF     country_grp  IS INITIAL
     AND program_name IS INITIAL.
    "program_name is obsolete!
    MESSAGE e006(hrpay99calc)
       WITH 'COUNTRY_GRP/PROGRAM_NAME'
       RAISING wrong_parameter.
  ENDIF.

  IF  payroll_area IS INITIAL.
    MESSAGE e005(hrpay99calc)
       WITH 'PAYROLL_AREA'
       RAISING  missing_parameter.
  ENDIF.

  IF off_cycle CN ' X'.         "no on-demand-check = SPACE or X
    MESSAGE e006(hrpay99calc)
       WITH 'OFF_CYCLE'
       RAISING wrong_parameter.
  ENDIF.

  IF (    off_cycle = 'X'            "we have an on-demand-check
      AND payroll_date IS INITIAL ). "so BONDT is needed
    MESSAGE e006(hrpay99calc)
       WITH 'OCC_CYCLE/PAYROLL_DATE'
       RAISING wrong_parameter.
  ENDIF.

  IF payroll_type = space. "regular period
    IF (   payroll_period IS INITIAL
        OR payroll_year   IS INITIAL ).
      MESSAGE e005(hrpay99calc)
         WITH 'PAYROLL_PERIOD, PAYROLL_YEAR'
         RAISING  missing_parameter.
    ENDIF.
    IF payroll_id NE space.  "no ID needed
      MESSAGE e006(hrpay99calc)
         WITH 'PAYROLL_ID'
         RAISING wrong_parameter.
    ENDIF.
    IF (   off_cycle IS INITIAL  "we don't have an on-demand-check
       AND payroll_date IS NOT INITIAL ).
      "so we dont't need the bondt
      MESSAGE e006(hrpay99calc)
         WITH 'OFF_CYCLE/PAYROLL_DATE'
         RAISING wrong_parameter.
    ENDIF.

  ELSE. "off-cycle run
    IF payroll_date IS INITIAL.
      MESSAGE e005(hrpay99calc)
         WITH 'PAYROLL_DATE'
         RAISING  missing_parameter.
    ENDIF.
    IF (   payroll_period IS NOT INITIAL
        OR payroll_year   IS NOT INITIAL ).
      MESSAGE e006(hrpay99calc)
         WITH 'PAYROLL_PERIOD/PAYROLL_YEAR'
         RAISING wrong_parameter.
    ENDIF.
  ENDIF.
  IF tst_on = abap_false.  "productive run
    IF pa03_off EQ 'X'.
      MESSAGE e006(hrpay99calc)
         WITH 'PA03_OFF'
         RAISING wrong_parameter.
    ENDIF.
    IF costplanning EQ 'X'.
      MESSAGE e006(hrpay99calc)
         WITH 'COSTPLANNING'
         RAISING wrong_parameter.
    ENDIF.
  ENDIF.

* Get relevant date for payroll
  lv_calc_date = sy-datum. "default value
  IF payroll_type = space. "regular payroll run
    TRY.
        lo_payarea = cl_hr_payroll_area=>get_instance( payroll_area ).
      CATCH cx_hrpy_payroll_area INTO lx_area_exception.
        MESSAGE lx_area_exception TYPE 'E'
          RAISING wrong_parameter.
    ENDTRY.
    TRY.
        CALL METHOD lo_payarea->get_period_info
          EXPORTING
            imp_pabrj = payroll_year
            imp_pabrp = payroll_period
          IMPORTING
            exp_endda = lv_calc_date.
      CATCH cx_hrpy_payroll_area INTO lx_area_exception.
        MESSAGE lx_area_exception TYPE 'E'
          RAISING wrong_parameter.
    ENDTRY.
  ELSE. "off-cycle run
    lv_calc_date = payroll_date.
  ENDIF.

* Get payroll driver and check if it is allowed to start it
  IF country_grp IS NOT INITIAL.
    TRY.
        lo_molga  = cl_hr_molga=>get_instance( country_grp ).
      CATCH cx_hr_molga INTO lx_molga_exception.
        MESSAGE lx_molga_exception TYPE 'E'
          RAISING wrong_country_group.
    ENDTRY.

    CONCATENATE lc_calc_prefix country_grp INTO lv_sname.
    CALL FUNCTION 'HRCCE_GET_REPORTNAME'
      EXPORTING
        smodn             = lv_sname
        date              = lv_calc_date "this is the end date of the
        "regular period or the off-cylce run date
      IMPORTING
        reportname        = lv_calcname
      EXCEPTIONS
        no_entry_in_table = 1
        OTHERS            = 2.
    IF sy-subrc <> 0. "program could not be found
      MESSAGE e005(hrpay99calc)
         WITH 'COUNTRY_GRP'
         RAISING  missing_parameter.
    ENDIF.

    IF program_name IS NOT INITIAL.
      IF program_name <> lv_calcname.
        MESSAGE e003(hrpay99calc)
          WITH program_name
               country_grp
          RAISING program_not_exist.
      ENDIF.
    ENDIF.
  ENDIF.

  IF country_grp IS NOT INITIAL.
    lv_country_grp = country_grp.
  ELSE.                            "program_name is filled!
    "determine country grouping if possible
    CONCATENATE lc_calc_prefix '__' INTO lv_sname_search.

    SELECT sname FROM t596f INTO TABLE lt_sname
      WHERE     sname LIKE lv_sname_search
            AND pgmna = program_name.
    SELECT sname FROM t77cce_prg_prgce APPENDING TABLE lt_sname
      WHERE     sname LIKE lv_sname_search
            AND prgname_ce = program_name.
    SORT lt_sname.
    DELETE ADJACENT DUPLICATES FROM lt_sname.
    DESCRIBE TABLE lt_sname LINES lv_num_of_sname.
    IF lv_num_of_sname = 1.
      READ TABLE lt_sname INDEX 1 INTO lv_sname.
      SPLIT lv_sname AT lc_calc_prefix INTO lv_empty lv_country_grp.
    ELSE.
      CLEAR lv_country_grp. "country grouping not unique!
    ENDIF.

    "check if program_name is a payroll driver
    IF st_whitelist IS INITIAL.
      "we select without date, since all prgrams entered for CALC__ in
      "the tables should be payroll programs
      SELECT pgmna FROM t596f INTO TABLE lt_repid
        WHERE sname LIKE lv_sname_search.
      SELECT prgname_ce FROM t77cce_prg_prgce APPENDING TABLE lt_repid
        WHERE sname LIKE lv_sname_search.
      "Notice there might be CALCMmm (like in RPCSC000).
      "Since the swiss RPCALCM0 is running out and since the CALCM-entry
      "is wrong design (the only payrolldriver should be with CALCmm)
      "we ignore this for now.
      SORT lt_repid.
      DELETE ADJACENT DUPLICATES FROM lt_repid.
      st_whitelist = lt_repid.
    ENDIF.
    FREE lt_repid.

    TRY.
        lv_calcname = cl_abap_dyn_prg=>check_whitelist_tab(
                            val       = program_name
                            whitelist = st_whitelist ).

      CATCH cx_abap_not_in_whitelist.
        MESSAGE e003(hrpay99calc)
          WITH lv_calcname
               '??'
          RAISING program_not_exist.
        RETURN.
    ENDTRY.
  ENDIF.

  SELECT SINGLE secu FROM trdir INTO lv_lprog_security      "EHA1291203
    WHERE name = lv_calcname.
  IF lv_lprog_security <> space.
    AUTHORITY-CHECK OBJECT 'S_PROGRAM'
                    ID     'P_GROUP'  FIELD lv_lprog_security
                    ID     'P_ACTION' FIELD 'SUBMIT'.
    IF sy-subrc <> 0.
      MESSAGE e645(db) WITH lv_lprog_security lv_calcname.
    ENDIF.
  ENDIF.


* handle infotype data for simulation
  IF NOT obj_iprel IS INITIAL.
    lt_iprel = obj_iprel->get_iprel( ).
    CREATE OBJECT lo_send_it. "Object for infotype simulation
    CALL METHOD lo_send_it->set_iprel( lt_iprel ).
    CALL METHOD lo_send_it->send_all. "send infotype object to memory
  ENDIF.

* check whether report is a CE report                   "XFG note 551218
  LOAD REPORT lv_calcname PART 'HEAD' INTO lt_head.
  IF sy-subrc = 0.
    READ TABLE lt_head INDEX 1 INTO wa_head.
    IF wa_head-ldbname = 'PNPCE'.
      is_ce = abap_true.
    ENDIF.
  ENDIF.

* Fill parameters for submit.

  "Fill the index for logical database driver PNP.
  CLEAR lt_lrange_db_index.
  SORT employee_numbers.
  LOOP AT employee_numbers INTO wa_employee_number.
    wa_db_index_range-low = wa_employee_number-pernr.
    APPEND wa_db_index_range TO lt_lrange_db_index.

    wa_employee_number-retcd = lc_initial_retcd.
    MODIFY employee_numbers FROM wa_employee_number.
  ENDLOOP.

* Export the set of employee numbers to be calculated to memory (will be
* picked up by payroll driver, see include RPCHRT09).
  ls_sim_key-ident = 'SIMULATION'.
  ls_sim_key-repid = lv_calcname.
  EXPORT sim_pernr FROM employee_numbers
  TO MEMORY ID ls_sim_key.

  IF sel_all_areas EQ space.
    wa_pay_area_range-low = payroll_area.
    wa_pay_area_range-sign = 'I'.
    wa_pay_area_range-option = 'EQ'.
    APPEND wa_pay_area_range TO lt_lrange_pay_area.
  ENDIF.

  "Fill optional parameters                               "RSC note 1372226
  IF  NOT schema IS INITIAL.
    CLEAR ls_params.
    ls_params-selname = 'SCHEMA'.
    ls_params-kind    = 'P'.
    ls_params-low     = schema.
    APPEND ls_params TO lt_params.
  ENDIF.
  IF prt_prot IS SUPPLIED.
    CLEAR ls_params.
    ls_params-selname = 'PRT_PROT'.
    ls_params-kind    = 'P'.
    ls_params-low     = prt_prot.
    APPEND ls_params TO lt_params.
    IF prt_prot = space.
      "we don't need a log variant in case no log is needed (see note 885932)
      CLEAR ls_params.
      ls_params-selname = 'P_VANAM'.
      ls_params-kind    = 'P'.
      ls_params-low     = '              '.
      APPEND ls_params TO lt_params.
    ENDIF.
  ENDIF.
  IF NOT rueck_ab IS INITIAL.                             "RSCN1728497
    CLEAR ls_params.
    ls_params-selname = 'RUECK-AB'.
    ls_params-kind    = 'P'.
    ls_params-low     = rueck_ab.
    APPEND ls_params TO lt_params.
  ENDIF.


**********************************************************************
**********************************************************************
**********************************************************************
  "<<--------Bütçe parametrelerini gönder ------>>
  " Burası çalışabilmesi için aşağıdaki include bordro fonksiyonu için
  " oluşturulan zli include içinde tanımlanmalıdır.
*  /DSL/HR80_CALC Bordro programı include (Seçim ekranı eklendi)

  DEFINE append_param.
  IF NOT &1 IS INITIAL.
    CLEAR ls_params.
    ls_params-selname = &2.
    ls_params-kind    = 'P'.
    ls_params-low     = &1.
    APPEND ls_params TO lt_params.
  ENDIF.
  END-OF-DEFINITION.

  append_param :  i_molga 'P_MOLGA',
                  i_grpid 'P_GRPID',
                  i_vrsid 'P_VRSID',
                  i_statu 'P_STATU',
                  i_pernr 'P_PERNR',
                  i_rfper 'P_RFPER'.
  "<<--------END CODE------>>
**********************************************************************
**********************************************************************
**********************************************************************




  "Determine whether period has to be entered (in case of regular runs)
  "or not (in case of adjustment, bonus, ... runs).
  IF payroll_type = space. "regular payroll run
    lv_pnptimra = 'X'.
    lv_pnptimed = '2'. "= payroll_period; see include DBPNPCE_PERIOD_CONSTANTS
  ELSE. "off-cycle run
    lv_pnptimr9 = 'X'.
    lv_pnptimed = '1'. "= act_payroll_period; see include DBPNPCE_PERIOD_CONSTANTS
  ENDIF.
  IF is_ce = abap_true.
    CLEAR ls_params.
    ls_params-selname = 'PNPTIMED'.
    ls_params-kind    = 'P'.
    ls_params-low     = lv_pnptimed.
    APPEND ls_params TO lt_params.
  ELSE.
    CLEAR ls_params.
    ls_params-selname = 'PNPTIMR9'.
    ls_params-kind    = 'P'.
    ls_params-low     = lv_pnptimr9.
    APPEND ls_params TO lt_params.
    CLEAR ls_params.
    ls_params-selname = 'PNPTIMRA'.
    ls_params-kind    = 'P'.
    ls_params-low     = lv_pnptimra.
    APPEND ls_params TO lt_params.
  ENDIF.

* Start the payroll driver.
  SUBMIT (lv_calcname) EXPORTING LIST TO MEMORY
                      AND RETURN
                      WITH SELECTION-TABLE lt_params
                      WITH pnpindex IN lt_lrange_db_index
                      WITH pnpxabkr EQ payroll_area
                      WITH pnppabrp EQ payroll_period
                      WITH pnppabrj EQ payroll_year
                      WITH pnpabkrs IN lt_lrange_pay_area
                      WITH tst_on EQ tst_on           "XAIAHRK038127
                      WITH pa03_off EQ pa03_off
                      WITH payty EQ payroll_type
                      WITH payid EQ payroll_id
                      WITH bondt EQ payroll_date
                      WITH ocrsn EQ payroll_ocrsn          "XAI
                      WITH exp_buff EQ 'X'           "XUJAHRK002341
                      WITH imp_buff EQ imp_buff
                      WITH set_nib EQ 'X'
                      WITH set_odc EQ off_cycle
                      WITH upd_ps EQ ' '
                      WITH brk_on EQ ' '
                      WITH brk_sc EQ ' '
                      WITH brk_off EQ 'X'
                      WITH ecalled EQ 'X'
                      WITH costplan EQ costplanning  "WOGL9CK040779
                      WITH plgmemky EQ log_mem_key   "XDOAHRK003361
                      WITH advance = advance_periods[]."XAIAHRK011846

* Import the data from the payroll run (see include RPCHRT09)
  IMPORT sim_pernr        TO lt_sim_pernr
         country_grouping TO lv_test_molga
  FROM MEMORY ID ls_sim_key.
  TRY.
      lo_molga  = cl_hr_molga=>get_instance( lv_test_molga ).
    CATCH cx_hr_molga INTO lx_molga_exception.
      MESSAGE lx_molga_exception TYPE 'E'
        RAISING wrong_country_group.
  ENDTRY.
  IF      lv_country_grp IS NOT INITIAL
      AND lv_test_molga <> lv_country_grp. "result differs from parameter

    MESSAGE e007(hrpay99calc)
       WITH lv_test_molga
            lv_country_grp
            lv_calcname
       RAISING wrong_country_group.
  ENDIF.

* Import the payroll results from memory.
  "for export see payroll driver, FORM update_databases and FORM new_ablehnung
  PERFORM fill_import_export_key USING lv_calcname
                                       0.
  PERFORM import_buffer_all.

* Populate the results.
  employee_numbers[] = lt_sim_pernr[].
  buffer[] = tbuff[].
  buffer_directory[] = buffer_dir[].
  delete_pclx[] = del_pclx_tab[].
  ps_oper[] = psoper[].
  pt_before_image[] = before_image[].
  pt_before_image_pclx[] = before_image_pclx[].
  pt_payr_buffer[] = payr_buffer[].

ENDFUNCTION.
