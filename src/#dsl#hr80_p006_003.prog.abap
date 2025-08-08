*----------------------------------------------------------------------*
***INCLUDE /DSL/HR80_P006_003.
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

  SELECT * FROM /dsl/hr80_t011
    WHERE grpid IN @s_grpid[]
      AND vrsid IN @s_vrsid[]
*    ORDER BY molga grpid vrsid DESCENDING
    INTO TABLE @DATA(lt_t011)
      UP TO 1 ROWS
     .
*  IF lt_t011[] IS NOT INITIAL .
*    MESSAGE i027 DISPLAY LIKE 'E'.
*    cv_check = 'X'.
*  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM at_selection_screen .
  go_alv->create_fcat( ).


  IF sscrfields-ucomm = 'CHAL'.
    CALL FUNCTION 'HR_PL_CHANGE_VAR_ON_SEL_SCREEN'
      EXPORTING
        imp_procl     = 'C'
        imp_schema    = p_schema
      CHANGING
        chan_var_name = p_vari
      EXCEPTIONS
        missing_info  = 1
        schema_error  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

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
      AND pa1_endda   GE s_datum-low .

  SORT go_alv->gt_main ASCENDING BY pernr pa1_begda.

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

*  PERFORM row_record USING mode lt_rows .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_MAIN
*&---------------------------------------------------------------------*
FORM save_main .

*  DATA : ls_t010  TYPE /dsl/hr80_t010 .
*  DATA : lt_t010  TYPE TABLE OF /dsl/hr80_t010 .
  DATA : lt_t011  TYPE TABLE OF /dsl/hr80_t011 .
  DATA : lr_pernr TYPE RANGE OF /dsl/hr80_t011-pernr WITH HEADER LINE .
  DATA : number TYPE numc10.
*

  REFRESH lt_t011.
  REFRESH : lr_pernr.
  lr_pernr[] = VALUE #( FOR ls_main IN go_alv->gt_main
              WHERE ( oprtn = 'N' OR oprtn = 'C' )
                    ( option = 'EQ'
                      sign   = 'I'
                      low    = ls_main-pernr )  ) .


  lt_t011[] = VALUE #( FOR ls_11 IN go_alv->gt_t011
                       WHERE ( pernr IN lr_pernr[] )
                      ( ls_11 ) ).
  IF lt_t011[] IS NOT INITIAL .
    MODIFY /dsl/hr80_t011 FROM TABLE lt_t011[].
  ENDIF.
  IF sy-subrc EQ 0 .
    MESSAGE s020 DISPLAY LIKE 'I' .
    CLEAR  :go_alv->gs_main.
    MODIFY go_alv->gt_main FROM go_alv->gs_main
          TRANSPORTING oprtn
            WHERE pernr IN lr_pernr[].
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
      WHEN OTHERS.
        go_alv->modify_target_value(  fname  = ls_fcat-fieldname
                              targt  = 'KEY'
                              zvalue = space ).

    ENDCASE.

    go_alv->modify_target_value(  fname  = ls_fcat-fieldname
                          targt  = 'COLDDICTXT'
                          zvalue = 'L' ).
    go_alv->modify_target_value(  fname  = ls_fcat-fieldname
                          targt  = 'COL_OPT'
                          zvalue = 'X' ).
  ENDLOOP.

  CLEAR ls_fcat .
  ls_fcat-fieldname = 'MSG'.
  ls_fcat-col_pos = 1.
  COLLECT ls_fcat INTO go_alv->mt_fcat.
  go_alv->modify_target_value(  fname  = 'MSG' targt  = 'TEXT'        zvalue = 'Açıklama' ).
  go_alv->modify_target_value(  fname  = 'MSG' targt  = 'COLDDICTXT'  zvalue = 'L' ).


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

        WHEN OTHERS.
      ENDCASE.

      <ls_main>-color      = 'C310'.

    ENDIF.
  ENDLOOP.

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


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALC_SIMU
*&---------------------------------------------------------------------*
FORM calc_simu .
  DATA : lr_pers            TYPE RANGE OF persno WITH HEADER LINE  .
  DATA : lr_abkrs            TYPE RANGE OF abkrs WITH HEADER LINE  .
  DATA : lt_emp_num         TYPE hrpaydepft_pay_sim_pernr.
  RANGES: advance FOR pc261-fpper.
  DATA: l_pay_reader TYPE REF TO cl_hr_pay_access.
  DATA :
      tbuff             TYPE cl_hrpclx_buffer=>ty_t_buffer        WITH HEADER LINE,
      buffer_dir        TYPE cl_hrpclx_buffer=>ty_t_directory     WITH HEADER LINE,
      del_pclx_tab      TYPE cl_hrpclx_buffer=>ty_t_del_directory WITH HEADER LINE,
      before_image_pclx TYPE cl_hrpclx_buffer=>ty_t_buffer        WITH HEADER LINE.

  DATA: BEGIN OF psoper OCCURS 10.
          INCLUDE STRUCTURE prelp AS prelp.
  DATA: opera.
  DATA: END OF psoper.
  DATA: BEGIN OF before_image OCCURS 10.
          INCLUDE STRUCTURE prelp.
  DATA: END OF before_image.
* Check register of FI.
  DATA: BEGIN OF payr_buffer OCCURS 5.
          INCLUDE STRUCTURE payr_fi.
  DATA:   oseqnr LIKE payr_fi-seqnr.
  DATA: END OF payr_buffer.

  DATA : buffer TYPE  hrpay_buffer.

  DATA : BEGIN OF ls_key,
            uname   TYPE sy-uname,
            datum   TYPE sy-datum,
         END OF ls_key.


  lr_abkrs[] = VALUE #( FOR ls_abkrs IN go_alv->gt_main
*              WHERE ( pernr LT '90000000' )
                    ( option = 'EQ'
                      sign   = 'I'
                      low    = ls_abkrs-abkrs )  ) .
  SORT lr_abkrs ASCENDING .
  DELETE ADJACENT DUPLICATES FROM lr_abkrs.

  LOOP AT lr_abkrs.

    " Log keys
    CLEAR : ls_key.
    ls_key-uname = sy-uname.
    ls_key-datum = sy-datum.

    " Bordro alt birimine göre sicilleri al
    REFRESH : lr_pers.
    lr_pers[] = VALUE #( FOR ls_main IN go_alv->gt_main
*                WHERE ( pernr LT '90000000' AND
                WHERE ( pernr LT go_alv->gmc_dummy AND
                        abkrs EQ lr_abkrs-low )
                      ( option = 'EQ'
                        sign   = 'I'
                        low    = ls_main-pernr )  ) .
    IF lr_pers[] IS NOT INITIAL .
      lt_emp_num[] = VALUE #( FOR ls_emp IN go_alv->gt_main
                  WHERE ( pernr LT go_alv->gmc_dummy AND
                          abkrs EQ lr_abkrs-low )
                        ( pernr    = ls_emp-pernr )  ) .
      CALL FUNCTION 'HR_PCLX_INIT_BUFFER'.

*      IF lr_pers[] IS NOT INITIAL .
*        " PA0003 change
*        SUBMIT rputrbk0
*                  WITH pnppernr IN lr_pers[]
*                  WITH p_test   EQ abap_false
*                  WITH p_adval  EQ abap_false
*                  WITH p_prdat  EQ abap_true
*                  WITH d_prdat  EQ go_alv->gs_t003-begda
*               EXPORTING LIST TO MEMORY
*                     AND RETURN.
*      ENDIF.

      CLEAR : buffer.
*      CALL FUNCTION 'HR_PAYROLL_SIMULATION'
      CALL FUNCTION '/DSL/HR80_FG002_02'
        EXPORTING
          payroll_area         = lr_abkrs-low
          payroll_period       = go_alv->gs_t003-endda+4(2)
          payroll_year         = go_alv->gs_t003-gjahr
*          selection_variant    = p_vari
          program_name         = 'HTRCALC0'
          log_mem_key          = ls_key
          sel_all_areas        = 'X'
          schema               = p_schema
          rueck_ab             = go_alv->gs_t003-endda

          "<<--------Bütçeden çağırıldığını anlaması için ------>>
*         " /dsl/hr80_t010 tablosundan personel için güncel verileri alınacak
          " personel verilerini  PAlı tablolarına yansıt
          i_molga              = go_alv->gs_t003-molga
          i_grpid              = go_alv->gs_t003-grpid
          i_vrsid              = go_alv->gs_t003-vrsid
          i_statu              = go_alv->gs_t003-statu
*          I_PERNR              = I_PERNR " gerçek sicilde gönderme fakat içerde
*          I_RFPER              = I_RFPER " gerçek sicilde gönderme
          "<<--------END CODE------>>

        TABLES
          employee_numbers     = lt_emp_num[]
          advance_periods      = advance[]
          buffer               = buffer-tbuff[]
          buffer_directory     = buffer-buffer_dir[]
          delete_pclx          = buffer-del_pclx_tab[]
          ps_oper              = psoper[]
          pt_before_image      = before_image[]
          pt_before_image_pclx = buffer-before_image_pclx[]
          pt_payr_buffer       = buffer-payr_buffer[]
        EXCEPTIONS
          program_not_exist    = 1
          variant_not_exist    = 2
          missing_parameter    = 3
          wrong_parameter      = 4
          wrong_country_group  = 5
          unknown_error        = 6
        .

      CASE sy-subrc .
        WHEN 0.
          PERFORM read_simu_message TABLES msgtab[]
                                     USING ls_key.

          LOOP AT lt_emp_num INTO DATA(ls_emp_num).
            READ TABLE go_alv->gt_main ASSIGNING FIELD-SYMBOL(<fs_main>)
                WITH KEY pernr = ls_emp_num-pernr.
            CHECK sy-subrc EQ 0 .
            CASE ls_emp_num-retcd.
              WHEN 99 OR 0 .
                IF ls_emp_num-retcd EQ 99.
                  <fs_main>-color = 'C310'.
                  <fs_main>-msg = TEXT-war.
                  <fs_main>-oprtn = 'N'.
                ELSE.
                  <fs_main>-color = 'C510'.
                  <fs_main>-msg = TEXT-che.
                  <fs_main>-oprtn = 'N'.
                ENDIF.
                PERFORM read_result USING buffer
                                          <fs_main>-pernr
                                 CHANGING <fs_main> .

                go_alv->record_check = 'C'. " Değişiklik kontrolü

              WHEN OTHERS.
                READ TABLE msgtab INTO DATA(ls_msgtab)
                      WITH KEY pernr = <fs_main>-pernr.
                IF sy-subrc EQ 0 .
                  READ TABLE ls_msgtab-msg INTO DATA(ls_msg)
                        WITH KEY message = 'E'.
                  IF sy-subrc EQ 0 .
                    <fs_main>-color = 'C600'.
                    <fs_main>-msg = ls_msg-text1.
                  ELSE.
                    <fs_main>-color = 'C600'.
                    <fs_main>-msg = TEXT-inc.
                  ENDIF.
                ELSE.
                  <fs_main>-color = 'C600'.
                  <fs_main>-msg = TEXT-inc.
                ENDIF.

            ENDCASE.
          ENDLOOP.

        WHEN 1.RAISE program_not_exist  .EXIT.
        WHEN 2.RAISE variant_not_exist  .EXIT.
        WHEN 3.RAISE missing_parameter  .EXIT.
        WHEN 4.RAISE wrong_parameter    .EXIT.
        WHEN 5.RAISE wrong_country_group.EXIT.
        WHEN 6.RAISE others.EXIT.
      ENDCASE.

      CLEAR : ls_key.
      REFRESH: lt_emp_num[],
               advance[],
               tbuff[],
               buffer_dir[],
               del_pclx_tab[],
               psoper[],
               before_image[],
               before_image_pclx[] .
    ENDIF.

**********************************************************************
**********************************************************************
**********************************************************************
    "<<--------DUMMY PERSONNEL------>>
    " burası daha geliştirilecak.

    LOOP AT go_alv->gt_main ASSIGNING <fs_main>
*          WHERE ( pernr GT '90000000' AND
          WHERE ( pernr GT go_alv->gmc_dummy AND
                  abkrs EQ lr_abkrs-low )
          .
      REFRESH lt_emp_num.
      APPEND VALUE #( pernr = <fs_main>-rfper ) TO lt_emp_num.
      CALL FUNCTION 'HR_PCLX_INIT_BUFFER'.

    " Log keys
      CLEAR : ls_key.
      ls_key-uname = sy-uname.
      ls_key-datum = sy-datum.

      CLEAR : buffer.
*      CALL FUNCTION 'HR_PAYROLL_SIMULATION'
      CALL FUNCTION '/DSL/HR80_FG002_02'
        EXPORTING
          payroll_area         = lr_abkrs-low
          payroll_period       = go_alv->gs_t003-endda+4(2)
          payroll_year         = go_alv->gs_t003-gjahr
*          selection_variant    = p_vari
          program_name         = 'HTRCALC0'
          log_mem_key          = ls_key
*          sel_all_areas        = 'X'
          schema               = p_schema
          rueck_ab             = go_alv->gs_t003-endda

          "<<--------Bütçeden çağırıldığını anlaması için ------>>
*         " /dsl/hr80_t010 tablosundan personel için güncel verileri alınacak
          " DUMMY personel verilerini referans sicilin PAlı tablolarına yansıt
          i_molga              = go_alv->gs_t003-molga
          i_grpid              = go_alv->gs_t003-grpid
          i_vrsid              = go_alv->gs_t003-vrsid
          i_statu              = go_alv->gs_t003-statu
          i_pernr              = <fs_main>-pernr
          i_rfper              = <fs_main>-rfper
          "<<--------END CODE------>>

        TABLES
          employee_numbers     = lt_emp_num[]
          advance_periods      = advance[]
          buffer               = buffer-tbuff[]
          buffer_directory     = buffer-buffer_dir[]
          delete_pclx          = buffer-del_pclx_tab[]
          ps_oper              = psoper[]
          pt_before_image      = before_image[]
          pt_before_image_pclx = buffer-before_image_pclx[]
          pt_payr_buffer       = buffer-payr_buffer[]
        EXCEPTIONS
          program_not_exist    = 1
          variant_not_exist    = 2
          missing_parameter    = 3
          wrong_parameter      = 4
          wrong_country_group  = 5
          unknown_error        = 6
        .

      CASE sy-subrc .
        WHEN 0.
          PERFORM read_simu_message TABLES msgtab[]
                                     USING ls_key.

          LOOP AT lt_emp_num INTO ls_emp_num.
*            READ TABLE go_alv->gt_main ASSIGNING <fs_main>
*                WITH KEY pernr = ls_emp_num-pernr.
*            CHECK sy-subrc EQ 0 .
            CASE ls_emp_num-retcd.
              WHEN 99 OR 0 .
                IF ls_emp_num-retcd EQ 99.
                  <fs_main>-color = 'C310'.
                  <fs_main>-msg = TEXT-war.
                  <fs_main>-oprtn = 'N'.
                ELSE.
                  <fs_main>-color = 'C510'.
                  <fs_main>-msg = TEXT-che.
                  <fs_main>-oprtn = 'N'.
                ENDIF.

                PERFORM read_result USING buffer
                                          <fs_main>-rfper
                                 CHANGING <fs_main> .
                go_alv->record_check = 'C'. " Değişiklik kontrolü
              WHEN OTHERS.

                READ TABLE msgtab INTO ls_msgtab
                      WITH KEY pernr = <fs_main>-rfper.
                IF sy-subrc EQ 0 .
                  READ TABLE ls_msgtab-msg INTO ls_msg
                        WITH KEY message = 'E'.
                  IF sy-subrc EQ 0 .
                    <fs_main>-color = 'C600'.
                    <fs_main>-msg = ls_msg-text1.
                  ELSE.
                    <fs_main>-color = 'C600'.
                    <fs_main>-msg = TEXT-inc.
                  ENDIF.
                ELSE.
                  <fs_main>-color = 'C600'.
                  <fs_main>-msg = TEXT-inc.
                ENDIF.

            ENDCASE.
          ENDLOOP.

        WHEN 1.RAISE program_not_exist  .EXIT.
        WHEN 2.RAISE variant_not_exist  .EXIT.
        WHEN 3.RAISE missing_parameter  .EXIT.
        WHEN 4.RAISE wrong_parameter    .EXIT.
        WHEN 5.RAISE wrong_country_group.EXIT.
        WHEN 6.RAISE others.EXIT.
      ENDCASE.

    ENDLOOP.

    CLEAR : ls_key.
    REFRESH: lt_emp_num[],
             advance[],
             tbuff[],
             buffer_dir[],
             del_pclx_tab[],
             psoper[],
             before_image[],
             before_image_pclx[] .

    "<<----------------------------->>


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_RESULT
*&---------------------------------------------------------------------*
FORM read_result  USING    ps_buffer TYPE  hrpay_buffer
                           pv_pernr  TYPE persno
                  CHANGING ps_main LIKE go_alv->gs_main.

  DATA : paytr_result TYPE /dsl/hr80_tt008   .
  FIELD-SYMBOLS <fs> TYPE any .
  DATA : ls_ret TYPE /dsl/hr80_s008   .
  DATA : ls_t011 TYPE /dsl/hr80_t011   .
  DATA : molga TYPE molga   .
  DATA : t500l_wa TYPE t500l .
  DATA : rt.
  DATA : crt.
  DATA : grt.

  DEFINE read_payment.
    LOOP AT ls_ret-paytr_result-inter-&1 INTO DATA(ls_&1).
      ls_t011-ztabl = &2.
      ls_t011-lgart = ls_&1-lgart.

      CLEAR : ls_t011-betpe,
              ls_t011-anzhl,
              ls_t011-betrg.
      CASE &2.
        WHEN 'RT' OR 'GRT'.
          UNASSIGN <fs>.
          ASSIGN COMPONENT 'BETPE' OF STRUCTURE ls_&1 TO <fs>.
          ls_t011-betpe   =  <fs>.
          ls_t011-anzhl   =  ls_&1-anzhl.
          ls_t011-betrg   =  ls_&1-betrg.
        WHEN 'CRT'.
          ls_t011-anzhl   =  ls_&1-anzhl.
          ls_t011-betrg   =  ls_&1-betrg.
      ENDCASE.
      APPEND ls_t011 TO go_alv->gt_t011.
    ENDLOOP.
  END-OF-DEFINITION.

  CALL FUNCTION '/DSL/HR80_FG002_01'
    EXPORTING
      persnr             = pv_pernr
      buffer             = ps_buffer        " Bütçe sonuçlarını alması zorunlu
      i_begda            = go_alv->gs_t003-begda
      i_endda            = go_alv->gs_t003-endda
    IMPORTING
      molga              = molga
    TABLES
      paytr_result       = paytr_result
    EXCEPTIONS
      no_record_found    = 1
    .
  CHECK paytr_result IS NOT INITIAL .

  LOOP AT paytr_result INTO ls_ret.

    ls_t011-molga   =  go_alv->gs_t003-molga.
    ls_t011-grpid   =  go_alv->gs_t003-grpid.
    ls_t011-vrsid   =  go_alv->gs_t003-vrsid.
    ls_t011-pernr   =  ps_main-pernr.
    ls_t011-spmon   =  ls_ret-rgdir-fpper.
    ls_t011-waers   =  ps_main-waers.
    ls_t011-uname   =  sy-uname.
    ls_t011-datum   =  sy-datum.
    ls_t011-uzeit   =  sy-uzeit.
    read_payment : rt   'RT' ,
                   crt  'CRT',
                   grt  'GRT'.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_SIMU_MESSAGE
*&---------------------------------------------------------------------*
FORM read_simu_message  TABLES   pt_msgtab "STRUCTURE msgtab
                         USING   mem_key.
  REFRESH exp_imp_tab.
  REFRESH pt_msgtab.

  exp_imp_tab = 'MSGTAB'.          APPEND exp_imp_tab.
*  exp_imp_tab = 'EDTFORM'.         APPEND exp_imp_tab.
*  exp_imp_tab = 'HRFORMS_ID_TAB'.  APPEND exp_imp_tab.
*  exp_imp_tab = 'LOG_BASIC'.       APPEND exp_imp_tab.
*  exp_imp_tab = 'LOG_REF'.         APPEND exp_imp_tab.


  IMPORT (exp_imp_tab) FROM MEMORY ID mem_key.

  FREE MEMORY ID mem_key.
ENDFORM.
