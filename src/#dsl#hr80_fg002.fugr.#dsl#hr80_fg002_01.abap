FUNCTION /DSL/HR80_FG002_01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PERSNR) LIKE  P0000-PERNR
*"     REFERENCE(BUFFER) TYPE  HRPAY_BUFFER
*"     REFERENCE(NO_AUTHORITY_CHECK) TYPE  XFELD DEFAULT ' '
*"     REFERENCE(I_BEGDA) TYPE  BEGDA
*"     REFERENCE(I_ENDDA) TYPE  ENDDA
*"  EXPORTING
*"     VALUE(MOLGA) LIKE  T500L-MOLGA
*"  TABLES
*"      PAYTR_RESULT TYPE  /DSL/HR80_TT008
*"  EXCEPTIONS
*"      NO_RECORD_FOUND
*"----------------------------------------------------------------------

  STATICS: last_persnr TYPE pernr_d.
  DATA : ls_result TYPE  /dsl/hr80_s008.
  DATA : t500l_wa TYPE t500l.

  DATA: save_no_authority_check TYPE xfeld.

*##--BEGIN--##
  cd-key = persnr.

  save_no_authority_check    = no_authority_check_cluster.
  no_authority_check_cluster = no_authority_check.

  IF buffer-tbuff[] IS INITIAL.
    IF persnr NE last_persnr.
      last_persnr = persnr.
      rp-init-buffer.
      rp-imp-c2-cu.
      IF rp-imp-cd-subrc NE 0.
        CLEAR last_persnr.
        MESSAGE s020 WITH persnr RAISING no_record_found.
        RETURN. "from function
      ENDIF.
    ENDIF.
  ELSE.
    tbuff[] = buffer-tbuff[].
    buffer_dir[] = buffer-buffer_dir[].
    rp-imp-c2-cu.
    IF rp-imp-cd-subrc NE 0.
      MESSAGE s020 WITH persnr RAISING no_record_found.
      RETURN. "from function
    ENDIF.
  ENDIF.

  no_authority_check_cluster = save_no_authority_check.
  molga = ocd-version-molga.

  CALL FUNCTION 'HRPY_READ_T500L'
    EXPORTING
      molga          = molga
    IMPORTING
      t500l_entry    = t500l_wa
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  LOOP AT rgdir WHERE fpbeg LE i_endda
                  AND fpend GE i_begda.
    MOVE-CORRESPONDING rgdir TO ls_result-rgdir .
    rx-key-pernr = persnr.
    PERFORM import_results USING t500l_wa-relid rgdir-seqnr
                        CHANGING ls_result-paytr_result.
    APPEND ls_result TO paytr_result.CLEAR ls_result .
  ENDLOOP.


ENDFUNCTION.
