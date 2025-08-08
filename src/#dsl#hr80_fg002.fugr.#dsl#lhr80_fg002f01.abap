*----------------------------------------------------------------------*
***INCLUDE /DSL/LHR80_FG002F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  IMPORT_RESULTS
*&---------------------------------------------------------------------*
*       import payroll results based on the RGDIR sequence number
*----------------------------------------------------------------------*
*  -->  SEQNR     sequence number for payroll results
*  <--  RT        results table
*  <--  CRT       Cumulation results table
*----------------------------------------------------------------------*
FORM import_results USING relid seqnr
                 CHANGING cs_result TYPE paytr_result.
  DATA pack TYPE p.

  pack = seqnr.
  UNPACK pack TO rx-key-seqno.
  CLEAR: cs_result  .

  IMPORT
      versc
      wpbp
      abc
      rt
      crt
      bt
      c0
      c1
      v0
      vcp
      alp
      dft
      grt
      ls
      status
      arrrs
      ddntk
      accr
      bentab
      ab
      fund
      average
      modif
  FROM DATABASE pcl2(tr)
      ID rx-key
  USING pcl2_exp_imp
        IGNORING STRUCTURE BOUNDARIES.
  CHECK sy-subrc EQ 0 .

  cs_result-inter-versc      = versc     .
  cs_result-inter-wpbp[]     = wpbp[]     .
  cs_result-inter-abc[]      = abc[]      .
  cs_result-inter-rt[]       = rt[]       .
  cs_result-inter-crt[]      = crt[]      .
  cs_result-inter-bt[]       = bt[]       .
  cs_result-inter-c0[]       = c0[]       .
  cs_result-inter-c1[]       = c1[]       .
  cs_result-inter-v0[]       = v0[]       .
  cs_result-inter-vcp[]      = vcp[]      .
  cs_result-inter-alp[]      = alp[]      .
  cs_result-inter-dft[]      = dft[]      .
  cs_result-inter-grt[]      = grt[]      .
  cs_result-inter-ls[]       = ls[]       .
  cs_result-inter-status     = status     .
  cs_result-inter-arrrs[]    = arrrs[]    .
  cs_result-inter-ddntk[]    = ddntk[]    .
  cs_result-inter-accr[]     = accr[]     .
  cs_result-inter-bentab[]   = bentab[]   .
  cs_result-inter-ab[]       = ab[]       .
  cs_result-inter-fund[]     = fund[]     .
  cs_result-inter-average[]  = average[]  .
  cs_result-inter-modif[]    = modif[]    .


ENDFORM.                    " IMPORT_RESULTS
