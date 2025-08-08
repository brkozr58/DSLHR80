*&---------------------------------------------------------------------*
*& Report /DSL/HR80_P001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&"ÖNEMLİ :
*&"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*&Simülasyon çalıştığında buffer dan bordro sonucu okumak için
*&aşağıdaki linkte T77s0 tablosuna
*&
*&
*&PBON = X: Activate Central PCLx Puffer
*&PBOFF = X: Deactivate Central PCLx Buffer
*&
*&https://me.sap.com/notes/2498143
*&
*&Aynı zamanda su01 den kullanıcının parametre sabitlerine
*&  HRPCLX_NEW_BUFFERING  + tanımlanmalı
*&"each user can switch the central buffer ON or OFF by maintaining
*&  "user parameter HRPCLX_NEW_BUFFERING in
*&  "  Menu->System->User Profile->Own Data: Parameters
*&
*&"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*&
*&/DSL/HR80 numara aralığı tanımlaması yapılmalıdır. Tanımlı değil ise
*& /DSL/HR80_001 Bütçe Yönetimi  programı çalıştığında otomatik oluşacak
*&
*&/DSL/HR80   BS    0100000000  9999999999
*&/DSL/HR80   GR    0000000001  0060000000
*&/DSL/HR80   PR    0099000000  0099999999
*&/DSL/HR80   VR    0060000001  0089999999
*&---------------------------------------------------------------------*
REPORT /dsl/hr80_p001 MESSAGE-ID /dsl/hr80.

*INCLUDE /dsl/hr80_i001.
INCLUDE /dsl/hr80_p001_001.

START-OF-SELECTION.
  PERFORM generate_snum .

  CALL SCREEN '0100'.





*&---------------------------------------------------------------------*
*& Form GENERATE_SNUM
*&---------------------------------------------------------------------*
FORM generate_snum .
  DATA : lt_nriv TYPE TABLE OF nriv WITH HEADER LINE .
  DATA : ls_nriv TYPE nriv   .

  DEFINE insert_nriv.
    CLEAR : ls_nriv .
    ls_nriv-object      = '/DSL/HR80'.
    ls_nriv-subobject   = ''.
    ls_nriv-nrrangenr   = &1 .
    ls_nriv-toyear      = ''.
    ls_nriv-fromnumber  = &2.
    ls_nriv-tonumber    = &3.
    ls_nriv-nrlevel     = ''.
    ls_nriv-externind   = ''.
    MODIFY nriv FROM ls_nriv.
  END-OF-DEFINITION.


  SELECT * FROM nriv INTO TABLE lt_nriv[]
       WHERE object EQ '/DSL/HR80' .

  IF lines( lt_nriv[] ) NE 4 .
    READ TABLE lt_nriv WITH KEY nrrangenr = 'BS'. " Boş numara alanı
    IF sy-subrc NE 0 .
      insert_nriv : 'BS' '0100000000' '9999999999'    .
    ENDIF.

    READ TABLE lt_nriv WITH KEY nrrangenr = 'GR'. " Bütçe grubu
    IF sy-subrc NE 0 .
      insert_nriv : 'GR' '0000000001' '0060000000'    .
    ENDIF.

    READ TABLE lt_nriv WITH KEY nrrangenr = 'PR'. " DUMMy Personel
    IF sy-subrc NE 0 .
      insert_nriv : 'PR' '0099000000' '0099999999'    .
    ENDIF.


    READ TABLE lt_nriv WITH KEY nrrangenr = 'VR'. " Bütçe versiyonu
    IF sy-subrc NE 0 .
      insert_nriv : 'VR' '0060000001' '0089999999'    .
    ENDIF.

  ENDIF.

ENDFORM.
