*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_I001
*&---------------------------------------------------------------------*

TABLES :  /dsl/hr80_t001,"Bütçe - Grup tanımı
          /dsl/hr80_t002,"Bütçe - Yetki alanları
          /dsl/hr80_t003,"Bütçe - Versiyon yönetimi
          /dsl/hr80_t004,"Bütçe - Parametre tanımlamaları
          /dsl/hr80_t005,"Bütçe - Ek ödeme ücretleri
          /dsl/hr80_t007,"Bütçe - Ücret skalası grupları
          /dsl/hr80_t010,"Bütçe - Personel anaverileri
          /dsl/hr80_t011."Bütçe - Bordro sonuçları
*
*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-000  .
  PARAMETERS : p_molga TYPE /dsl/hr80_t001-molga
                      OBLIGATORY
                      DEFAULT '47'.
SELECT-OPTIONS : s_grpid FOR /dsl/hr80_t003-grpid
                      NO INTERVALS
                      NO-EXTENSION
*                      OBLIGATORY
                      MATCHCODE OBJECT /dsl/hr80_grpid
                      ,

                s_vrsid FOR /dsl/hr80_t003-vrsid
                      NO INTERVALS
                      NO-EXTENSION
*                      OBLIGATORY
                      MATCHCODE OBJECT /dsl/hr80_vrsid
                       ,

                s_gjahr FOR /dsl/hr80_t003-gjahr
                      NO INTERVALS
                      NO-EXTENSION ,

                s_statu FOR /dsl/hr80_t003-statu
                      NO INTERVALS
*                      OBLIGATORY
                      DEFAULT '1'
                 .
SELECTION-SCREEN END OF BLOCK bl1.
