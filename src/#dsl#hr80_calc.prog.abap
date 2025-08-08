*&---------------------------------------------------------------------*
*& Include          /DSL/HR80_CALC
*&---------------------------------------------------------------------*
TABLES :  /dsl/hr80_t001    ,"Bütçe - Grup tanımı
          /dsl/hr80_t002    ,"Bütçe - Yetki alanları
          /dsl/hr80_t003    ,"Bütçe - Versiyon yönetimi
          /dsl/hr80_t004    ,"Bütçe - Parametre tanımlamaları
          /dsl/hr80_t005    ,"Bütçe - Ek ödeme ücretleri
          /dsl/hr80_t007    ,"Bütçe - Ücret skalası grupları (T510)
          /dsl/hr80_t010    ,"Bütçe - Personel anaverileri
          /dsl/hr80_t011    ,"Bütçe - Bordro sonuçları
          /dsl/hr80_tvergd  ,"Bütçe - Vergi dilimleri (T7TRT01)
          /dsl/hr80_tvergi  ."Bütçe - Vergi indirimleri (T7TRT02)


  " CALC programında bu parametrelere ulaşabilecek.
  "sadece submitte çalışması sağlanacaktır.
  " değiştirmeyiniz!!!!!!!!!!!
  PARAMETERS p_molga TYPE /dsl/hr80_t001-molga NO-DISPLAY .
  PARAMETERS p_grpid TYPE /dsl/hr80_t003-grpid NO-DISPLAY .
  PARAMETERS p_vrsid TYPE /dsl/hr80_t003-vrsid NO-DISPLAY .
  PARAMETERS p_statu TYPE /dsl/hr80_t003-statu NO-DISPLAY .
  PARAMETERS p_pernr TYPE /dsl/hr80_t010-pernr NO-DISPLAY .
  PARAMETERS p_rfper TYPE /dsl/hr80_t010-rfper NO-DISPLAY .


  DATA :  gt_t001   TYPE TABLE OF /dsl/hr80_t001    WITH HEADER LINE, "Bütçe - Grup tanımı
          gt_t002   TYPE TABLE OF /dsl/hr80_t002    WITH HEADER LINE, "Bütçe - Yetki alanları
          gt_t003   TYPE TABLE OF /dsl/hr80_t003    WITH HEADER LINE, "Bütçe - Versiyon yönetimi
          gt_t004   TYPE TABLE OF /dsl/hr80_t004    WITH HEADER LINE, "Bütçe - Parametre tanımlamaları
          gt_t005   TYPE TABLE OF /dsl/hr80_t005    WITH HEADER LINE, "Bütçe - Ek ödeme ücretleri
          gt_t007   TYPE TABLE OF /dsl/hr80_t007    WITH HEADER LINE, "Bütçe - Ücret skalası grupları (T510)
          gt_t010   TYPE TABLE OF /dsl/hr80_t010    WITH HEADER LINE, "Bütçe - Personel anaverileri
          gt_t011   TYPE TABLE OF /dsl/hr80_t011    WITH HEADER LINE, "Bütçe - Bordro sonuçları
          gt_tvergd TYPE TABLE OF /dsl/hr80_tvergd  WITH HEADER LINE, "Bütçe - Vergi dilimleri (T7TRT01)
          gt_tvergi TYPE TABLE OF /dsl/hr80_tvergi  WITH HEADER LINE. "Bütçe - Vergi indirimleri (T7TRT02)
