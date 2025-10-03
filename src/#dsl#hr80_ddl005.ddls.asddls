@AbapCatalog.sqlViewName: '/DSL/HR80_DDL005'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Personel yıllık bordro sonuçları (son)'
@Metadata.ignorePropagatedAnnotations: true
define view /DSL/HR80_CDS005
  as select from /dsl/hr80_ddl004
{
  key molga,
  key gjahr,
  key grpid,
  key vrsid,
  key pernr,
  key ztabl,
  key lgart,
  key ktosl,
  key konts,
      //
      lgtxt,
      momag,
      //
      sum( betrg01 ) as bet01,
      sum( betrg02 ) as bet02,
      sum( betrg03 ) as bet03,
      sum( betrg04 ) as bet04,
      sum( betrg05 ) as bet05,
      sum( betrg06 ) as bet06,
      sum( betrg07 ) as bet07,
      sum( betrg08 ) as bet08,
      sum( betrg09 ) as bet09,
      sum( betrg10 ) as bet10,
      sum( betrg11 ) as bet11,
      sum( betrg12 ) as bet12,
      //
      sum( anzhl01 ) as ANZ01,
      sum( anzhl02 ) as ANZ02,
      sum( anzhl03 ) as ANZ03,
      sum( anzhl04 ) as ANZ04,
      sum( anzhl05 ) as ANZ05,
      sum( anzhl06 ) as ANZ06,
      sum( anzhl07 ) as ANZ07,
      sum( anzhl08 ) as ANZ08,
      sum( anzhl09 ) as ANZ09,
      sum( anzhl10 ) as ANZ10,
      sum( anzhl11 ) as ANZ11,
      sum( anzhl12 ) as ANZ12,
      //
      sum( betpe01 ) as btp01,
      sum( betpe02 ) as btp02,
      sum( betpe03 ) as btp03,
      sum( betpe04 ) as btp04,
      sum( betpe05 ) as btp05,
      sum( betpe06 ) as btp06,
      sum( betpe07 ) as btp07,
      sum( betpe08 ) as btp08,
      sum( betpe09 ) as btp09,
      sum( betpe10 ) as btp10,
      sum( betpe11 ) as btp11,
      sum( betpe12 ) as btp12
      //
}
group by
  molga,
  gjahr,
  grpid,
  vrsid,
  pernr,
  ztabl,
  lgart,
  ktosl,
  konts,
  momag,
  lgtxt
