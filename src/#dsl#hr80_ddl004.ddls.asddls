@AbapCatalog.sqlViewName: '/DSL/HR80_DDL004'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Bordro sonuçları'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view /DSL/HR80_CDS004
  as select from /dsl/hr80_t011 as t1
    inner join   pa0001         as p1 on  p1.pernr = t1.pernr
                                      and p1.endda >= $session.system_date
                                      and p1.begda <= $session.system_date

{

  t1.molga,
  t1.grpid,
  t1.vrsid,

  t1.pernr,
  p1.ename,
  t1.spmon,
  t1.ztabl,
  t1.lgart,
  t1.betrg,
  t1.anzhl,
  t1.betpe,
  t1.waers,
  t1.uname,
  t1.datum,
  t1.uzeit
  //


}
