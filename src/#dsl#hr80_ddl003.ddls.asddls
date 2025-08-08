@AbapCatalog.sqlViewName: '/DSL/HR80_DDL003'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Ücret skalası grupları'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view /DSL/HR80_CDS003
  as select from /dsl/hr80_t007 as t1
    inner join   t511 on  t511.lgart =  t1.lgart
                      
                      and t511.endda >= t1.begda
                      and t511.begda <= t1.endda

{
   
  t1.molga,
  t1.grpid,
  t1.vrsid,
  
  t1.trfar,
  t1.trfgb,
  t1.trfkz,
  t1.trfgr,
  t1.trfst,
  t1.lgart,
  t1.endda,
  t1.begda,
  t1.betrg,
  //
  t511.abtyz,
  t511.wktyz,
  t511.opken,
  t511.zeinh,
  t511.kombi,
  t511.btmin,
  t511.btmax,
  t511.adsum,
  t511.modna,
  t511.modsp,
  t511.rutyp,
  t511.rudiv,
  t511.addjn,
  t511.mod01,
  t511.mod02,
  t511.mod03,
  t511.anmin,
  t511.anmax


}
