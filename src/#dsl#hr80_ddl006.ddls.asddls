@AbapCatalog.sqlViewName: '/DSL/HR80_DDL006'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe Özet Anaveri'
@Metadata.ignorePropagatedAnnotations: true
define view /DSL/HR80_CDS006
  as select from /dsl/hr80_ddl004
{
  key molga,
  key gjahr,
  key grpid,
  key vrsid,
  key pernr,
  key ztabl,
  key lgart,
      lgtxt
}
