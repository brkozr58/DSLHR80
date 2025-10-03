@AbapCatalog.sqlViewName: '/DSL/HR80_DDL004'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Personel yıllık bordro sonuçları'
@Metadata.ignorePropagatedAnnotations: true
define view /DSL/HR80_CDS004
  as select from    /dsl/hr80_t003   as t003
    inner join      /dsl/hr80_ddl001 as person  on  person.molga     =  t003.molga
                                                and person.grpid     =  t003.grpid
                                                and person.vrsid     =  t003.vrsid
                                                and person.pa1_endda >= t003.endda
  //
    inner join      t001                        on t001.bukrs = person.bukrs
  //
    inner join      /dsl/hr80_t012   as t012    on  t012.molga = person.molga
                                                and t012.grpid = person.grpid
                                                and t012.vrsid = person.vrsid
                                                and t012.pernr = person.pernr
  //
    inner join      /dsl/hr80_t011   as payroll on  payroll.molga = person.molga
                                                and payroll.grpid = person.grpid
                                                and payroll.vrsid = person.vrsid
                                                and payroll.pernr = person.pernr
                                                and payroll.spmon = t012.spmon
  //
    left outer join /dsl/hr80_t013   as t013    on  t013.bukrs    =  person.bukrs
                                                and t013.lgart    =  payroll.lgart
                                                and t013.endda    >= t003.endda
                                                and t013.spprc    <> 'P'
                                                and payroll.ztabl =  'RT'
  //
    left outer join /dsl/hr80_t014   as t014    on  t014.molga      = person.molga
                                                and t014.grpid      = person.grpid
                                                and t014.vrsid      = person.vrsid
                                                and (
                                                   (
                                                     t014.ktosl     = 'HRC'
                                                     and t014.bklas = ''
                                                   )
                                                   or t014.ktosl    = 'HRF'
                                                   // or t014.ktosl    = 'HRA'
                                                 )
                                                and t014.ktopl      = t001.ktopl
                                                and t014.bwmod      = t013.symko
  //
    left outer join t512t                       on  t512t.lgart = payroll.lgart
                                                and t512t.molga = payroll.molga
                                                and t512t.sprsl = $session.system_language
  //
    left outer join t52ekt           as T52EKT  on  T52EKT.symko = t013.symko
                                                and T52EKT.sprsl = $session.system_language
  //

{

  key  payroll.molga,
  key  t003.gjahr,
  key  payroll.grpid,
  key  payroll.vrsid,
  key  payroll.pernr,
  key  payroll.ztabl,
  key  payroll.lgart,
       //
  key  case when payroll.ztabl = 'RT' then
                case when t014.ktosl = 'HRC' then 'HRC'
                     when t014.ktosl = 'HRF' then 'HRF'
       //                     else 'HRA'
                end
        else ''
        end                                                                                                                                                                                 as ktosl,
  key  case when payroll.ztabl = 'RT' then
                cast ( t014.konts  as saknr )
        else ''
        end                                                                                                                                                                                 as konts,
       //
       t512t.lgtxt,
       t012.momag,
       //
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg01,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg02,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg03,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg04,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg05,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg06,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg07,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg08,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg09,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg10,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg11,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betrg * -1 else payroll.betrg end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg12,
       // 
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl01,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl02,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl03,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl04,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl05,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl06,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl07,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl08,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl09,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl10,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl11,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.anzhl * -1 else payroll.anzhl end ) else 0 end ) as abap.curr( 15 , 2 ) ) as anzhl12,
       // 
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe01,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe02,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe03,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe04,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe05,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe06,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe07,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe08,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe09,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe10,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe11,
       cast( ( case ( substring( payroll.spmon,5,2 ) ) when '01' then sum( case when t013.sign = '-' then payroll.betpe * -1 else payroll.betpe end ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe12
       //       //
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '01' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg01,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '02' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg02,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '03' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg03,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '04' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg04,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '05' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg05,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '06' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg06,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '07' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg07,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '08' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg08,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '09' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg09,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '10' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg10,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '11' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg11,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '12' then sum( payroll.betrg )  else 0 end ) as abap.curr( 15 , 2 ) ) as betrg12,
       //
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '01' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl01,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '02' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl02,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '03' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl03,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '04' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl04,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '05' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl05,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '06' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl06,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '07' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl07,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '08' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl08,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '09' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl09,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '10' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl10,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '11' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl11,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '12' then sum( payroll.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl12,
       //
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '01' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe01,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '02' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe02,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '03' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe03,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '04' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe04,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '05' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe05,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '06' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe06,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '07' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe07,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '08' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe08,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '09' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe09,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '10' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe10,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '11' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe11,
       //       cast( ( case ( substring( payroll.spmon,5,2 ) )  when '12' then sum( payroll.betpe )  else 0 end ) as abap.curr( 15 , 2 ) ) as betpe12
       //       //
}

group by
  payroll.molga,
  t003.gjahr,
  payroll.grpid,
  payroll.vrsid,
  payroll.pernr,
  payroll.spmon,
  payroll.ztabl,
  payroll.lgart,
  ktosl,
  konts,
  t012.momag,
  t512t.lgtxt






//
