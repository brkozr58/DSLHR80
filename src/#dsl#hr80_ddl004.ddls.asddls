@AbapCatalog.sqlViewName: '/DSL/HR80_DDL004'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Personel Yıllık Bordro Sonuçları'
@Metadata.ignorePropagatedAnnotations: true
define view /DSL/HR80_CDS004
  as select from    /dsl/hr80_t003   as t003
    inner join      /dsl/hr80_ddl001 as person  on  person.molga     =  t003.molga
                                                and person.grpid     =  t003.grpid
                                                and person.vrsid     =  t003.vrsid
                                                and person.pa1_endda >= t003.begda
                                                and person.pa1_begda <= t003.endda
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
    left outer join t512t                       on  t512t.lgart = payroll.lgart
                                                and t512t.molga = payroll.molga
                                                and t512t.sprsl = $session.system_language
  //
{

  key  payroll.molga,
  key  t003.gjahr,
  key  payroll.grpid,
  key  payroll.vrsid,
  key  person.rfper,
  key  payroll.pernr,
  key  payroll.ztabl,
  key  payroll.lgart,

       //
       person.ename,
       t512t.lgtxt,
       t012.momag,
       t001.ktopl,


       //
       person.pa1_begda,
       person.pa1_endda,
       person.bukrs,
       person.bukrs_t,
       person.orgeh,
       person.orgeh_t,
       person.plans,
       person.plans_t,
       person.stell,
       person.stell_t,
       person.kokrs,
       person.kokrs_t,
       person.kostl,
       person.kostl_t,
       person.werks,
       person.werks_t,
       person.werks_new,
       person.btrtl,
       person.btrtl_t,
       person.btrtl_new,
       person.persg,
       person.ptext_t,
       person.persg_new,
       person.persg_new_t,
       person.persk,
       person.persk_t,
       person.persk_new,
       person.persk_new_t,
       person.ssgrp,
       person.kanun,
       person.prozt,
       person.trfar,
       person.trfar_t,
       person.trfar_new,
       person.trfar_new_t,
       person.trfgb,
       person.trfgb_t,
       person.trfgb_new,
       person.trfgb_new_t,
       person.trfgr,
       person.trfgr_new,
       person.trfst,
       person.trfst_new,
       person.lga01,
       person.salry,
       person.lga01_new,
       person.salry_new,
       person.mgart,
       person.massn,
       person.massn_t,
       person.massg,
       person.massg_t,
       person.stat2,
       person.ansvh,
       person.abkrs,
       person.waers,

       //
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '01' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg01,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '02' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg02,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '03' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg03,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '04' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg04,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '05' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg05,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '06' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg06,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '07' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg07,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '08' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg08,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '09' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg09,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '10' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg10,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '11' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg11,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '12' then sum( payroll.betrg ) else 0 end ) as abap.curr( 15 , 2 ) ) as betrg12,
       //
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '01' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl01,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '02' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl02,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '03' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl03,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '04' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl04,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '05' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl05,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '06' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl06,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '07' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl07,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '08' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl08,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '09' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl09,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '10' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl10,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '11' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl11,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '12' then sum( payroll.anzhl ) else 0 end ) as abap.dec( 15 , 2 ) )  as anzhl12,
       //
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '01' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe01,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '02' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe02,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '03' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe03,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '04' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe04,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '05' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe05,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '06' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe06,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '07' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe07,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '08' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe08,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '09' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe09,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '10' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe10,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '11' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe11,
       cast( ( case ( substring( t012.spmon,5,2 ) ) when '12' then sum( payroll.betpe ) else 0 end ) as abap.curr( 15 , 2 ) ) as betpe12
       //

}

group by
  payroll.molga,
  t003.gjahr,
  payroll.grpid,
  payroll.vrsid,
  person.rfper,
  payroll.pernr,
  t012.spmon,
  payroll.ztabl,
  payroll.lgart,
  t012.momag,
  person.ename,
  t512t.lgtxt,
  t001.ktopl,
  //

  person.pa1_begda,
  person.pa1_endda,
  person.bukrs,
  person.bukrs_t,
  person.orgeh,
  person.orgeh_t,
  person.plans,
  person.plans_t,
  person.stell,
  person.stell_t,
  person.kokrs,
  person.kokrs_t,
  person.kostl,
  person.kostl_t,
  person.werks,
  person.werks_t,
  person.werks_new,
  person.btrtl,
  person.btrtl_t,
  person.btrtl_new,
  person.persg,
  person.ptext_t,
  person.persg_new,
  person.persg_new_t,
  person.persk,
  person.persk_t,
  person.persk_new,
  person.persk_new_t,
  person.ssgrp,
  person.kanun,
  person.prozt,
  person.trfar,
  person.trfar_t,
  person.trfar_new,
  person.trfar_new_t,
  person.trfgb,
  person.trfgb_t,
  person.trfgb_new,
  person.trfgb_new_t,
  person.trfgr,
  person.trfgr_new,
  person.trfst,
  person.trfst_new,
  person.lga01,
  person.salry,
  person.lga01_new,
  person.salry_new,
  person.mgart,
  person.massn,
  person.massn_t,
  person.massg,
  person.massg_t,
  person.stat2,
  person.ansvh,
  person.abkrs,
  person.waers
//






//
