@AbapCatalog.sqlViewName: '/DSL/HR80_DDL002'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Personel anaverileri - PA'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view /DSL/HR80_CDS002
  as select from    /dsl/hr80_t003 as t003
    inner join      /dsl/hr80_t001 as t001  on  t001.grpid = t003.grpid
                                            and t001.molga = t003.molga
//                                            
    inner join      /dsl/hr80_t002 as t002  on  t002.grpid = t003.grpid
                                            and t002.molga = t003.molga
//                                            
    inner join      pa0001         as t1    on  t1.begda <= t003.endda
                                            and t1.endda >= t003.begda
                                            and t1.bukrs =  t002.bukrs
                                            and t1.werks =  t002.werks
                                            and t1.btrtl =  t002.btrtl
                                            and t1.abkrs =  t002.abkrs
                                            and t1.begda <= t003.endda
                                            and t1.endda >= t003.begda
//
    inner join      pa0000         as t2    on  t2.pernr =  t1.pernr
                                            and t2.begda <= t1.endda
                                            and t2.endda >= t1.begda
//
    left outer  join      pa0008         as t8    on  t8.pernr =  t1.pernr
                                            and t8.begda <= t1.endda
                                            and t8.endda >= t1.begda
//
    left outer  join      pa0769                  on  pa0769.pernr =  t1.pernr
                                            and pa0769.begda <= t1.endda
                                            and pa0769.endda >= t1.begda
  //
    left outer join t001           as bukrs on bukrs.bukrs = t1.bukrs
  //
  //    left outer join csks                    on  csks.kostl =  t1.kostl
  //                                            and csks.kokrs =  t1.kokrs
  //                                            and csks.datbi >= t3.endda
 //  
    left outer join tka01          as kokrs on    kokrs.kokrs = t1.kokrs
  //
    left outer join cskt                    on  cskt.kostl =  t1.kostl
                                            and cskt.kokrs =  t1.kokrs
                                            and cskt.datbi >= t1.endda
                                            and cskt.spras = $session.system_language
  //
    left outer join t500p                   on t500p.persa = t1.werks
  //
    left outer join t001p                   on  t001p.werks = t1.werks
                                            and t001p.btrtl = t1.btrtl
  //
    left outer join t501t                   on  t501t.persg = t1.persg
                                            and t501t.sprsl = $session.system_language

  //
    left outer join hrp1000        as orgeh on  orgeh.plvar =  '01'
                                            and orgeh.otype =  'O'
                                            and orgeh.objid =  t1.orgeh
                                            and orgeh.begda <= t1.endda
                                            and orgeh.endda >= t1.begda
                                            and orgeh.langu = $session.system_language
  //
    left outer join hrp1000        as plans on  plans.plvar =  '01'
                                            and plans.otype =  'S'
                                            and plans.objid =  t1.plans
                                            and plans.begda <= t1.endda
                                            and plans.endda >= t1.begda
                                            and plans.langu = $session.system_language
  //
    left outer join hrp1000        as stell on  stell.plvar =  '01'
                                            and stell.otype =  'C'
                                            and stell.objid =  t1.stell
                                            and stell.begda <= t1.endda
                                            and stell.endda >= t1.begda
                                            and stell.langu = $session.system_language
  //
    left outer join t503t                   on  t503t.persk = t1.persk
                                            and t503t.sprsl = bukrs.spras
  //
    left outer join t510a                   on  t510a.trfar = t8.trfar
                                            and t510a.molga = t8.trfar

    left outer join t510g                   on  t510g.trfgb = t8.trfgb
                                            and t510g.molga = t8.trfar
  //
    left outer join t529t                   on  t529t.massn = t2.massn
                                            and t529t.sprsl = $session.system_language
  //
    left outer join t530t                   on  t530t.massn = t2.massn
                                            and t530t.massg = t2.massg
                                            and t530t.sprsl = $session.system_language
  //


{
  //

  t003.molga,
  t003.grpid,
  t001.grpid_t,
  t003.vrsid,
  t003.vrsid_t,
  t003.gjahr,
  t003.begda  as bg_begda,
  t003.endda  as bg_endda,
  t003.statu,
  //
  t1.pernr,
  t1.ename,
//  
  t1.bukrs,
  bukrs.butxt as bukrs_t,
//  
  t1.begda    as pa1_begda,
  t1.endda    as pa1_endda,
  //
  t1.orgeh,
  orgeh.stext as orgeh_t,
//  
  t1.plans,
  plans.stext as plans_t,
//  
  t1.stell,
  stell.stext as stell_t,
//  
  t1.kokrs,
  kokrs.bezei as kokrs_t,
//  
  t1.kostl,
  cskt.ltext  as kostl_t,
//  
  t1.werks,
  t500p.name1 as werks_t,
//  
  t1.btrtl,
  t001p.btext as btrtl_t,
//  
  t1.persg,
  t501t.ptext as PTEXT_t,
//  
  t1.persk,
  t503t.ptext as persk_t,
  //
  pa0769.ssgrp,
  pa0769.kanun,
  //
  t8.trfar,
  t510a.tartx as trfar_t,
  t8.trfgb,
  t510g.tgbtx as trfgb_t,
  t8.trfgr,
  t8.trfst,
  t8.lga01,
  t8.bet01    as salry,
  t8.waers,
  //
  t2.massn,
  t529t.mntxt as massn_t,
//  
  t2.massg,
  t530t.mgtxt as massg_t,
//  
  t2.stat2,
  //
  t1.ansvh,
  t1.abkrs

}
