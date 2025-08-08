@AbapCatalog.sqlViewName: '/DSL/HR80_DDL001'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Personel anaverileri'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view /DSL/HR80_CDS001
  as select from    /dsl/hr80_t010 as t1
    inner join      /dsl/hr80_t001 as t2        on  t2.grpid = t1.grpid
                                                and t2.molga = t1.molga
    inner join      /dsl/hr80_t003 as t3        on  t3.grpid = t2.grpid
                                                and t3.vrsid = t1.vrsid
                                                and t3.molga = t2.molga
//                                            
//    inner join      /dsl/hr80_t002 as t4        on  t4.grpid = t3.grpid
//                                                and t4.molga = t3.molga
  //
    left outer join t001           as bukrs     on bukrs.bukrs = t1.bukrs 
 //  
    left outer join tka01          as kokrs on    kokrs.kokrs = t1.kokrs
  //
  //
    left outer join cskt                        on  cskt.kostl =  t1.kostl
                                                and cskt.kokrs =  t1.kokrs
                                                and cskt.datbi >= t3.endda
                                                and cskt.spras = $session.system_language
  //
    left outer join t500p                       on t500p.persa = t1.werks
  //
    left outer join t001p                       on  t001p.werks = t1.werks
                                                and t001p.btrtl = t1.btrtl
  //
    left outer join t501t                       on  t501t.persg = t1.persg
                                                and t501t.sprsl = $session.system_language

  //
    left outer join hrp1000        as orgeh     on  orgeh.plvar =  '01'
                                                and orgeh.otype =  'O'
                                                and orgeh.objid =  t1.orgeh
                                                and orgeh.begda <= t3.endda
                                                and orgeh.endda >= t3.begda
                                                and orgeh.langu = $session.system_language
  //
    left outer join hrp1000        as plans     on  plans.plvar =  '01'
                                                and plans.otype =  'S'
                                                and plans.objid =  t1.plans
                                                and plans.begda <= t3.endda
                                                and plans.endda >= t3.begda
                                                and plans.langu = $session.system_language
  //
    left outer join hrp1000        as stell     on  stell.plvar =  '01'
                                                and stell.otype =  'C'
                                                and stell.objid =  t1.stell
                                                and stell.begda <= t3.endda
                                                and stell.endda >= t3.begda
                                                and stell.langu = $session.system_language
  //
    left outer join t501t          as persg_new on  persg_new.persg = t1.persg_new
                                                and persg_new.sprsl = bukrs.spras
  //
    left outer join t503t                       on  t503t.persk = t1.persk
                                                and t503t.sprsl = bukrs.spras
  //
    left outer join t503t          as persk_new on  persk_new.persk = t1.persk_new
                                                and persk_new.sprsl = bukrs.spras
  //
    left outer join t510a                       on  t510a.trfar = t1.trfar
                                                and t510a.molga = t2.molga
  //
    left outer join t510a          as trfar_new on  trfar_new.trfar = t1.trfar_new
                                                and trfar_new.molga = t1.molga
  //
    left outer join t510g                       on  t510g.trfgb = t1.trfgb
                                                and t510g.molga = t2.molga
  //
    left outer join t510g          as trfgb_new on  trfgb_new.trfgb = t1.trfgb_new
                                                and trfgb_new.molga = t2.molga
  //
    left outer join t529t                       on  t529t.massn = t1.massn
                                                and t529t.sprsl = $session.system_language
  //
    left outer join t530t                       on  t530t.massn = t1.massn
                                                and t530t.massg = t1.massg
                                                and t530t.sprsl = $session.system_language
  //
{
  t1.molga,
  t1.grpid,
  t2.grpid_t,
  t1.vrsid,
  t3.vrsid_t,
  t1.gjahr,
  t1.rfper,
  t1.pernr,
  t1.ename,

  t1.begda        as pa1_begda,
  t1.endda        as pa1_endda,

  t1.bukrs,
  bukrs.butxt     as bukrs_t,
  t1.orgeh,
  orgeh.stext     as orgeh_t,
  t1.plans,
  plans.stext     as plans_t,
  t1.stell,
  stell.stext     as stell_t,
  t1.kokrs,
  kokrs.bezei as kokrs_t,
  t1.kostl,
  cskt.ltext      as kostl_t,
  t1.werks,
  t500p.name1     as werks_t,
  t1.werks_new,
  t1.btrtl,
  t001p.btext     as btrtl_t,
  t1.btrtl_new,
  t1.persg,
  t501t.ptext     as PTEXT_t,
  t1.persg_new,
  persg_new.ptext as persg_new_t,
  t1.persk,
  t503t.ptext     as persk_t,
  t1.persk_new,
  persk_new.ptext as persk_new_t,
  t1.ssgrp,
  t1.kanun,
  t1.prozt,
  t1.trfar,
  t510a.tartx     as trfar_t,
  t1.trfar_new,
  trfar_new.tartx as trfar_new_t,
  t1.trfgb,
  t510g.tgbtx     as trfgb_t,
  t1.trfgb_new,
  trfgb_new.tgbtx as trfgb_new_t,
  t1.trfgr,
  t1.trfgr_new,
  t1.trfst,
  t1.trfst_new,
  t1.lga01,
  t1.salry,
  t1.lga01_new,
  t1.salry_new,
  t1.komok,
  t1.mgart,
  t1.massn,
  t529t.mntxt     as massn_t,
  t1.massg,
  t530t.mgtxt     as massg_t,
  t1.stat2,
  t1.ansvh,
  t1.abkrs,
  t1.waers,
  t3.statu

}
