@AbapCatalog.sqlViewName: '/DSL/HR80_DDL006'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Personel Yıllık Bordro Sonuçları (HRC-HRF)'
@Metadata.ignorePropagatedAnnotations: true
define view /DSL/HR80_CDS006
  as select from    /dsl/hr80_ddl005 as payroll
  //
    left outer join /dsl/hr80_t013   as t013   on  t013.bukrs    =  payroll.bukrs
                                               and t013.lgart    =  payroll.lgart
                                               and t013.endda    >= payroll.pa1_endda
                                               and t013.spprc    <> 'P'
                                               and payroll.ztabl =  'RT'
  //
    left outer join /dsl/hr80_t014   as t014   on  t014.molga      = payroll.molga
                                               and t014.grpid      = payroll.grpid
                                               and t014.vrsid      = payroll.vrsid
                                               and payroll.ztabl   = 'RT'
                                               and (
                                                  (
                                                    t014.ktosl     = 'HRC'
                                                    and t014.bklas = ''
                                                  )
                                                  or t014.ktosl    = 'HRF'
                                                  // or t014.ktosl    = 'HRA'
                                                )
                                               and t014.ktopl      = payroll.ktopl
                                               and t014.bwmod      = t013.symko
  //
    left outer join t52ekt           as T52EKT on  T52EKT.symko  = t013.symko
                                               and T52EKT.sprsl  = $session.system_language
                                               and payroll.ztabl = 'RT'
  //
{

  key  payroll.molga,
  key  gjahr,
  key  payroll.grpid,
  key  payroll.vrsid,
  key  rfper,
  key  pernr,
  key  ztabl,
  key  payroll.lgart,

       //
  key  case when payroll.ztabl = 'RT' then
                case when t014.ktosl = 'HRC' then 'HRC'
                     when t014.ktosl = 'HRF' then 'HRF'
       //                     else 'HRA'
                end
        else ''
        end                                                                     as ktosl,
  key  case when payroll.ztabl = 'RT' then
                cast ( t014.konts  as saknr )
        else ''
        end                                                                     as konts,
       //

       ename,
       lgtxt,
       momag,
       payroll.ktopl,

       //
       case when t013.sign = '-' then payroll.bet01 * -1 else payroll.bet01 end as bet01,
       case when t013.sign = '-' then payroll.bet02 * -1 else payroll.bet02 end as bet02,
       case when t013.sign = '-' then payroll.bet03 * -1 else payroll.bet03 end as bet03,
       case when t013.sign = '-' then payroll.bet04 * -1 else payroll.bet04 end as bet04,
       case when t013.sign = '-' then payroll.bet05 * -1 else payroll.bet05 end as bet05,
       case when t013.sign = '-' then payroll.bet06 * -1 else payroll.bet06 end as bet06,
       case when t013.sign = '-' then payroll.bet07 * -1 else payroll.bet07 end as bet07,
       case when t013.sign = '-' then payroll.bet08 * -1 else payroll.bet08 end as bet08,
       case when t013.sign = '-' then payroll.bet09 * -1 else payroll.bet09 end as bet09,
       case when t013.sign = '-' then payroll.bet10 * -1 else payroll.bet10 end as bet10,
       case when t013.sign = '-' then payroll.bet11 * -1 else payroll.bet11 end as bet11,
       case when t013.sign = '-' then payroll.bet12 * -1 else payroll.bet12 end as bet12,
       //
       
       
       pa1_begda,
       pa1_endda,
       payroll.bukrs,
       bukrs_t,
       orgeh,
       orgeh_t,
       plans,
       plans_t,
       stell,
       stell_t,
       kokrs,
       kokrs_t,
       kostl,
       kostl_t,
       werks,
       werks_t,
       werks_new,
       btrtl,
       btrtl_t,
       btrtl_new,
       persg,
       ptext_t,
       persg_new,
       persg_new_t,
       persk,
       persk_t,
       persk_new,
       persk_new_t,
       ssgrp,
       kanun,
       prozt,
       trfar,
       trfar_t,
       trfar_new,
       trfar_new_t,
       trfgb,
       trfgb_t,
       trfgb_new,
       trfgb_new_t,
       trfgr,
       trfgr_new,
       trfst,
       trfst_new,
       lga01,
       salry,
       lga01_new,
       salry_new,
       mgart,
       massn,
       massn_t,
       massg,
       massg_t,
       stat2,
       ansvh,
       abkrs,
       waers,
       //
       case when t013.sign = '-' then payroll.anz01 * -1 else payroll.anz01 end as anz01,
       case when t013.sign = '-' then payroll.anz02 * -1 else payroll.anz02 end as anz02,
       case when t013.sign = '-' then payroll.anz03 * -1 else payroll.anz03 end as anz03,
       case when t013.sign = '-' then payroll.anz04 * -1 else payroll.anz04 end as anz04,
       case when t013.sign = '-' then payroll.anz05 * -1 else payroll.anz05 end as anz05,
       case when t013.sign = '-' then payroll.anz06 * -1 else payroll.anz06 end as anz06,
       case when t013.sign = '-' then payroll.anz07 * -1 else payroll.anz07 end as anz07,
       case when t013.sign = '-' then payroll.anz08 * -1 else payroll.anz08 end as anz08,
       case when t013.sign = '-' then payroll.anz09 * -1 else payroll.anz09 end as anz09,
       case when t013.sign = '-' then payroll.anz10 * -1 else payroll.anz10 end as anz10,
       case when t013.sign = '-' then payroll.anz11 * -1 else payroll.anz11 end as anz11,
       case when t013.sign = '-' then payroll.anz12 * -1 else payroll.anz12 end as anz12,
       //
       case when t013.sign = '-' then payroll.btp01 * -1 else payroll.btp01 end as btp01,
       case when t013.sign = '-' then payroll.btp02 * -1 else payroll.btp02 end as btp02,
       case when t013.sign = '-' then payroll.btp03 * -1 else payroll.btp03 end as btp03,
       case when t013.sign = '-' then payroll.btp04 * -1 else payroll.btp04 end as btp04,
       case when t013.sign = '-' then payroll.btp05 * -1 else payroll.btp05 end as btp05,
       case when t013.sign = '-' then payroll.btp06 * -1 else payroll.btp06 end as btp06,
       case when t013.sign = '-' then payroll.btp07 * -1 else payroll.btp07 end as btp07,
       case when t013.sign = '-' then payroll.btp08 * -1 else payroll.btp08 end as btp08,
       case when t013.sign = '-' then payroll.btp09 * -1 else payroll.btp09 end as btp09,
       case when t013.sign = '-' then payroll.btp10 * -1 else payroll.btp10 end as btp10,
       case when t013.sign = '-' then payroll.btp11 * -1 else payroll.btp11 end as btp11,
       case when t013.sign = '-' then payroll.btp12 * -1 else payroll.btp12 end as btp12
       //
} 










//
