@AbapCatalog.sqlViewName: '/DSL/HR80_DDL006'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Hesap tayinleri listesi'
@Metadata.ignorePropagatedAnnotations: true

define view /DSL/HR80_CDS006
  as select from    /dsl/hr80_ddl005 as payroll
    inner join      /dsl/hr80_t003   as t003   on  t003.molga = payroll.molga
                                               and t003.grpid = payroll.grpid
                                               and t003.vrsid = payroll.vrsid
  //
    inner join      /dsl/hr80_ddl001 as person on  person.molga     =  payroll.molga
                                               and person.grpid     =  payroll.grpid
                                               and person.vrsid     =  payroll.vrsid
                                               and person.pernr     =  payroll.pernr
                                               and person.pa1_endda >= t003.endda
  //
    inner join      /dsl/hr80_t013   as t013   on  t013.bukrs =  person.bukrs
                                               and t013.lgart =  payroll.lgart
                                               and t013.endda >= t003.endda
                                               and t013.spprc <> 'P'

  //
    left outer join t52ekt           as T52EKT on  T52EKT.sprsl = $session.system_language
                                               and T52EKT.symko = t013.symko
  //
    left outer join t001                       on t001.bukrs = person.bukrs
  //
    left outer join /dsl/hr80_t014   as HRC    on  HRC.molga = person.molga
                                               and HRC.grpid = person.grpid
                                               and HRC.vrsid = person.vrsid
                                               and HRC.ktosl = 'HRC'
                                               and HRC.ktopl = t001.ktopl
                                               and HRC.bwmod = t013.symko
                                               and HRC.bklas = ''
  //
    left outer join /dsl/hr80_t014   as HRF    on  HRC.molga = person.molga
                                               and HRC.grpid = person.grpid
                                               and HRC.vrsid = person.vrsid
                                               and HRF.ktosl = 'HRF'
                                               and HRF.ktopl = t001.ktopl
                                               and HRF.bwmod = t013.symko
  //
{
  key  person.molga,
  key  person.grpid,
  key  person.vrsid,
  key  person.pernr,
  key  payroll.ztabl,
  key  payroll.lgart,

       //
  key  cast ( coalesce(HRF.ktosl,HRC.ktosl ) as ktosl )                         as ktosl,
  key  cast ( coalesce(HRF.konts,HRC.konts ) as saknr )                         as konts,
       //
  key  t013.symko,
       person.grpid_t,
       person.vrsid_t,
       person.ename,
       payroll.lgtxt,
       T52EKT.text                                                              as symko_txt,

       person.kostl,
       person.kostl_t,
       //
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
       case when t013.sign = '-' then payroll.btp12 * -1 else payroll.btp12 end as btp12,
       //

       person.werks,
       person.werks_t,
       person.btrtl,
       person.btrtl_t,
       person.persk,
       person.persk_t,
       person.ansvh,
       person.bukrs,
       person.bukrs_t,
       person.orgeh,
       person.orgeh_t,
       person.plans,
       person.plans_t,
       person.stell,
       person.stell_t

       //

}
where
      person.pa1_endda >= $session.system_date
  and payroll.ztabl    = 'RT'












//
