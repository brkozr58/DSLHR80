@AbapCatalog.sqlViewName: '/DSL/HR80_DDL004'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Bütçe - Personel yıllık bordro sonuçları'
@Metadata.ignorePropagatedAnnotations: true
define view /DSL/HR80_CDS004
  as select from    /dsl/hr80_t011   as t1 
    inner join      /dsl/hr80_t003   as t2 on  t2.molga = t1.molga
                                           and t2.grpid = t1.grpid
                                           and t2.vrsid = t1.vrsid

    left outer join t512t                  on  t1.lgart    = t512t.lgart
                                           and t512t.molga = t1.molga
                                           and t512t.sprsl = $session.system_language


{

  key t1.molga,
  key t2.gjahr,
  key t1.grpid,
  key t1.vrsid,
  key t1.pernr,
  key t1.ztabl,
  key t1.lgart,
      t512t.lgtxt,

      cast( ( case ( substring( t1.spmon,5,2 ) )  when '01' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg01,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '02' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg02,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '03' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg03,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '04' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg04,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '05' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg05,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '06' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg06,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '07' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg07,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '08' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg08,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '09' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg09,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '10' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg10,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '11' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg11,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '12' then sum( t1.betrg )  else 0 end ) as abap.dec( 15 , 2 ) ) as betrg12,

      cast( ( case ( substring( t1.spmon,5,2 ) )  when '01' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl01,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '02' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl02,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '03' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl03,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '04' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl04,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '05' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl05,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '06' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl06,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '07' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl07,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '08' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl08,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '09' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl09,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '10' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl10,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '11' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl11,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '12' then sum( t1.anzhl )  else 0 end ) as abap.dec( 15 , 2 ) ) as anzhl12,

      cast( ( case ( substring( t1.spmon,5,2 ) )  when '01' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe01,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '02' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe02,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '03' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe03,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '04' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe04,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '05' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe05,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '06' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe06,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '07' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe07,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '08' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe08,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '09' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe09,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '10' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe10,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '11' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe11,
      cast( ( case ( substring( t1.spmon,5,2 ) )  when '12' then sum( t1.betpe )  else 0 end ) as abap.dec( 15 , 2 ) ) as betpe12

}

group by
  t1.molga,
  t2.gjahr,
  t1.grpid,
  t1.vrsid,
  t1.pernr,
  t1.spmon,
  t1.ztabl,
  t1.lgart,
  t512t.lgtxt
