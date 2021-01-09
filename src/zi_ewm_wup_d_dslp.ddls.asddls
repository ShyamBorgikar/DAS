@AbapCatalog.sqlViewName: 'ZIEWMDSLP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Loding point'
@ClientHandling.algorithm:           #SESSION_VARIABLE
@Search.searchable: true
define view ZI_EWM_WUP_D_DSLP
  as select from /scwm/d_dslp as LoadingPoint

  association [0..*] to ZI_EWM_WUP_D_DSPLPT as _LoadingPointTxt on $projection.DbKey = _LoadingPointTxt.ParentKey
{
  key db_key          as DbKey,
      @Search.defaultSearchElement: true
      loadpoint       as Loadpoint,
      cal             as Cal,
      slotdura        as Slotdura,
      slotmin         as Slotmin,
      slotmax         as Slotmax,
      numrin          as Numrin,
      lptype          as Lptype,
      lp_parent       as LpParent,
      lploca          as Lploca,
      ewmint          as Ewmint,
      plhor_ind       as PlhorInd,
      plhor_rel_hours as PlhorRelHours,
      plhor_abs_days  as PlhorAbsDays,
      plhor_abs_time  as PlhorAbsTime,
      app_lead_time   as AppLeadTime,
      var_length_ind  as VarLengthInd,
      created_by      as CreatedBy,
      created_on      as CreatedOn,
      changed_by      as ChangedBy,
      changed_on      as ChangedOn,
      eew_dslp_root   as EewDslpRoot,

      //      Association
      _LoadingPointTxt
}
