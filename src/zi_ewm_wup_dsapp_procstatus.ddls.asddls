@AbapCatalog.sqlViewName: 'ZIEWMDSAPPSTAT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Appointment Process Status'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.resultSet.sizeCategory: #XS
@ObjectModel.representativeKey: 'DSAPP_PROCSTATUS'
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view ZI_EWM_WUP_DSAPP_PROCSTATUS
  as select from dd07t
{      
  key domvalue_l as ProcessStatus,
      @Semantics.language: true
  key ddlanguage as Language,
      ddtext     as StatusText
}
where
      domname    = '/SCWM/DSAPP_PROCSTATUS'
  and ddlanguage = $session.system_language;
