@AbapCatalog.sqlViewName: 'ZIEWMDSPLPT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Loding point Description'
@ObjectModel.dataCategory: #TEXT
@ObjectModel.resultSet.sizeCategory: #XS
@ObjectModel.representativeKey: 'DSAPP_PROCSTATUS'
define view ZI_EWM_WUP_D_DSPLPT
  as select from /scwm/d_dslpt as LoadingPointTxt
{
  key    db_key     as DbKey,
         @Semantics.language: true
  key    language   as Language,
         parent_key as ParentKey,
         text       as Text
}
where
  language = $session.system_language;
