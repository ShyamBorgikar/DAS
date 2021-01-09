@EndUserText.label: 'Projection view App. Reference Document'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.allowExtensions: true

define view entity ZC_EWM_WUP_D_DSREFDO
  as projection on ZI_EWM_WUP_D_DSREFDO as RefDocument
{
  key DbKey,
      ParentKey,
      @Search.defaultSearchElement: true
      Refdocno,
      Refdoccat,
      Refbskey,
      /* Associations */
      _DockAppointment: redirected to parent ZC_EWM_WUP_D_DSAPP
}
