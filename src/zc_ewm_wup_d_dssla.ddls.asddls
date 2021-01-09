@EndUserText.label: 'Projection for DAS Slot Assignment'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.allowExtensions: true
define view entity ZC_EWM_WUP_D_DSSLA
  as projection on ZI_EWM_WUP_D_DSSLA as AssignedSlot
{
  key DbKey,
      ParentKey,
      @Search.defaultSearchElement: true
      Loadpoint,
      @Search.defaultSearchElement: true
      StartTime,
      @Search.defaultSearchElement: true
      FinishTime,
      SlotRootKey,
      Capalloc,
      /* Associations */
      _DockAppointment : redirected to parent ZC_EWM_WUP_D_DSAPP
}
