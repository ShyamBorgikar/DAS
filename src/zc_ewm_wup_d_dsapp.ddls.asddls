@EndUserText.label: 'Projection view for Dock Appointment'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.allowExtensions: true

define root view entity ZC_EWM_WUP_D_DSAPP
  as projection on ZI_EWM_WUP_D_DSAPP as DockAppointment
{
  key DbKey,
//      @UI.selectionField: [{ position : 20 }]
//      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_EWM_WUP_D_DSLP', element: 'Loadpoint' } } ]
      @Search.defaultSearchElement: true
      Loadpoint,
      StartTime,
      FinishTime,
      SlotRootKey,
      @Search.defaultSearchElement: true
      Docno,
      Carrier,
      ScacCarr,
      Transmeansid,
      @UI.selectionField: [{ position : 40 }]
      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_MeansOfTransportText', element: 'MeansOfTransport' } } ]
      @Search.defaultSearchElement: true
      Mtr,
      Motscode,
      Driver,
      Capalloc,
      Carrcreaind,
      AppLength,
      ReqStartTime,
      ReqFinishTime,
      ReqLength,
      @UI.selectionField: [{ position : 30 }]
      @ObjectModel.text.element: ['ProcstatusName']
      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_EWM_WUP_DSAPP_PROCSTATUS', element: 'ProcessStatus' } } ]
      @Search.defaultSearchElement: true
      Procstatus,
      ProcstatusName,
      Criticality,
      AppType,
      CheckinTime,
      DockTime,
      UndockTime,
      CheckoutTime,
      Note,
      Refdocno,
      LpParent,
      NumberPackages,
      ProdGrp,
      Category,
      LoadpointInd,
      InYardLength,
      RecurringInd,
      StagingInd,
      PlCheckinTime,
      CreatedBy,
      CreatedOn,
      ChangedBy,
      ChangedOn,
      EewDsappRoot,

      /* Associations */
      _AssignedSlot : redirected to composition child ZC_EWM_WUP_D_DSSLA,
      _RefDocument  : redirected to composition child ZC_EWM_WUP_D_DSREFDO,
      _ProcessStatusTxt,
      _LoadingPoint,
      _MeansOfTransport
}
