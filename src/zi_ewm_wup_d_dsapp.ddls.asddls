@AbapCatalog.sqlViewName: 'ZIEWMDSAPP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@ClientHandling.algorithm: #SESSION_VARIABLE
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dock Appointment'
@ObjectModel.modelCategory: #BUSINESS_OBJECT
@ObjectModel.usageType: {serviceQuality: #A,
                         sizeCategory:   #XL,
                         dataClass:      #TRANSACTIONAL}

@VDM.viewType: #BASIC

/******************************************************************************************
    Author:     Shyam Borgikar
    Comments:   Basic View Dock Appointment

    History:    1)(Author)-(Change Reason)
                2)

    To Do:      1) Build the complete Object Model


        |
        |-- <child node> - To be implemented
        |-- <child node> - To be implemented
******************************************************************************************/

define root view ZI_EWM_WUP_D_DSAPP
  as select from /scwm/d_dsapp as DockAppointment
  composition [0..*] of ZI_EWM_WUP_D_DSSLA          as _AssignedSlot
  composition [0..*] of ZI_EWM_WUP_D_DSREFDO        as _RefDocument
  association [0..1] to ZI_EWM_WUP_DSAPP_PROCSTATUS as _ProcessStatusTxt on  $projection.Procstatus     = _ProcessStatusTxt.ProcessStatus
                                                                         and _ProcessStatusTxt.Language = $session.system_language
  association [0..*] to ZI_EWM_WUP_D_DSLP           as _LoadingPoint     on  $projection.Loadpoint = _LoadingPoint.Loadpoint
  association [0..*] to I_MeansOfTransportText      as _MeansOfTransport on  $projection.Mtr = _MeansOfTransport.MeansOfTransport

{

  key db_key                                                           as DbKey,
      loadpoint                                                        as Loadpoint,
      //      start_time      as StartTime,
      cast(start_time as timestamp preserving type )                   as StartTime,
      //      finish_time                   as FinishTime,
      cast(finish_time as timestamp preserving type )                  as FinishTime,
      slot_root_key                                                    as SlotRootKey,
      docno                                                            as Docno,
      carrier                                                          as Carrier,
      scac_carr                                                        as ScacCarr,
      transmeansid                                                     as Transmeansid,
      mtr                                                              as Mtr,
      motscode                                                         as Motscode,
      driver                                                           as Driver,
      capalloc                                                         as Capalloc,
      carrcreaind                                                      as Carrcreaind,
      //      app_length                     as AppLength,
      cast(app_length as abap.int4(10))                                as AppLength,
      //      req_start_time                 as ReqStartTime,
      cast(req_start_time as timestamp preserving type )               as ReqStartTime,
      //      req_finish_time                as ReqFinishTime,
      cast(req_finish_time as timestamp preserving type )              as ReqFinishTime,
      //      req_length                     as ReqLength,
      cast(req_length as abap.int4(10))                                as ReqLength,
      procstatus                                                       as Procstatus,
      _ProcessStatusTxt.StatusText                                     as ProcstatusName,
      case DockAppointment.procstatus
          when '' then 0
          when 'F' then 2
          when 'S' then 1
          when 'B' then 3
          when 'C' then 3
          when 'D' then 3
          when 'E' then 3
          else 0
          end                                                          as Criticality,
      app_type                                                         as AppType,
      //      checkin_time                   as CheckinTime,
      cast(checkin_time as timestamp preserving type )                 as CheckinTime,
      //      dock_time                      as DockTime,
      cast(dock_time as timestamp preserving type )                    as DockTime,
      //      undock_time                    as UndockTime,
      cast(undock_time as timestamp preserving type )                  as UndockTime,
      //      checkout_time                  as CheckoutTime,
      cast(checkout_time as timestamp preserving type )                as CheckoutTime,
      note                                                             as Note,
      refdocno                                                         as Refdocno,
      lp_parent                                                        as LpParent,
      number_packages                                                  as NumberPackages,
      prod_grp                                                         as ProdGrp,
      category                                                         as Category,
      loadpoint_ind                                                    as LoadpointInd,
      //      in_yard_length                 as InYardLength,
      cast(in_yard_length as abap.int4(10))                            as InYardLength,
      recurring_ind                                                    as RecurringInd,
      staging_ind                                                      as StagingInd,
      //      pl_checkin_time                as PlCheckinTime,
      cast(pl_checkin_time as timestamp preserving type )              as PlCheckinTime,
      @Semantics.user.createdBy: true
      created_by                                                       as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      //      created_on                            as CreatedOn,
      cast(created_on as zewm_de_tstmp_creation_time preserving type ) as CreatedOn,
      @Semantics.user.lastChangedBy: true
      changed_by                                                       as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      //      changed_on                            as ChangedOn,
      cast(changed_on as zewm_de_tstmp_changed_time preserving type )  as ChangedOn,
      eew_dsapp_root                                                   as EewDsappRoot,

      // Associations
      _AssignedSlot,
      _RefDocument,
      _ProcessStatusTxt,
      _LoadingPoint,
      _MeansOfTransport
}
