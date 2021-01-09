@AbapCatalog.sqlViewName: 'ZIEWMDSSLA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@ClientHandling.algorithm: #SESSION_VARIABLE
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Appointment Slot Assignment'
@ObjectModel.modelCategory: #BUSINESS_OBJECT
@ObjectModel.usageType: {serviceQuality: #A,
                         sizeCategory:   #XL,
                         dataClass:      #TRANSACTIONAL}

@VDM.viewType: #BASIC

/******************************************************************************************
    Author:     Shyam Borgikar
    Comments:   Basic View Root for Appointment Slot Assignment

    History:    1)(Author)-(Change Reason)
                2)

    To Do:      1) Build the complete Object Model


        |
        |-- <child node> - To be implemented
        |-- <child node> - To be implemented
******************************************************************************************/
define view ZI_EWM_WUP_D_DSSLA
  as select from /scwm/d_dssla as AssignedSlot
  association to parent ZI_EWM_WUP_D_DSAPP as _DockAppointment on $projection.ParentKey = _DockAppointment.DbKey
{

  key db_key                                                      as DbKey,
      parent_key                                                  as ParentKey,
      loadpoint                                                   as Loadpoint,
      //      start_time    as StartTime,
      cast(start_time as zewm_de_dsslot_start preserving type )   as StartTime,
      //      finish_time   as FinishTime,
      cast(finish_time as zewm_de_dsslot_finish preserving type ) as FinishTime,
      slot_root_key                                               as SlotRootKey,
      capalloc                                                    as Capalloc,

      //      Associations
      _DockAppointment

}
