@AbapCatalog.sqlViewName: 'ZIEWMDSREFDO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@ClientHandling.algorithm: #SESSION_VARIABLE
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Appointment Reference Document'
@ObjectModel.modelCategory: #BUSINESS_OBJECT
@ObjectModel.usageType: {serviceQuality: #A,
                         sizeCategory:   #XL,
                         dataClass:      #TRANSACTIONAL}

@VDM.viewType: #BASIC
/******************************************************************************************
    Author:     Shyam Borgikar
    Comments:   Basic View Root for Appointment Reference Document

    History:    1)(Author)-(Change Reason)
                2)

    To Do:      1) Build the complete Object Model


        |
        |-- <child node> - To be implemented
        |-- <child node> - To be implemented
******************************************************************************************/
define view ZI_EWM_WUP_D_DSREFDO
  as select from /scwm/d_dsrefdo as RefDocument
  association to parent ZI_EWM_WUP_D_DSAPP as _DockAppointment on $projection.ParentKey = _DockAppointment.DbKey
{

  key db_key     as DbKey,
      parent_key as ParentKey,
      refdocno   as Refdocno,
      refdoccat  as Refdoccat,
      refbskey   as Refbskey,

      //      Association
      _DockAppointment

}
