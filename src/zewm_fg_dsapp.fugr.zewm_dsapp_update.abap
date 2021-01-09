FUNCTION zewm_dsapp_update.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_APP) TYPE  /SCWM/S_DSAPP_ROOT_K
*"     REFERENCE(IS_APPX) TYPE  ZEWM_S_X_D_DSAPP
*"     REFERENCE(IT_ASGN_SLOT) TYPE  ZEWM_TT_D_DSSLA OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_APP) TYPE  /SCWM/D_DSAPP
*"     REFERENCE(ET_ASGN_SLOT) TYPE  ZEWM_TT_D_DSSLA
*"     REFERENCE(ET_MESSAGES) TYPE  ZEWM_TT_MESSAGE
*"----------------------------------------------------------------------
  CLEAR es_app.
  CLEAR et_asgn_slot.
  CLEAR et_messages.

  zcl_ewm_das_uitility=>get_instance( )->update_appointment(
    EXPORTING
      is_app       = is_app
      is_appx      = is_appx
      it_asgn_slot = it_asgn_slot
    IMPORTING
      es_app       = es_app
      et_asgn_slot = et_asgn_slot
      et_messages  = DATA(lt_messages)  ).

  zcl_ewm_das_uitility=>get_instance( )->convert_messages( EXPORTING it_messages = lt_messages
                                                            IMPORTING et_messages = et_messages ).

ENDFUNCTION.
