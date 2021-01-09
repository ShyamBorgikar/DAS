FUNCTION zewm_dsapp_delete.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_APP_DB_KEY) TYPE  /BOBF/CONF_KEY
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  ZEWM_TT_MESSAGE
*"----------------------------------------------------------------------
  CLEAR et_messages.

  zcl_ewm_das_uitility=>get_instance( )->delete_appointment(
    EXPORTING
      iv_app_db_key    = iv_app_db_key
    IMPORTING
      et_messages = DATA(lt_messages)  ).

  zcl_ewm_das_uitility=>get_instance( )->convert_messages( EXPORTING it_messages = lt_messages
                                                           IMPORTING et_messages = et_messages ).

ENDFUNCTION.
