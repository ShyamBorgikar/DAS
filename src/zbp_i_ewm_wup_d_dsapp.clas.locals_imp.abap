CLASS lhc_dockappointment DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE dockappointment.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE dockappointment.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE dockappointment.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK dockappointment.

    METHODS read FOR READ
      IMPORTING keys FOR READ dockappointment RESULT result.

    METHODS cba_assignedslot FOR MODIFY
      IMPORTING entities_cba FOR CREATE dockappointment\_assignedslot.

    METHODS rba_assignedslot FOR READ
      IMPORTING keys_rba FOR READ dockappointment\_assignedslot FULL result_requested RESULT result LINK association_links.

    METHODS get_features FOR FEATURES
      IMPORTING keys REQUEST requested_features FOR dockappointment RESULT result.

    METHODS confirm_appointment FOR MODIFY
      IMPORTING keys FOR ACTION dockappointment~confirmappointment RESULT result.
ENDCLASS.

CLASS lhc_dockappointment IMPLEMENTATION.

  METHOD create.

    DATA legacy_entity_in   TYPE /scwm/d_dsapp.
    DATA legacy_entity_x TYPE zewm_s_x_d_dsapp . "refers to x structure (> BAPIs)
    DATA messages TYPE zewm_tt_message.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

      legacy_entity_in = CORRESPONDING #( <entity> MAPPING FROM ENTITY USING CONTROL ).

      CALL FUNCTION 'ZEWM_DSAPP_CREATE'
        EXPORTING
          is_app      = CORRESPONDING /scwm/s_dsapp_root_k( legacy_entity_in MAPPING key = db_key )
*         it_asgn_slot =
        IMPORTING
          et_messages = messages.

      IF messages IS INITIAL.

        APPEND VALUE #( dbkey = legacy_entity_in-db_key ) TO mapped-dockappointment.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( dbkey = legacy_entity_in-db_key ) TO failed-dockappointment.
        "fill reported structure to be displayed on the UI
        APPEND VALUE #( dbkey = legacy_entity_in-db_key
                        %msg = new_message( id = messages[ 1 ]-msgid
                                            number = messages[ 1 ]-msgno
                                            v1 = messages[ 1 ]-msgv1
                                            v2 = messages[ 1 ]-msgv2
                                            v3 = messages[ 1 ]-msgv3
                                            v4 = messages[ 1 ]-msgv4
                                            severity = CONV #( messages[ 1 ]-msgty ) )
       ) TO reported-dockappointment.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD delete.

    DATA messages TYPE zewm_tt_message.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).

      CALL FUNCTION 'ZEWM_DSAPP_DELETE'
        EXPORTING
          iv_app_db_key = <key>-dbkey
        IMPORTING
          et_messages   = messages.

      IF messages IS INITIAL.

        APPEND VALUE #( dbkey = <key>-dbkey ) TO mapped-dockappointment.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( dbkey = <key>-dbkey ) TO failed-dockappointment.
        "fill reported structure to be displayed on the UI
        APPEND VALUE #( dbkey = <key>-dbkey
                        %msg = new_message( id = messages[ 1 ]-msgid
                                            number = messages[ 1 ]-msgno
                                            v1 = messages[ 1 ]-msgv1
                                            v2 = messages[ 1 ]-msgv2
                                            v3 = messages[ 1 ]-msgv3
                                            v4 = messages[ 1 ]-msgv4
                                            severity = CONV #( messages[ 1 ]-msgty ) )
       ) TO reported-dockappointment.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD update.

    DATA legacy_entity_in   TYPE /scwm/d_dsapp.
    DATA legacy_entity_x TYPE zewm_s_x_d_dsapp . "refers to x structure (> BAPIs)
    DATA messages TYPE zewm_tt_message.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

      legacy_entity_in = CORRESPONDING #( <entity> MAPPING FROM ENTITY ).
      legacy_entity_x = CORRESPONDING zewm_s_x_d_dsapp( <entity> MAPPING FROM ENTITY ).
      legacy_entity_x-dbkey = <entity>-dbkey.

      CALL FUNCTION 'ZEWM_DSAPP_UPDATE'
        EXPORTING
          is_app      = CORRESPONDING /scwm/s_dsapp_root_k( legacy_entity_in MAPPING key = db_key )
          is_appx     = legacy_entity_x
*         it_asgn_slot =
        IMPORTING
          et_messages = messages.

      IF messages IS INITIAL.

        APPEND VALUE #( dbkey = legacy_entity_in-db_key ) TO mapped-dockappointment.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( dbkey = legacy_entity_in-db_key ) TO failed-dockappointment.
        "fill reported structure to be displayed on the UI
        APPEND VALUE #( dbkey = legacy_entity_in-db_key
                        %msg = new_message( id = messages[ 1 ]-msgid
                                            number = messages[ 1 ]-msgno
                                            v1 = messages[ 1 ]-msgv1
                                            v2 = messages[ 1 ]-msgv2
                                            v3 = messages[ 1 ]-msgv3
                                            v4 = messages[ 1 ]-msgv4
                                            severity = CONV #( messages[ 1 ]-msgty ) )
       ) TO reported-dockappointment.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD cba_assignedslot.
  ENDMETHOD.

  METHOD rba_assignedslot.
  ENDMETHOD.

  METHOD get_features.
* READ ENTITY ZI_EWM_WUP_D_DSAPP
*         FIELDS ( Note )
*          WITH CORRESPONDING #( keys )
*         RESULT DATA(lt_result_variable).
*
*    " Return feature control information
*    result = VALUE #( FOR ls_variable IN lt_result_variable
*             ( %KEY                          = ls_variable-%KEY
*    " Field control information
**               %field-Note                   = if_abap_behv=>fc-f-read_only
**               %field-field2                 = if_abap_behv=>fc-f-mandatory
*    " Action control information
**               %features-%action-action_name = COND #( WHEN condition
**                                                       THEN if_abap_behv=>fc-o-disabled
**                                                       ELSE if_abap_behv=>fc-o-enabled   )
*    " Operation (example: update) control information
*               %features-%create           =    if_abap_behv=>fc-o-disabled   ) ).
  ENDMETHOD.

  METHOD confirm_appointment.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zi_ewm_wup_d_dsapp DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS check_before_save REDEFINITION.

    METHODS finalize          REDEFINITION.

    METHODS save              REDEFINITION.

ENDCLASS.

CLASS lsc_zi_ewm_wup_d_dsapp IMPLEMENTATION.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD finalize.
  ENDMETHOD.

  METHOD save.
    CALL FUNCTION 'ZEWM_BOPF_TRANSACTION_MGR'.
  ENDMETHOD.

ENDCLASS.
