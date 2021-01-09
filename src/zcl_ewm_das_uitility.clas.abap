CLASS zcl_ewm_das_uitility DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_ewm_das_utility .

    TYPES:
      BEGIN OF ENUM ty_change_mode STRUCTURE change_mode," Key checks are done separately
        create,
        update," Only fields that have been changed need to be checked
      END OF ENUM ty_change_mode STRUCTURE change_mode .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_ewm_das_uitility .
    "   With respect to the same method call of create/update/delete_travel() we have All or Nothing.
    "   I.e. when one of the levels contains an error, the complete call is refused.
    "   However, the buffer is not cleared in case of an error.
    "   I.e. when the caller wants to start over, he needs to call Initialize() explicitly.
    METHODS create_appointment
      IMPORTING
        !is_app       TYPE /scwm/s_dsapp_root_k
        !it_asgn_slot TYPE zewm_tt_d_dssla OPTIONAL
      EXPORTING
        !es_app       TYPE /scwm/d_dsapp
        !et_asgn_slot TYPE zewm_tt_d_dssla
        !et_messages  TYPE zif_ewm_das_utility=>tt_if_t100_message .
*                                    it_asgn_slotx            TYPE              OPTIONAL
    METHODS update_appointment
      IMPORTING
        !is_app       TYPE /scwm/s_dsapp_root_k
        !is_appx      TYPE zewm_s_x_d_dsapp
        !it_asgn_slot TYPE zewm_tt_d_dssla OPTIONAL
      EXPORTING
        !es_app       TYPE /scwm/d_dsapp
        !et_asgn_slot TYPE zewm_tt_d_dssla
        !et_messages  TYPE zif_ewm_das_utility=>tt_if_t100_message .
    METHODS delete_appointment
      IMPORTING
        !iv_app_db_key TYPE /bobf/conf_key
      EXPORTING
        !et_messages   TYPE zif_ewm_das_utility=>tt_if_t100_message .
    METHODS get_appointment
      IMPORTING
        !iv_app_id              TYPE /scwm/dsappid
        !iv_include_buffer      TYPE abap_boolean
        !iv_include_temp_buffer TYPE abap_boolean OPTIONAL
      EXPORTING
        !es_app                 TYPE /scwm/d_dsapp
        !et_asgn_slot           TYPE zewm_tt_d_dssla
        !et_messages            TYPE zif_ewm_das_utility=>tt_if_t100_message .
    METHODS save .
    METHODS initialize .
    METHODS convert_messages
      IMPORTING
        !it_messages TYPE zif_ewm_das_utility=>tt_if_t100_message
      EXPORTING
        !et_messages TYPE zewm_tt_message .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA go_instance TYPE REF TO zcl_ewm_das_uitility.


    METHODS lock_app IMPORTING iv_lock TYPE abap_bool
                     RAISING   zcx_das_exceptions ##RELAX ##NEEDED.

    METHODS _resolve_attribute IMPORTING iv_attrname      TYPE scx_attrname
                                         ix               TYPE REF TO zcx_das_exceptions
                               RETURNING VALUE(rv_symsgv) TYPE symsgv.
    "! Final determinations / derivations after all levels have been prepared, e.g. bottom-up derivations
    METHODS _determine EXPORTING et_messages TYPE zif_ewm_das_utility=>tt_if_t100_message
                       CHANGING  cs_travel   TYPE /scwm/d_dsapp
                                 ct_booking  TYPE zewm_tt_d_dssla.

ENDCLASS.



CLASS zcl_ewm_das_uitility IMPLEMENTATION.


  METHOD convert_messages.
    CLEAR et_messages.
    DATA ls_message TYPE symsg.
    LOOP AT it_messages INTO DATA(lr_error) ##INTO_OK.
      ls_message-msgty = 'E'.
      ls_message-msgid = lr_error->t100key-msgid.
      ls_message-msgno = lr_error->t100key-msgno.
      IF lr_error IS INSTANCE OF zcx_das_exceptions.
        DATA(lx) = CAST zcx_das_exceptions( lr_error ).
        ls_message-msgv1 = _resolve_attribute( iv_attrname = lr_error->t100key-attr1  ix = lx ).
        ls_message-msgv2 = _resolve_attribute( iv_attrname = lr_error->t100key-attr2  ix = lx ).
        ls_message-msgv3 = _resolve_attribute( iv_attrname = lr_error->t100key-attr3  ix = lx ).
        ls_message-msgv4 = _resolve_attribute( iv_attrname = lr_error->t100key-attr4  ix = lx ).
      ENDIF.
      APPEND ls_message TO et_messages.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_appointment.
    CLEAR: es_app, et_asgn_slot, et_messages.

    "Appointment
    lcl_app_buffer=>get_instance( )->cud_prep( EXPORTING it_app   = VALUE #( ( CORRESPONDING #( is_app ) ) )
                                                            it_appx  = VALUE #( ( dbkey = is_app-key  action_code = zif_ewm_das_utility=>action_code-create ) )
                                                  IMPORTING et_app   = DATA(lt_app)
                                                            et_messages = et_messages ).
*  IF et_messages IS INITIAL.
*    ASSERT lines( lt_travel ) = 1.
*    es_travel = lt_travel[ 1 ].
*  ENDIF.
*
*  " Bookings
*  IF et_messages IS INITIAL.
*    DATA lt_booking  TYPE /dmo/t_booking.
*    DATA lt_bookingx TYPE /dmo/t_bookingx.
*    LOOP AT it_booking INTO DATA(ls_booking_in).
*      DATA ls_booking TYPE /dmo/booking.
*      ls_booking = CORRESPONDING #( ls_booking_in ).
*      ls_booking-travel_id = es_travel-travel_id.
*      INSERT ls_booking INTO TABLE lt_booking.
*      INSERT VALUE #( travel_id = ls_booking-travel_id  booking_id = ls_booking-booking_id  action_code = /dmo/if_flight_legacy=>action_code-create ) INTO TABLE lt_bookingx.
*    ENDLOOP.
*    lcl_booking_buffer=>get_instance( )->cud_prep( EXPORTING it_booking  = lt_booking
*                                                             it_bookingx = lt_bookingx
*                                                   IMPORTING et_booking  = et_booking
*                                                             et_messages = DATA(lt_messages) ).
*    APPEND LINES OF lt_messages TO et_messages.
*  ENDIF.
*
*  " Booking Supplements
*  IF et_messages IS INITIAL.
*    DATA lt_booking_supplement  TYPE /dmo/t_booking_supplement.
*    DATA lt_booking_supplementx TYPE /dmo/t_booking_supplementx.
*    LOOP AT it_booking_supplement INTO DATA(ls_booking_supplement_in).
*      DATA ls_booking_supplement TYPE /dmo/book_suppl.
*      ls_booking_supplement = CORRESPONDING #( ls_booking_supplement_in ).
*      ls_booking_supplement-travel_id = es_travel-travel_id.
*      IF lcl_booking_buffer=>get_instance( )->check_booking_id( EXPORTING iv_travel_id = ls_booking_supplement-travel_id  iv_booking_id = ls_booking_supplement-booking_id CHANGING ct_messages = et_messages ) = abap_false.
*        EXIT.
*      ENDIF.
*      INSERT ls_booking_supplement INTO TABLE lt_booking_supplement.
*      INSERT VALUE #( travel_id             = ls_booking_supplement-travel_id
*                      booking_id            = ls_booking_supplement-booking_id
*                      booking_supplement_id = ls_booking_supplement-booking_supplement_id
*                      action_code           = /dmo/if_flight_legacy=>action_code-create ) INTO TABLE lt_booking_supplementx.
*    ENDLOOP.
*    IF et_messages IS INITIAL.
*      lcl_booking_supplement_buffer=>get_instance( )->cud_prep( EXPORTING it_booking_supplement  = lt_booking_supplement
*                                                                          it_booking_supplementx = lt_booking_supplementx
*                                                                IMPORTING et_booking_supplement  = et_booking_supplement
*                                                                          et_messages            = lt_messages ).
*      APPEND LINES OF lt_messages TO et_messages.
*    ENDIF.
*  ENDIF.
*
*  " Now do any derivations that require the whole business object (not only a single node), but which may in principle result in an error
*  IF et_messages IS INITIAL.
*    _determine( IMPORTING et_messages           = et_messages
*                CHANGING  cs_travel             = es_travel
*                          ct_booking            = et_booking
*                          ct_booking_supplement = et_booking_supplement ).
*  ENDIF.
*
    IF et_messages IS INITIAL.
      lcl_app_buffer=>get_instance( )->cud_copy( ).
    ELSE.
      CLEAR: es_app, et_asgn_slot.
      lcl_app_buffer=>get_instance( )->cud_disc( ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_appointment.
    CLEAR et_messages.

*
*  get_travel( EXPORTING iv_travel_id           = iv_travel_id
*                        iv_include_buffer      = abap_true
*                        iv_include_temp_buffer = abap_true
*              IMPORTING et_booking             = DATA(lt_booking)
*                        et_booking_supplement  = DATA(lt_booking_supplement)
*                        et_messages            = et_messages ).
*
*  IF et_messages IS INITIAL.
*    lcl_booking_supplement_buffer=>get_instance( )->cud_prep( EXPORTING it_booking_supplement  = CORRESPONDING #( lt_booking_supplement MAPPING travel_id             = travel_id
*                                                                                                                                                booking_id            = booking_id
*                                                                                                                                                booking_supplement_id = booking_supplement_id EXCEPT * )
*                                                                        it_booking_supplementx = VALUE #( FOR ls_bs IN lt_booking_supplement ( action_code           = /dmo/if_flight_legacy=>action_code-delete
*                                                                                                                                               travel_id             = ls_bs-travel_id
*                                                                                                                                               booking_id            = ls_bs-booking_id
*                                                                                                                                               booking_supplement_id = ls_bs-booking_supplement_id ) )
*                                                                        iv_no_delete_check     = abap_true " No existence check required
*                                                              IMPORTING et_messages            = DATA(lt_messages) ).
*    APPEND LINES OF lt_messages TO et_messages.
*  ENDIF.
*
*  IF et_messages IS INITIAL.
*    lcl_booking_buffer=>get_instance( )->cud_prep( EXPORTING it_booking         = CORRESPONDING #( lt_booking MAPPING travel_id = travel_id  booking_id = booking_id EXCEPT * )
*                                                             it_bookingx        = VALUE #( FOR ls_b IN lt_booking ( action_code = /dmo/if_flight_legacy=>action_code-delete  travel_id = ls_b-travel_id  booking_id = ls_b-booking_id ) )
*                                                             iv_no_delete_check = abap_true " No existence check required
*                                                   IMPORTING et_messages        = lt_messages ).
*    APPEND LINES OF lt_messages TO et_messages.
*  ENDIF.
*
*  IF et_messages IS INITIAL.
    lcl_app_buffer=>get_instance( )->cud_prep( EXPORTING it_app          = VALUE #( ( db_key = iv_app_db_key ) )
                                                            it_appx         = VALUE #( ( dbkey = iv_app_db_key  action_code = zif_ewm_das_utility=>action_code-delete ) )
                                                            iv_no_delete_check = abap_true " No existence check required
                                                  IMPORTING et_messages        = DATA(lt_messages) ).
    APPEND LINES OF lt_messages TO et_messages.
*  ENDIF.
*
*  IF et_messages IS INITIAL.
*    lcl_travel_buffer=>get_instance( )->cud_copy( ).
*    lcl_booking_buffer=>get_instance( )->cud_copy( ).
*    lcl_booking_supplement_buffer=>get_instance( )->cud_copy( ).
*  ELSE.
*    lcl_travel_buffer=>get_instance( )->cud_disc( ).
*    lcl_booking_buffer=>get_instance( )->cud_disc( ).
*    lcl_booking_supplement_buffer=>get_instance( )->cud_disc( ).
*  ENDIF.
  ENDMETHOD.


  METHOD get_appointment.
    CLEAR: es_app, et_asgn_slot, et_messages.

*  IF iv_travel_id IS INITIAL.
*    APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
*    RETURN.
*  ENDIF.
*
*  lcl_travel_buffer=>get_instance( )->get( EXPORTING it_travel              = VALUE #( ( travel_id = iv_travel_id ) )
*                                                     iv_include_buffer      = iv_include_buffer
*                                                     iv_include_temp_buffer = iv_include_temp_buffer
*                                           IMPORTING et_travel              = DATA(lt_travel) ).
*  IF lt_travel IS INITIAL.
*    APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = iv_travel_id ) TO et_messages.
*    RETURN.
*  ENDIF.
*  ASSERT lines( lt_travel ) = 1.
*  es_travel = lt_travel[ 1 ].
*
*  lcl_booking_buffer=>get_instance( )->get( EXPORTING it_booking             = VALUE #( ( travel_id = iv_travel_id ) )
*                                                      iv_include_buffer      = iv_include_buffer
*                                                      iv_include_temp_buffer = iv_include_temp_buffer
*                                            IMPORTING et_booking             = et_booking ).
*
*  lcl_booking_supplement_buffer=>get_instance( )->get( EXPORTING it_booking_supplement  = CORRESPONDING #( et_booking MAPPING travel_id = travel_id  booking_id = booking_id EXCEPT * )
*                                                                 iv_include_buffer      = iv_include_buffer
*                                                                 iv_include_temp_buffer = iv_include_temp_buffer
*                                                       IMPORTING et_booking_supplement  = et_booking_supplement ).
  ENDMETHOD.


  METHOD get_instance.
    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD initialize.
*  lcl_travel_buffer=>get_instance( )->initialize( ).
*  lcl_booking_buffer=>get_instance( )->initialize( ).
*  lcl_booking_supplement_buffer=>get_instance( )->initialize( ).
  ENDMETHOD.


  METHOD lock_app ##NEEDED.

  ENDMETHOD.


  METHOD save.
*  lcl_travel_buffer=>get_instance( )->save( ).
*  lcl_booking_buffer=>get_instance( )->save( ).
*  lcl_booking_supplement_buffer=>get_instance( )->save( ).
*  initialize( ).
  ENDMETHOD.


  METHOD update_appointment.
    CLEAR: es_app, et_asgn_slot, et_messages.

    "Appointment
    IF is_app-key IS INITIAL.
      APPEND NEW zcx_das_exceptions( textid = zcx_das_exceptions=>appointment_no_key ) TO et_messages.
      RETURN.
    ENDIF.
    DATA ls_appx TYPE zewm_s_x_d_dsapp.
    ls_appx = CORRESPONDING #( is_appx ).
    ls_appx-action_code = 'U'.
    lcl_app_buffer=>get_instance( )->cud_prep( EXPORTING it_app   = VALUE #( ( CORRESPONDING #( is_app MAPPING db_key = key ) ) )
                                                         it_appx  = VALUE #( ( ls_appx ) )
                                                  IMPORTING et_app   = DATA(lt_app)
                                                            et_messages = et_messages ).

    IF et_messages IS INITIAL.
      lcl_app_buffer=>get_instance( )->cud_copy( ).
    ELSE.
      CLEAR: es_app, et_asgn_slot.
      lcl_app_buffer=>get_instance( )->cud_disc( ).
    ENDIF.

  ENDMETHOD.


  METHOD _determine.

  ENDMETHOD.


  METHOD _resolve_attribute.
    CLEAR rv_symsgv.
    CASE iv_attrname.
      WHEN ''.
        rv_symsgv = ''.
      WHEN 'MV_APP_ID'.
        rv_symsgv = |{ ix->mv_app_id ALPHA = OUT }|.
      WHEN OTHERS.
*        ASSERT 1 = 2.
        rv_symsgv = iv_attrname.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
