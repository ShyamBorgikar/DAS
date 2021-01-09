*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_common_checks DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.

  PRIVATE SECTION.

ENDCLASS.


CLASS lcl_common_checks IMPLEMENTATION.

ENDCLASS.

CLASS lcl_booking_buffer DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
*    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_booking_buffer.
*    METHODS save.
*    METHODS initialize.
*    METHODS check_booking_id IMPORTING iv_travel_id       TYPE /dmo/travel_id
*                                       iv_booking_id      TYPE /dmo/booking_id
*                             CHANGING  ct_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message
*                             RETURNING VALUE(rv_is_valid) TYPE abap_bool.
*    "! Prepare changes in a temporary buffer
*    "! @parameter iv_no_delete_check | In some cases we do not need to check the existence of a record to be deleted, as this check has been done before.
*    "!                               | E.g. delete all subnodes of a node to be deleted.  In this case we have read the subnodes to get their keys.
*    METHODS cud_prep IMPORTING it_booking         TYPE /dmo/t_booking
*                               it_bookingx        TYPE /dmo/t_bookingx
*                               iv_no_delete_check TYPE abap_bool OPTIONAL
*                     EXPORTING et_booking         TYPE /dmo/t_booking
*                               et_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message.
*    "! Add content of the temporary buffer to the real buffer and clear the temporary buffer
*    METHODS cud_copy.
*    "! Discard content of the temporary buffer
*    METHODS cud_disc.
*    "! Get all Bookings for given Travels
*    METHODS get IMPORTING it_booking             TYPE /dmo/t_booking
*                          iv_include_buffer      TYPE abap_boolean
*                          iv_include_temp_buffer TYPE abap_boolean
*                EXPORTING et_booking             TYPE /dmo/t_booking.
*
*  PRIVATE SECTION.
*    CLASS-DATA go_instance TYPE REF TO lcl_booking_buffer.
*    " Main buffer
*    DATA: mt_create_buffer TYPE /dmo/t_booking,
*          mt_update_buffer TYPE /dmo/t_booking,
*          mt_delete_buffer TYPE /dmo/t_booking_key.
*    " Temporary buffer valid during create / update / delete Travel
*    DATA: mt_create_buffer_2 TYPE /dmo/t_booking,
*          mt_update_buffer_2 TYPE /dmo/t_booking,
*          mt_delete_buffer_2 TYPE /dmo/t_booking_key.
*
*    TYPES: BEGIN OF ts_flight_key,
*             carrier_id    TYPE /dmo/carrier_id,
*             connection_id TYPE /dmo/connection_id,
*             flight_date   TYPE /dmo/flight_date,
*           END OF ts_flight_key.
*    TYPES tt_flight_key TYPE SORTED TABLE OF ts_flight_key WITH UNIQUE KEY carrier_id  connection_id  flight_date.
*    DATA mt_flight_key TYPE tt_flight_key.
*
*    METHODS _create IMPORTING it_booking  TYPE /dmo/t_booking
*                    EXPORTING et_booking  TYPE /dmo/t_booking
*                              et_messages TYPE zif_ewm_das_utility=>tt_if_t100_message.
*    METHODS _update IMPORTING it_booking  TYPE /dmo/t_booking
*                              it_bookingx TYPE /dmo/t_bookingx
*                    EXPORTING et_booking  TYPE /dmo/t_booking
*                              et_messages TYPE zif_ewm_das_utility=>tt_if_t100_message.
*    METHODS _delete IMPORTING it_booking         TYPE /dmo/t_booking
*                              iv_no_delete_check TYPE abap_bool
*                    EXPORTING et_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message.
*
*    METHODS _check IMPORTING is_booking         TYPE /dmo/booking
*                             is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
*                             iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
*                   CHANGING  ct_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message
*                   RETURNING VALUE(rv_is_valid) TYPE abap_bool.
*    METHODS _check_booking_date IMPORTING is_booking         TYPE /dmo/booking
*                                          is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
*                                          iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
*                                CHANGING  ct_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message
*                                RETURNING VALUE(rv_is_valid) TYPE abap_bool.
*    METHODS _check_customer IMPORTING is_booking         TYPE /dmo/booking
*                                      is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
*                                      iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
*                            CHANGING  ct_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message
*                            RETURNING VALUE(rv_is_valid) TYPE abap_bool.
*    METHODS _check_flight IMPORTING is_booking         TYPE /dmo/booking
*                                    is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
*                                    iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
*                          CHANGING  ct_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message
*                          RETURNING VALUE(rv_is_valid) TYPE abap_bool.
*    METHODS _check_currency_code IMPORTING is_booking         TYPE /dmo/booking
*                                           is_bookingx        TYPE /dmo/s_bookingx OPTIONAL
*                                           iv_change_mode     TYPE /dmo/cl_flight_legacy=>ty_change_mode
*                                 CHANGING  ct_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message
*                                 RETURNING VALUE(rv_is_valid) TYPE abap_bool.
*    METHODS _determine IMPORTING iv_change_mode TYPE /dmo/cl_flight_legacy=>ty_change_mode
*                       CHANGING  cs_booking     TYPE /dmo/booking.
ENDCLASS.

CLASS lcl_booking_buffer IMPLEMENTATION.
*  METHOD get_instance.
*    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
*    ro_instance = go_instance.
*  ENDMETHOD.
*
*
*  METHOD _create.
*    CLEAR et_booking.
*    CLEAR et_messages.
*
*    TYPES: BEGIN OF ts_travel_booking_id,
*             travel_id  TYPE /dmo/travel_id,
*             booking_id TYPE /dmo/booking_id,
*           END OF ts_travel_booking_id,
*           tt_travel_booking_id TYPE SORTED TABLE OF ts_travel_booking_id WITH UNIQUE KEY travel_id booking_id.
*    DATA lt_travel_booking_id TYPE tt_travel_booking_id.
*
*    CHECK it_booking IS NOT INITIAL.
*
*    SELECT FROM /dmo/booking FIELDS travel_id, booking_id FOR ALL ENTRIES IN @it_booking WHERE travel_id = @it_booking-travel_id AND booking_id = @it_booking-booking_id INTO CORRESPONDING FIELDS OF TABLE @lt_travel_booking_id.
*
*    LOOP AT it_booking INTO DATA(ls_booking_create) ##INTO_OK.
*      " Booking_ID key must not be initial
*      IF ls_booking_create-booking_id IS INITIAL.
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_no_key  travel_id = ls_booking_create-travel_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
*      " Booking_ID key check DB
*      READ TABLE lt_travel_booking_id TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = ls_booking_create-travel_id  booking_id = ls_booking_create-booking_id.
*      IF sy-subrc = 0.
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_exists  travel_id = ls_booking_create-travel_id  booking_id = ls_booking_create-booking_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
*      " Booking_ID key check Buffer
*      READ TABLE mt_create_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = ls_booking_create-travel_id  booking_id = ls_booking_create-booking_id.
*      IF sy-subrc = 0.
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_exists  travel_id = ls_booking_create-travel_id  booking_id = ls_booking_create-booking_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
*      " Checks
*      IF _check( EXPORTING is_booking     = ls_booking_create
*                           iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
*                 CHANGING  ct_messages    = et_messages ) = abap_false.
*        RETURN.
*      ENDIF.
*
*      " standard determinations
*      _determine( EXPORTING iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
*                  CHANGING cs_booking      = ls_booking_create ).
*
*      INSERT ls_booking_create INTO TABLE mt_create_buffer_2.
*    ENDLOOP.
*
*    et_booking = mt_create_buffer_2.
*  ENDMETHOD.
*
*
*  METHOD _update.
*    CLEAR et_booking.
*    CLEAR et_messages.
*
*    CHECK it_booking IS NOT INITIAL.
*
*    " Check for empty keys
*    LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<s_booking_update>) WHERE booking_id = 0. "#EC CI_SORTSEQ
*      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_no_key  travel_id = <s_booking_update>-travel_id ) TO et_messages.
*      RETURN.
*    ENDLOOP.
*
*    DATA lt_booking TYPE SORTED TABLE OF /dmo/booking WITH UNIQUE KEY travel_id  booking_id.
*    SELECT * FROM /dmo/booking FOR ALL ENTRIES IN @it_booking WHERE travel_id = @it_booking-travel_id AND booking_id = @it_booking-booking_id INTO TABLE @lt_booking.
*
*    FIELD-SYMBOLS <s_buffer_booking> TYPE /dmo/booking.
*    DATA ls_buffer_booking TYPE /dmo/booking.
*    LOOP AT it_booking ASSIGNING <s_booking_update>.
*      UNASSIGN <s_buffer_booking>.
*
*      READ TABLE mt_delete_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
*      IF sy-subrc = 0." Error: Record to be updated marked for deletion
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
*      IF <s_buffer_booking> IS NOT ASSIGNED." Special case: record already in temporary create buffer
*        READ TABLE mt_create_buffer_2 ASSIGNING <s_buffer_booking> WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
*      ENDIF.
*
*      IF <s_buffer_booking> IS NOT ASSIGNED." Special case: record already in create buffer
*        READ TABLE mt_create_buffer INTO ls_buffer_booking WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
*        IF sy-subrc = 0.
*          INSERT ls_buffer_booking INTO TABLE mt_create_buffer_2 ASSIGNING <s_buffer_booking>.
*        ENDIF.
*      ENDIF.
*
*      IF <s_buffer_booking> IS NOT ASSIGNED." Special case: record already in temporary update buffer
*        READ TABLE mt_update_buffer_2 ASSIGNING <s_buffer_booking> WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
*      ENDIF.
*
*      IF <s_buffer_booking> IS NOT ASSIGNED." Special case: record already in update buffer
*        READ TABLE mt_update_buffer INTO ls_buffer_booking WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
*        IF sy-subrc = 0.
*          INSERT ls_buffer_booking INTO TABLE mt_update_buffer_2 ASSIGNING <s_buffer_booking>.
*        ENDIF.
*      ENDIF.
*
*      IF <s_buffer_booking> IS NOT ASSIGNED." Usual case: record not already in update buffer
*        READ TABLE lt_booking ASSIGNING FIELD-SYMBOL(<s_booking_old>) WITH TABLE KEY travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id.
*        IF sy-subrc = 0.
*          INSERT <s_booking_old> INTO TABLE mt_update_buffer_2 ASSIGNING <s_buffer_booking>.
*          ASSERT sy-subrc = 0.
*        ENDIF.
*      ENDIF.
*
*      " Error
*      IF <s_buffer_booking> IS NOT ASSIGNED.
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = <s_booking_update>-travel_id  booking_id = <s_booking_update>-booking_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
*      " Merge fields to be updated
*      READ TABLE it_bookingx ASSIGNING FIELD-SYMBOL(<s_bookingx>) WITH KEY travel_id   = <s_booking_update>-travel_id
*                                                                           booking_id  = <s_booking_update>-booking_id
*                                                                           action_code = zif_ewm_das_utility=>action_code-update.
*      IF sy-subrc <> 0.
*        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_no_control
*                                          travel_id  = <s_booking_update>-travel_id
*                                          booking_id = <s_booking_update>-booking_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
*      IF   ( <s_bookingx>-carrier_id    = abap_true AND ( <s_booking_update>-carrier_id    <> <s_buffer_booking>-carrier_id    ) )
*        OR ( <s_bookingx>-connection_id = abap_true AND ( <s_booking_update>-connection_id <> <s_buffer_booking>-connection_id ) )
*        OR ( <s_bookingx>-flight_date   = abap_true AND ( <s_booking_update>-flight_date   <> <s_buffer_booking>-flight_date   ) ).
*        " The flight must not be changed (delete the record and create a new one)
*        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_flight_u
*                                          travel_id  = <s_booking_update>-travel_id
*                                          booking_id = <s_booking_update>-booking_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
*      IF   ( <s_bookingx>-flight_price = abap_true  AND <s_bookingx>-currency_code = abap_false )
*        OR ( <s_bookingx>-flight_price = abap_false AND <s_bookingx>-currency_code = abap_true  ).
*        " Price and currency code must be changed together
*        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_price_currency_u
*                                          travel_id  = <s_booking_update>-travel_id
*                                          booking_id = <s_booking_update>-booking_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
*      DATA lv_field TYPE i.
*      lv_field = 4.
*      DO.
*        ASSIGN COMPONENT lv_field OF STRUCTURE <s_bookingx> TO FIELD-SYMBOL(<v_flag>).
*        IF sy-subrc <> 0.
*          EXIT.
*        ENDIF.
*        IF <v_flag> = abap_true.
*          ASSIGN COMPONENT lv_field OF STRUCTURE <s_booking_update> TO FIELD-SYMBOL(<v_field_new>).
*          ASSERT sy-subrc = 0.
*          ASSIGN COMPONENT lv_field OF STRUCTURE <s_buffer_booking> TO FIELD-SYMBOL(<v_field_old>).
*          ASSERT sy-subrc = 0.
*          <v_field_old> = <v_field_new>.
*        ENDIF.
*        lv_field = lv_field + 1.
*      ENDDO.
*
*      " Checks
*      IF _check( EXPORTING is_booking     = <s_buffer_booking>
*                           is_bookingx    = <s_bookingx>
*                           iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update
*                 CHANGING  ct_messages    = et_messages ) = abap_false.
*        RETURN.
*      ENDIF.
*
*      " standard determinations
*      DATA(ls_booking) = <s_buffer_booking>." Needed, as key fields must not be changed
*      _determine( EXPORTING iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update
*                  CHANGING  cs_booking     = ls_booking ).
*      <s_buffer_booking>-gr_data = ls_booking-gr_data.
*
*      INSERT <s_buffer_booking> INTO TABLE et_booking.
*    ENDLOOP.
*  ENDMETHOD.
*
*
*  METHOD _delete.
*    CLEAR et_messages.
*
*    CHECK it_booking IS NOT INITIAL.
*
*    " Check for empty keys
*    LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<s_booking_delete>) WHERE booking_id = 0. "#EC CI_SORTSEQ
*      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_no_key  travel_id = <s_booking_delete>-travel_id ) TO et_messages.
*      RETURN.
*    ENDLOOP.
*
*    DATA(lt_booking) = it_booking.
*
*    LOOP AT lt_booking ASSIGNING <s_booking_delete>.
*      " Special case: record already in create buffer
*      READ TABLE mt_create_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id.
*      IF sy-subrc = 0.
*        INSERT VALUE #( travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id ) INTO TABLE mt_delete_buffer_2.
*        DELETE lt_booking.
*      ENDIF.
*    ENDLOOP.
*
*    IF iv_no_delete_check = abap_false.
*      DATA lt_booking_db TYPE /dmo/t_booking_key.
*      SELECT travel_id, booking_id FROM /dmo/booking FOR ALL ENTRIES IN @lt_booking WHERE travel_id = @lt_booking-travel_id AND booking_id = @lt_booking-booking_id INTO CORRESPONDING FIELDS OF TABLE @lt_booking_db.
*    ENDIF.
*
*    " Check existence and append to delete buffer
*    LOOP AT lt_booking ASSIGNING <s_booking_delete>.
*      IF iv_no_delete_check = abap_false.
*        READ TABLE lt_booking_db TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id.
*        IF sy-subrc <> 0.
*          APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id ) TO et_messages.
*          RETURN.
*        ENDIF.
*      ENDIF.
*      INSERT VALUE #( travel_id = <s_booking_delete>-travel_id  booking_id = <s_booking_delete>-booking_id ) INTO TABLE mt_delete_buffer_2.
*    ENDLOOP.
*  ENDMETHOD.
*
*
*  METHOD save.
*    ASSERT mt_create_buffer_2 IS INITIAL.
*    ASSERT mt_update_buffer_2 IS INITIAL.
*    ASSERT mt_delete_buffer_2 IS INITIAL.
*    INSERT /dmo/booking FROM TABLE @mt_create_buffer.
*    UPDATE /dmo/booking FROM TABLE @mt_update_buffer.
*    DELETE /dmo/booking FROM TABLE @( CORRESPONDING #( mt_delete_buffer ) ).
*  ENDMETHOD.
*
*
*  METHOD initialize.
*    CLEAR: mt_create_buffer, mt_update_buffer, mt_delete_buffer.
*  ENDMETHOD.
*
*
*  METHOD check_booking_id." Here we can safely assume that the Travel ID has already been checked!
*    rv_is_valid = abap_false.
*
*    IF iv_booking_id IS INITIAL.
*      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_no_key  travel_id = iv_travel_id ) TO ct_messages.
*      RETURN.
*    ENDIF.
*
*    IF line_exists( mt_delete_buffer[ travel_id = iv_travel_id  booking_id = iv_booking_id ] ).
*      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = iv_travel_id  booking_id = iv_booking_id ) TO ct_messages.
*      RETURN.
*    ENDIF.
*
*    IF line_exists( mt_delete_buffer_2[ travel_id = iv_travel_id  booking_id = iv_booking_id ] ).
*      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = iv_travel_id  booking_id = iv_booking_id ) TO ct_messages.
*      RETURN.
*    ENDIF.
*
*    IF line_exists( mt_create_buffer[ travel_id = iv_travel_id  booking_id = iv_booking_id ] ).
*      rv_is_valid = abap_true.
*      RETURN.
*    ENDIF.
*
*    IF line_exists( mt_create_buffer_2[ travel_id = iv_travel_id  booking_id = iv_booking_id ] ).
*      rv_is_valid = abap_true.
*      RETURN.
*    ENDIF.
*
*    SELECT SINGLE FROM /dmo/booking FIELDS @abap_true WHERE travel_id = @iv_travel_id AND booking_id = @iv_booking_id INTO @DATA(lv_db_exists).
*    IF lv_db_exists = abap_true.
*      rv_is_valid = abap_true.
*      RETURN.
*    ENDIF.
*
*    APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_unknown  travel_id = iv_travel_id  booking_id = iv_booking_id ) TO ct_messages.
*  ENDMETHOD.
*
*
*  METHOD cud_prep.
*    CLEAR et_booking.
*    CLEAR et_messages.
*
*    CHECK it_booking IS NOT INITIAL.
*
*    DATA lt_booking_c  TYPE /dmo/t_booking.
*    DATA lt_booking_u  TYPE /dmo/t_booking.
*    DATA lt_booking_d  TYPE /dmo/t_booking.
*    DATA lt_bookingx_u TYPE /dmo/t_bookingx.
*    LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<s_booking>).
*      READ TABLE it_bookingx ASSIGNING FIELD-SYMBOL(<s_bookingx>) WITH TABLE KEY travel_id = <s_booking>-travel_id  booking_id = <s_booking>-booking_id.
*      IF sy-subrc <> 0.
*        APPEND NEW /dmo/cx_flight_legacy( textid     = /dmo/cx_flight_legacy=>booking_no_control
*                                          travel_id  = <s_booking>-travel_id
*                                          booking_id = <s_booking>-booking_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*      CASE CONV zif_ewm_das_utility=>action_code_enum( <s_bookingx>-action_code ).
*        WHEN zif_ewm_das_utility=>action_code-create.
*          INSERT <s_booking>  INTO TABLE lt_booking_c.
*        WHEN zif_ewm_das_utility=>action_code-update.
*          INSERT <s_booking>  INTO TABLE lt_booking_u.
*          INSERT <s_bookingx> INTO TABLE lt_bookingx_u.
*        WHEN zif_ewm_das_utility=>action_code-delete.
*          INSERT <s_booking>  INTO TABLE lt_booking_d.
*      ENDCASE.
*    ENDLOOP.
*
*    _create( EXPORTING it_booking  = lt_booking_c
*             IMPORTING et_booking  = et_booking
*                       et_messages = et_messages ).
*
*    _update( EXPORTING it_booking  = lt_booking_u
*                       it_bookingx = lt_bookingx_u
*             IMPORTING et_booking  = DATA(lt_booking)
*                       et_messages = DATA(lt_messages) ).
*    INSERT LINES OF lt_booking INTO TABLE et_booking.
*    APPEND LINES OF lt_messages TO et_messages.
*
*    _delete( EXPORTING it_booking         = lt_booking_d
*                       iv_no_delete_check = iv_no_delete_check
*             IMPORTING et_messages        = lt_messages ).
*    APPEND LINES OF lt_messages TO et_messages.
*  ENDMETHOD.
*
*
*  METHOD cud_copy.
*    LOOP AT mt_create_buffer_2 ASSIGNING FIELD-SYMBOL(<s_create_buffer_2>).
*      READ TABLE mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WITH TABLE KEY travel_id = <s_create_buffer_2>-travel_id  booking_id = <s_create_buffer_2>-booking_id.
*      IF sy-subrc <> 0.
*        INSERT VALUE #( travel_id = <s_create_buffer_2>-travel_id  booking_id = <s_create_buffer_2>-booking_id ) INTO TABLE mt_create_buffer ASSIGNING <s_create_buffer>.
*      ENDIF.
*      <s_create_buffer>-gr_data = <s_create_buffer_2>-gr_data.
*    ENDLOOP.
*    LOOP AT mt_update_buffer_2 ASSIGNING FIELD-SYMBOL(<s_update_buffer_2>).
*      READ TABLE mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WITH TABLE KEY travel_id = <s_update_buffer_2>-travel_id  booking_id = <s_update_buffer_2>-booking_id.
*      IF sy-subrc <> 0.
*        INSERT VALUE #( travel_id = <s_update_buffer_2>-travel_id  booking_id = <s_update_buffer_2>-booking_id ) INTO TABLE mt_update_buffer ASSIGNING <s_update_buffer>.
*      ENDIF.
*      <s_update_buffer>-gr_data = <s_update_buffer_2>-gr_data.
*    ENDLOOP.
*    LOOP AT mt_delete_buffer_2 ASSIGNING FIELD-SYMBOL(<s_delete_buffer_2>).
*      DELETE mt_create_buffer WHERE travel_id = <s_delete_buffer_2>-travel_id AND booking_id = <s_delete_buffer_2>-booking_id.
*      IF sy-subrc = 0.
*        CONTINUE.
*      ENDIF.
*      DELETE mt_update_buffer WHERE travel_id = <s_delete_buffer_2>-travel_id AND booking_id = <s_delete_buffer_2>-booking_id.
*      INSERT <s_delete_buffer_2> INTO TABLE mt_delete_buffer.
*    ENDLOOP.
*    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
*  ENDMETHOD.
*
*
*  METHOD cud_disc.
*    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
*  ENDMETHOD.
*
*
*  METHOD get.
*    CLEAR et_booking.
*
*    CHECK it_booking IS NOT INITIAL.
*
*    SELECT * FROM /dmo/booking FOR ALL ENTRIES IN @it_booking WHERE travel_id  = @it_booking-travel_id
*      INTO TABLE @et_booking ##SELECT_FAE_WITH_LOB[DESCRIPTION]. "#EC CI_ALL_FIELDS_NEEDED "#EC CI_SEL_DEL
*
*    IF iv_include_buffer = abap_true.
*      LOOP AT it_booking ASSIGNING FIELD-SYMBOL(<s_booking>).
*        LOOP AT mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WHERE travel_id = <s_booking>-travel_id.
*          INSERT <s_create_buffer> INTO TABLE et_booking.
*        ENDLOOP.
*
*        LOOP AT mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WHERE travel_id = <s_booking>-travel_id.
*          MODIFY TABLE et_booking FROM <s_update_buffer>.
*        ENDLOOP.
*
*        LOOP AT mt_delete_buffer ASSIGNING FIELD-SYMBOL(<s_delete_buffer>) WHERE travel_id = <s_booking>-travel_id.
*          DELETE et_booking WHERE travel_id = <s_delete_buffer>-travel_id AND booking_id = <s_delete_buffer>-booking_id. "#EC CI_SEL_DEL
*        ENDLOOP.
*      ENDLOOP.
*    ENDIF.
*
*    IF iv_include_temp_buffer = abap_true.
*      LOOP AT it_booking ASSIGNING <s_booking>.
*        LOOP AT mt_create_buffer_2 ASSIGNING <s_create_buffer> WHERE travel_id = <s_booking>-travel_id.
*          DELETE et_booking WHERE travel_id = <s_create_buffer>-travel_id AND booking_id = <s_create_buffer>-booking_id. "#EC CI_SEL_DEL
*          INSERT <s_create_buffer> INTO TABLE et_booking.
*        ENDLOOP.
*
*        LOOP AT mt_update_buffer_2 ASSIGNING <s_update_buffer> WHERE travel_id = <s_booking>-travel_id.
*          MODIFY TABLE et_booking FROM <s_update_buffer>.
*        ENDLOOP.
*
*        LOOP AT mt_delete_buffer_2 ASSIGNING <s_delete_buffer> WHERE travel_id = <s_booking>-travel_id.
*          DELETE et_booking WHERE travel_id = <s_delete_buffer>-travel_id AND booking_id = <s_delete_buffer>-booking_id. "#EC CI_SEL_DEL
*        ENDLOOP.
*      ENDLOOP.
*    ENDIF.
*  ENDMETHOD.
*
*
*  METHOD _check.
*    rv_is_valid = abap_true.
*
*    IF NOT _check_booking_date( EXPORTING is_booking     = is_booking
*                                          is_bookingx    = is_bookingx
*                                          iv_change_mode = iv_change_mode
*                                CHANGING  ct_messages    = ct_messages ).
*      rv_is_valid = abap_false.
*    ENDIF.
*
*    IF NOT _check_customer( EXPORTING is_booking     = is_booking
*                                      is_bookingx    = is_bookingx
*                                      iv_change_mode = iv_change_mode
*                            CHANGING  ct_messages    = ct_messages ).
*      rv_is_valid = abap_false.
*    ENDIF.
*
*    IF NOT _check_flight( EXPORTING is_booking     = is_booking
*                                    is_bookingx    = is_bookingx
*                                    iv_change_mode = iv_change_mode
*                          CHANGING  ct_messages    = ct_messages ).
*      rv_is_valid = abap_false.
*    ENDIF.
*
*    IF NOT _check_currency_code( EXPORTING is_booking     = is_booking
*                                           is_bookingx    = is_bookingx
*                                           iv_change_mode = iv_change_mode
*                                 CHANGING  ct_messages    = ct_messages ).
*      rv_is_valid = abap_false.
*    ENDIF.
*  ENDMETHOD.
*
*
*  METHOD _check_booking_date.
*    rv_is_valid = abap_true.
*    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_bookingx-booking_date = abap_true ).
*
*      " A. Booking Date must not be initial
*      " B. When the record is created it must not be in the past
*      IF is_booking-booking_date IS INITIAL OR is_booking-booking_date = '' OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create AND is_booking-booking_date < cl_abap_context_info=>get_system_date( ) ).
*        rv_is_valid = abap_false.
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>booking_booking_date_invalid  travel_id = is_booking-travel_id  booking_id = is_booking-booking_id  booking_date = is_booking-booking_date ) TO ct_messages.
*      ENDIF.
*    ENDIF.
*  ENDMETHOD.
*
*
*  METHOD _check_customer.
*    rv_is_valid = abap_true.
*    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_bookingx-customer_id = abap_true ).
*      rv_is_valid = lcl_common_checks=>is_customer_id_valid( EXPORTING iv_customer_id = is_booking-customer_id CHANGING ct_messages = ct_messages ).
*    ENDIF.
*  ENDMETHOD.
*
*
*  METHOD _check_flight.
*    rv_is_valid = abap_true.
*    IF     iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
*      OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update
*        AND ( is_bookingx-carrier_id = abap_true OR is_bookingx-connection_id = abap_true OR is_bookingx-flight_date = abap_true ) ).
*      IF mt_flight_key IS INITIAL.
*        SELECT carrier_id, connection_id, flight_date FROM /dmo/flight INTO CORRESPONDING FIELDS OF TABLE @mt_flight_key. "#EC CI_NOWHERE
*      ENDIF.
*      READ TABLE mt_flight_key TRANSPORTING NO FIELDS WITH TABLE KEY carrier_id = is_booking-carrier_id  connection_id = is_booking-connection_id  flight_date = is_booking-flight_date.
*      IF sy-subrc <> 0.
*        rv_is_valid = abap_false.
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>flight_unknown  carrier_id = is_booking-carrier_id  connection_id = is_booking-connection_id  flight_date = is_booking-flight_date ) TO ct_messages.
*      ENDIF.
*    ENDIF.
*  ENDMETHOD.
*
*
*  METHOD _check_currency_code.
*    rv_is_valid = abap_true.
*    IF   ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create AND is_booking-currency_code IS NOT INITIAL ) " Will be derived if initial
*      OR ( iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update AND is_bookingx-currency_code = abap_true ).
*      rv_is_valid = lcl_common_checks=>is_currency_code_valid( EXPORTING iv_currency_code = is_booking-currency_code CHANGING ct_messages = ct_messages ).
*    ENDIF.
*  ENDMETHOD.
*
*
*  METHOD _determine.
*    IF iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create.
*      " Derive price and currency code if one of the fields is initial
*      IF cs_booking-flight_price IS INITIAL OR cs_booking-currency_code IS INITIAL.
*        " Flight price might have changed, we need to use current flight price
*        SELECT SINGLE price, currency_code FROM /dmo/flight WHERE carrier_id    = @cs_booking-carrier_id
*                                                              AND connection_id = @cs_booking-connection_id
*                                                              AND flight_date   = @cs_booking-flight_date INTO ( @cs_booking-flight_price, @cs_booking-currency_code ).
*        ASSERT sy-subrc = 0. " Check has been done before
*      ENDIF.
*    ENDIF.
*  ENDMETHOD.
ENDCLASS.


CLASS lcl_app_buffer DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_app_buffer.
    METHODS save.
    METHODS initialize.
    METHODS check_travel_id IMPORTING iv_app_id          TYPE /scwm/dsappid
                            CHANGING  ct_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message
                            RETURNING VALUE(rv_is_valid) TYPE abap_bool.
    "! Prepare changes in a temporary buffer
    "! @parameter iv_no_delete_check | In some cases we do not need to check the existence of a record to be deleted, as this check has been done before.
    "!                               | E.g. delete all subnodes of a node to be deleted.  In this case we have read the subnodes to get their keys.
    METHODS cud_prep IMPORTING it_app             TYPE zewm_tt_d_dsapp
                               it_appx            TYPE zewm_tt_x_d_dsapp
                               iv_no_delete_check TYPE abap_bool OPTIONAL
                     EXPORTING et_app             TYPE zewm_tt_d_dsapp
                               et_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message.
    "! Add content of the temporary buffer to the real buffer and clear the temporary buffer
    METHODS cud_copy.
    "! Discard content of the temporary buffer
    METHODS cud_disc.
    METHODS get IMPORTING it_travel              TYPE zewm_tt_d_dsapp
                          iv_include_buffer      TYPE abap_boolean
                          iv_include_temp_buffer TYPE abap_boolean
                EXPORTING et_travel              TYPE zewm_tt_d_dsapp.

  PRIVATE SECTION.
    CLASS-DATA go_instance TYPE REF TO lcl_app_buffer.
    " Main buffer
    DATA: mt_create_buffer TYPE zewm_tt_d_dsapp,
          mt_update_buffer TYPE zewm_tt_d_dsapp.
*          mt_delete_buffer TYPE /dmo/t_travel_key.
    " Temporary buffer valid during create / update / delete Travel
    DATA: mt_create_buffer_2 TYPE zewm_tt_d_dsapp,
          mt_update_buffer_2 TYPE zewm_tt_d_dsapp.
*          mt_delete_buffer_2 TYPE /dmo/t_travel_key.

    METHODS _create IMPORTING it_app      TYPE zewm_tt_d_dsapp
                    EXPORTING et_app      TYPE zewm_tt_d_dsapp
                              et_messages TYPE zif_ewm_das_utility=>tt_if_t100_message.
    METHODS _update IMPORTING it_app      TYPE zewm_tt_d_dsapp
                              it_appx     TYPE zewm_tt_x_d_dsapp
                    EXPORTING et_app      TYPE zewm_tt_d_dsapp
                              et_messages TYPE zif_ewm_das_utility=>tt_if_t100_message.
    METHODS _delete IMPORTING it_app          TYPE zewm_tt_d_dsapp
                              iv_no_delete_check TYPE abap_bool
                    EXPORTING et_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message.

    METHODS _check IMPORTING is_travel          TYPE /scwm/d_dsapp
                             is_travelx         TYPE zewm_s_x_d_dsapp OPTIONAL
                             iv_change_mode     TYPE zcl_ewm_das_uitility=>ty_change_mode
                   CHANGING  ct_messages        TYPE zif_ewm_das_utility=>tt_if_t100_message
                   RETURNING VALUE(rv_is_valid) TYPE abap_bool.


ENDCLASS.


CLASS lcl_app_buffer IMPLEMENTATION.
  METHOD get_instance.
    go_instance = COND #( WHEN go_instance IS BOUND THEN go_instance ELSE NEW #( ) ).
    ro_instance = go_instance.
  ENDMETHOD.


  METHOD _create.

    DATA:
      lo_srv              TYPE REF TO /bobf/if_tra_service_manager,
      lt_failed_key       TYPE /bobf/t_frw_key,
      lo_message          TYPE REF TO /bobf/if_frw_message,
      lo_change           TYPE REF TO /bobf/if_tra_change,
      lv_rejected         TYPE abap_bool,
      lt_rejecting_bo_key TYPE /bobf/t_frw_key2,
      ls_modification     TYPE /bobf/s_frw_modification,
      lt_modification     TYPE /bobf/t_frw_modification.

    DATA lr_data TYPE REF TO data.
    DATA ls_root TYPE /scwm/s_dsapp_root_k.
    CLEAR et_app.
    CLEAR et_messages.
*
    CHECK it_app IS NOT INITIAL.
*
*    DATA lv_travel_id_max TYPE /dmo/travel_id.
*    IF lcl_app_buffer=>get_instance( )->mt_create_buffer IS INITIAL.
*      SELECT FROM /dmo/travel FIELDS MAX( travel_id ) INTO @lv_travel_id_max. "#EC CI_NOWHERE
*    ELSE.
*      LOOP AT mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_buffer_travel_create>).
*        IF <s_buffer_travel_create>-travel_id > lv_travel_id_max.
*          lv_travel_id_max = <s_buffer_travel_create>-travel_id.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
    LOOP AT it_app ASSIGNING FIELD-SYMBOL(<ls_app_create>).
*
*      " Checks
*      IF _check( EXPORTING is_travel     = ls_travel_create
*                           iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
*                 CHANGING  ct_messages   = et_messages ) = abap_false.
*        RETURN.
*      ENDIF.

      lo_srv = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scwm/if_dsapp_c=>sc_bo_key ).
      ls_root = CORRESPONDING #( <ls_app_create> MAPPING key = db_key ).
      ls_root-key = /bobf/cl_frw_factory=>get_new_key( ).

      CREATE DATA lr_data LIKE ls_root.
      ASSIGN lr_data->* TO FIELD-SYMBOL(<ls_data>).
      <ls_data> = ls_root.
      ls_modification-data = lr_data.
      ls_modification-node = /scwm/if_dsapp_c=>sc_node-root.
      ls_modification-key = ls_root-key.
      ls_modification-change_mode = 'C'.

      APPEND ls_modification TO lt_modification.

      lo_srv->modify(
        EXPORTING
          it_modification = lt_modification
        IMPORTING
          eo_change       = lo_change
          eo_message      = lo_message
      ).
*      CATCH /bobf/cx_frw_contrct_violation.
      lo_message->get_messages( IMPORTING et_message = DATA(lt_message)  ).

      LOOP AT lt_message ASSIGNING FIELD-SYMBOL(<ls_bopf_msg>).
*        APPEND NEW zcx_das_exceptions( textid = <ls_bopf_msg>-message->if_t100_message~t100key ) TO et_messages.
        APPEND NEW /bobf/cl_behv_message( <ls_bopf_msg>-message ) TO et_messages.
      ENDLOOP.
*
*      " standard determinations
*      ls_travel_create-createdby = sy-uname.
*      GET TIME STAMP FIELD ls_travel_create-createdat.
*      ls_travel_create-lastchangedby = ls_travel_create-createdby.
*      ls_travel_create-lastchangedat = ls_travel_create-createdat.
*      ls_travel_create-status = zif_ewm_das_utility=>travel_status-new.
*
*      " **Internal** numbering: Override travel_id
*      lv_travel_id_max = lv_travel_id_max + 1.
*      ASSERT lv_travel_id_max IS NOT INITIAL.
*      ls_travel_create-travel_id = lv_travel_id_max.
*
*      INSERT ls_travel_create INTO TABLE mt_create_buffer_2.
    ENDLOOP.
*
*    et_travel = mt_create_buffer_2.


  ENDMETHOD.


  METHOD _update.

    DATA:
      lo_srv              TYPE REF TO /bobf/if_tra_service_manager,
      lt_failed_key       TYPE /bobf/t_frw_key,
      lo_message          TYPE REF TO /bobf/if_frw_message,
      lo_change           TYPE REF TO /bobf/if_tra_change,
      lv_rejected         TYPE abap_bool,
      lt_rejecting_bo_key TYPE /bobf/t_frw_key2,
      ls_modification     TYPE /bobf/s_frw_modification,
      lt_modification     TYPE /bobf/t_frw_modification.

    DATA lr_data TYPE REF TO data.
    DATA ls_root TYPE /scwm/s_dsapp_root_k.
    FIELD-SYMBOLS:
      <ls_mod>  LIKE LINE OF lt_modification.

    DATA lv_new TYPE abap_bool.

    CLEAR et_app.
    CLEAR et_messages.
*
    CHECK it_app IS NOT INITIAL.
*
*    " Check for empty keys
*    READ TABLE it_travel TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = '0'.
*    IF sy-subrc = 0.
*      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
*      RETURN.
*    ENDIF.
*
    DATA lt_app TYPE SORTED TABLE OF /scwm/d_dsapp WITH UNIQUE KEY db_key.
    SELECT * FROM /scwm/d_dsapp
     FOR ALL ENTRIES IN @it_app
     WHERE db_key = @it_app-db_key
     INTO TABLE @lt_app ##SELECT_FAE_WITH_LOB[DESCRIPTION].
*
    FIELD-SYMBOLS <s_buffer_app> TYPE /scwm/d_dsapp.
    DATA ls_buffer_app TYPE /scwm/d_dsapp.
    LOOP AT it_app ASSIGNING FIELD-SYMBOL(<s_app_update>).
      UNASSIGN <s_buffer_app>.
*
*      READ TABLE mt_delete_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
*      IF sy-subrc = 0." Error: Record to be updated marked for deletion
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = <s_travel_update>-travel_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
*      IF <s_buffer_travel> IS NOT ASSIGNED." Special case: record already in temporary create buffer
*        READ TABLE mt_create_buffer_2 ASSIGNING <s_buffer_travel> WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
*        IF sy-subrc = 0.
*          lv_new = abap_true.
*        ENDIF.
*      ENDIF.
*
*      IF <s_buffer_travel> IS NOT ASSIGNED." Special case: record already in create buffer
*        lv_new = abap_false.
*        READ TABLE mt_create_buffer INTO ls_buffer_travel WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
*        IF sy-subrc = 0.
*          INSERT ls_buffer_travel INTO TABLE mt_create_buffer_2 ASSIGNING <s_buffer_travel>.
*          lv_new = abap_true.
*        ENDIF.
*      ENDIF.
*
*      IF <s_buffer_travel> IS NOT ASSIGNED." Special case: record already in temporary update buffer
*        READ TABLE mt_update_buffer_2 ASSIGNING <s_buffer_travel> WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
*      ENDIF.
*
*      IF <s_buffer_travel> IS NOT ASSIGNED." Special case: record already in update buffer
*        READ TABLE mt_update_buffer INTO ls_buffer_travel WITH TABLE KEY travel_id = <s_travel_update>-travel_id.
*        IF sy-subrc = 0.
*          INSERT ls_buffer_travel INTO TABLE mt_update_buffer_2 ASSIGNING <s_buffer_travel>.
*        ENDIF.
*      ENDIF.
*
      IF <s_buffer_app> IS NOT ASSIGNED." Usual case: record not already in update buffer
        READ TABLE lt_app ASSIGNING FIELD-SYMBOL(<s_app_old>)
        WITH TABLE KEY db_key = <s_app_update>-db_key.
        IF sy-subrc = 0.
          INSERT <s_app_old> INTO TABLE mt_update_buffer_2 ASSIGNING FIELD-SYMBOL(<s_buffer_travel>).
          ASSERT sy-subrc = 0.
        ENDIF.
      ENDIF.
*
*      " Error
*      IF <s_buffer_travel> IS NOT ASSIGNED.
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = <s_travel_update>-travel_id ) TO et_messages.
*        RETURN.
*      ENDIF.
*
      " Merge fields to be updated
      READ TABLE it_appx ASSIGNING FIELD-SYMBOL(<s_appx>)
      WITH KEY dbkey = <s_app_update>-db_key  action_code = zif_ewm_das_utility=>action_code-update.
      IF sy-subrc <> 0.
*        APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_control  travel_id = <s_travel_update>-travel_id ) TO et_messages.
        RETURN.
      ENDIF.
      DATA lv_field TYPE i.
      lv_field = 3.
      DO.
        ASSIGN COMPONENT lv_field OF STRUCTURE <s_appx> TO FIELD-SYMBOL(<v_flag>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF <v_flag> = abap_true.

          ASSIGN COMPONENT lv_field OF STRUCTURE <s_app_update> TO FIELD-SYMBOL(<v_field_new>).
          ASSERT sy-subrc = 0.


*          DATA: lo_type_descriptor TYPE REF TO cl_abap_typedescr.
          APPEND INITIAL LINE TO ls_modification-changed_fields ASSIGNING FIELD-SYMBOL(<ls_chng_field>).
*          lo_type_descriptor ?= cl_abap_datadescr=>describe_by_data( <v_field_new> ).
*          DATA(lv_fieldname) = lo_type_descriptor->get_relative_name( ).
          DATA: gr_structdescr TYPE REF TO cl_abap_structdescr.
          DATA gt_components TYPE abap_component_tab.
          gr_structdescr ?= cl_abap_structdescr=>describe_by_data( <s_appx> ).
          gt_components = gr_structdescr->get_components( ).
          ASSIGN gt_components[ lv_field ] TO FIELD-SYMBOL(<ls_field>).
          IF sy-subrc = 0.
            <ls_chng_field> = <ls_field>-name.
          ENDIF.

          ASSIGN COMPONENT lv_field OF STRUCTURE <s_buffer_travel> TO FIELD-SYMBOL(<v_field_old>).
          ASSERT sy-subrc = 0.
          <v_field_old> = <v_field_new>.
        ENDIF.
        lv_field = lv_field + 1.
      ENDDO.

*      " Checks
*      IF _check( EXPORTING is_travel      = <s_buffer_travel>
*                           is_travelx     = <s_travelx>
*                           iv_change_mode = /dmo/cl_flight_legacy=>change_mode-update
*                 CHANGING  ct_messages    = et_messages ) = abap_false.
*        RETURN.
*      ENDIF.
*
*      " Set administrative fields
*      _update_admin( EXPORTING iv_new = lv_new CHANGING cs_travel_admin = <s_buffer_travel>-gr_admin ).
*
*      " standard determinations
*
*      INSERT <s_buffer_travel> INTO TABLE et_travel.

      lo_srv = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scwm/if_dsapp_c=>sc_bo_key ).
      ls_modification-node = /scwm/if_dsapp_c=>sc_node-root.
      ls_modification-key = <s_app_update>-db_key.
      ls_modification-change_mode = 'U'.

      ls_root = CORRESPONDING #( <s_app_update> MAPPING key = db_key ).
      CREATE DATA lr_data LIKE ls_root.
      ASSIGN lr_data->* TO FIELD-SYMBOL(<ls_data>).
      <ls_data> = ls_root.
      ls_modification-data = lr_data.

      APPEND ls_modification TO lt_modification.

      lo_srv->modify(
        EXPORTING
          it_modification = lt_modification
        IMPORTING
          eo_change       = lo_change
          eo_message      = lo_message
      ).
*      CATCH /bobf/cx_frw_contrct_violation.
      lo_message->get_messages( IMPORTING et_message = DATA(lt_message)  ).

      LOOP AT lt_message ASSIGNING FIELD-SYMBOL(<ls_bopf_msg>).
*        APPEND NEW zcx_das_exceptions( textid = <ls_bopf_msg>-message->if_t100_message~t100key ) TO et_messages.
        APPEND NEW /bobf/cl_behv_message( <ls_bopf_msg>-message ) TO et_messages.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD _delete.

   DATA:
      lo_srv              TYPE REF TO /bobf/if_tra_service_manager,
      lt_failed_key       TYPE /bobf/t_frw_key,
      lo_message          TYPE REF TO /bobf/if_frw_message,
      lo_change           TYPE REF TO /bobf/if_tra_change,
      lv_rejected         TYPE abap_bool,
      lt_rejecting_bo_key TYPE /bobf/t_frw_key2,
      ls_modification     TYPE /bobf/s_frw_modification,
      lt_modification     TYPE /bobf/t_frw_modification.

    DATA lr_data TYPE REF TO data.
    DATA ls_root TYPE /scwm/s_dsapp_root_k.

    CLEAR et_messages.
*
    CHECK it_app IS NOT INITIAL.

    SELECT db_key FROM /scwm/d_dsapp
     FOR ALL ENTRIES IN @it_app
      WHERE db_key = @it_app-db_key INTO TABLE @DATA(lt_app_db_key).

      LOOP AT lt_app_db_key ASSIGNING FIELD-SYMBOL(<ls_app_delete>).
*
*      " Checks
*      IF _check( EXPORTING is_travel     = ls_travel_create
*                           iv_change_mode = /dmo/cl_flight_legacy=>change_mode-create
*                 CHANGING  ct_messages   = et_messages ) = abap_false.
*        RETURN.
*      ENDIF.

      lo_srv = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scwm/if_dsapp_c=>sc_bo_key ).
      ls_root = CORRESPONDING #( <ls_app_delete> MAPPING key = db_key ).

      CREATE DATA lr_data LIKE ls_root.
      ASSIGN lr_data->* TO FIELD-SYMBOL(<ls_data>).
      <ls_data> = ls_root.
      ls_modification-data = lr_data.
      ls_modification-node = /scwm/if_dsapp_c=>sc_node-root.
      ls_modification-key = ls_root-key.
      ls_modification-change_mode = 'D'.

      APPEND ls_modification TO lt_modification.

      lo_srv->modify(
        EXPORTING
          it_modification = lt_modification
        IMPORTING
          eo_change       = lo_change
          eo_message      = lo_message
      ).
*      CATCH /bobf/cx_frw_contrct_violation.
      lo_message->get_messages( IMPORTING et_message = DATA(lt_message)  ).

      LOOP AT lt_message ASSIGNING FIELD-SYMBOL(<ls_bopf_msg>).
*        APPEND NEW zcx_das_exceptions( textid = <ls_bopf_msg>-message->if_t100_message~t100key ) TO et_messages.
        APPEND NEW /bobf/cl_behv_message( <ls_bopf_msg>-message ) TO et_messages.
      ENDLOOP.
      endloop.
*
*    " Check for empty keys
*    READ TABLE it_travel TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = '0'.
*    IF sy-subrc = 0.
*      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO et_messages.
*      RETURN.
*    ENDIF.
*
*    DATA(lt_travel) = it_travel.
*
*    " Special case: record already in create buffer
*    LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<s_travel_delete>).
*      READ TABLE mt_create_buffer TRANSPORTING NO FIELDS WITH KEY travel_id = <s_travel_delete>-travel_id.
*      IF sy-subrc = 0.
*        INSERT VALUE #( travel_id = <s_travel_delete>-travel_id ) INTO TABLE mt_delete_buffer_2.
*        DELETE lt_travel.
*      ENDIF.
*    ENDLOOP.
*
*    IF iv_no_delete_check = abap_false.
*      DATA lt_travel_db TYPE SORTED TABLE OF /dmo/travel_id WITH UNIQUE KEY table_line.
*      SELECT travel_id FROM /dmo/travel FOR ALL ENTRIES IN @lt_travel WHERE travel_id = @lt_travel-travel_id INTO TABLE @lt_travel_db.
*    ENDIF.
*
*    " Check existence and append to delete buffer
*    LOOP AT lt_travel ASSIGNING <s_travel_delete>.
*      IF iv_no_delete_check = abap_false.
*        READ TABLE lt_travel_db ASSIGNING FIELD-SYMBOL(<s_travel_old>) WITH TABLE KEY table_line = <s_travel_delete>-travel_id.
*        IF sy-subrc <> 0.
*          APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = <s_travel_delete>-travel_id ) TO et_messages.
*          RETURN.
*        ENDIF.
*      ENDIF.
*      INSERT VALUE #( travel_id = <s_travel_delete>-travel_id ) INTO TABLE mt_delete_buffer_2.
*    ENDLOOP.
  ENDMETHOD.


  METHOD save.
    ASSERT mt_create_buffer_2 IS INITIAL.
    ASSERT mt_update_buffer_2 IS INITIAL.

  ENDMETHOD.


  METHOD initialize.
    CLEAR: mt_create_buffer, mt_update_buffer.
  ENDMETHOD.


  METHOD check_travel_id.
*    rv_is_valid = abap_false.
*
*    IF iv_travel_id IS INITIAL.
*      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_no_key ) TO ct_messages.
*      RETURN.
*    ENDIF.
*
*    IF line_exists( mt_delete_buffer[ travel_id = iv_travel_id ] ).
*      APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = iv_travel_id ) TO ct_messages.
*      RETURN.
*    ENDIF.
*
*    IF line_exists( mt_create_buffer[ travel_id = iv_travel_id ] ).
*      rv_is_valid = abap_true.
*      RETURN.
*    ENDIF.
*
*    SELECT SINGLE FROM /dmo/travel FIELDS @abap_true WHERE travel_id = @iv_travel_id INTO @DATA(lv_db_exists).
*    IF lv_db_exists = abap_true.
*      rv_is_valid = abap_true.
*      RETURN.
*    ENDIF.
*
*    APPEND NEW /dmo/cx_flight_legacy( textid = /dmo/cx_flight_legacy=>travel_unknown  travel_id = iv_travel_id ) TO ct_messages.
  ENDMETHOD.


  METHOD cud_prep.
    CLEAR et_app.
    CLEAR et_messages.
*
    CHECK it_app IS NOT INITIAL.
*
    DATA lt_app_c  TYPE zewm_tt_d_dsapp.
    DATA lt_app_u  TYPE zewm_tt_d_dsapp.
    DATA lt_app_d  TYPE zewm_tt_d_dsapp.
    DATA lt_appx_u TYPE zewm_tt_x_d_dsapp.
    LOOP AT it_app ASSIGNING FIELD-SYMBOL(<s_app>).
      READ TABLE it_appx ASSIGNING FIELD-SYMBOL(<s_appx>) WITH TABLE KEY dbkey = <s_app>-db_key.
      IF sy-subrc <> 0.
        APPEND NEW zcx_das_exceptions( textid     = zcx_das_exceptions=>appointment_no_key
                                       app_id  = <s_app>-docno ) TO et_messages.
        RETURN.
      ENDIF.
      CASE CONV zif_ewm_das_utility=>action_code_enum( <s_appx>-action_code ).
        WHEN zif_ewm_das_utility=>action_code-create.
          INSERT <s_app>  INTO TABLE lt_app_c.
        WHEN zif_ewm_das_utility=>action_code-update.
          INSERT <s_app>  INTO TABLE lt_app_u.
          INSERT <s_appx> INTO TABLE lt_appx_u.
        WHEN zif_ewm_das_utility=>action_code-delete.
          INSERT <s_app>  INTO TABLE lt_app_d.
      ENDCASE.
    ENDLOOP.

    _create( EXPORTING it_app   = lt_app_c
             IMPORTING et_app   = et_app
                       et_messages = et_messages ).

    _update( EXPORTING it_app   = lt_app_u
                       it_appx  = lt_appx_u
             IMPORTING et_app   = DATA(lt_app)
                       et_messages = DATA(lt_messages) ).
    INSERT LINES OF lt_app INTO TABLE et_app.
    APPEND LINES OF lt_messages TO et_messages.
*
    _delete( EXPORTING it_app          = lt_app_d
                       iv_no_delete_check = iv_no_delete_check
             IMPORTING et_messages        = lt_messages ).
    APPEND LINES OF lt_messages TO et_messages.
  ENDMETHOD.


  METHOD cud_copy.
*    LOOP AT mt_create_buffer_2 ASSIGNING FIELD-SYMBOL(<s_create_buffer_2>).
*      READ TABLE mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WITH TABLE KEY travel_id = <s_create_buffer_2>-travel_id.
*      IF sy-subrc <> 0.
*        INSERT VALUE #( travel_id = <s_create_buffer_2>-travel_id ) INTO TABLE mt_create_buffer ASSIGNING <s_create_buffer>.
*      ENDIF.
*      <s_create_buffer>-gr_data  = <s_create_buffer_2>-gr_data.
*      <s_create_buffer>-gr_admin = <s_create_buffer_2>-gr_admin.
*    ENDLOOP.
*    LOOP AT mt_update_buffer_2 ASSIGNING FIELD-SYMBOL(<s_update_buffer_2>).
*      READ TABLE mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WITH TABLE KEY travel_id = <s_update_buffer_2>-travel_id.
*      IF sy-subrc <> 0.
*        INSERT VALUE #( travel_id = <s_update_buffer_2>-travel_id ) INTO TABLE mt_update_buffer ASSIGNING <s_update_buffer>.
*      ENDIF.
*      <s_update_buffer>-gr_data  = <s_update_buffer_2>-gr_data.
*      <s_update_buffer>-gr_admin = <s_update_buffer_2>-gr_admin.
*    ENDLOOP.
*    LOOP AT mt_delete_buffer_2 ASSIGNING FIELD-SYMBOL(<s_delete_buffer_2>).
*      DELETE mt_create_buffer WHERE travel_id = <s_delete_buffer_2>-travel_id.
*      IF sy-subrc = 0.
*        CONTINUE.
*      ENDIF.
*      DELETE mt_update_buffer WHERE travel_id = <s_delete_buffer_2>-travel_id.
*      INSERT <s_delete_buffer_2> INTO TABLE mt_delete_buffer.
*    ENDLOOP.
*    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
  ENDMETHOD.
*
*
  METHOD cud_disc.
*    CLEAR: mt_create_buffer_2, mt_update_buffer_2, mt_delete_buffer_2.
  ENDMETHOD.


  METHOD get.
*    CLEAR et_travel.
*
*    CHECK it_travel IS NOT INITIAL.
*
*    SELECT * FROM /dmo/travel FOR ALL ENTRIES IN @it_travel WHERE travel_id = @it_travel-travel_id
*      INTO TABLE @et_travel ##SELECT_FAE_WITH_LOB[DESCRIPTION]. "#EC CI_ALL_FIELDS_NEEDED "#EC CI_SEL_DEL
*
*    IF iv_include_buffer = abap_true.
*      LOOP AT it_travel ASSIGNING FIELD-SYMBOL(<s_travel>).
*        READ TABLE mt_create_buffer ASSIGNING FIELD-SYMBOL(<s_create_buffer>) WITH TABLE KEY travel_id = <s_travel>-travel_id.
*        IF sy-subrc = 0.
*          INSERT <s_create_buffer> INTO TABLE et_travel.
*        ENDIF.
*
*        READ TABLE mt_update_buffer ASSIGNING FIELD-SYMBOL(<s_update_buffer>) WITH TABLE KEY travel_id = <s_travel>-travel_id.
*        IF sy-subrc = 0.
*          MODIFY TABLE et_travel FROM <s_update_buffer>.
*        ENDIF.
*
*        READ TABLE mt_delete_buffer TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_travel>-travel_id.
*        IF sy-subrc = 0.
*          DELETE et_travel WHERE travel_id = <s_travel>-travel_id. "#EC CI_SEL_DEL
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*
*    IF iv_include_temp_buffer = abap_true.
*      LOOP AT it_travel ASSIGNING <s_travel>.
*        READ TABLE mt_create_buffer_2 ASSIGNING <s_create_buffer> WITH TABLE KEY travel_id = <s_travel>-travel_id.
*        IF sy-subrc = 0.
*          DELETE et_travel WHERE travel_id = <s_travel>-travel_id. "#EC CI_SEL_DEL
*          INSERT <s_create_buffer> INTO TABLE et_travel.
*        ENDIF.
*
*        READ TABLE mt_update_buffer_2 ASSIGNING <s_update_buffer> WITH TABLE KEY travel_id = <s_travel>-travel_id.
*        IF sy-subrc = 0.
*          MODIFY TABLE et_travel FROM <s_update_buffer>.
*        ENDIF.
*
*        READ TABLE mt_delete_buffer_2 TRANSPORTING NO FIELDS WITH TABLE KEY travel_id = <s_travel>-travel_id.
*        IF sy-subrc = 0.
*          DELETE et_travel WHERE travel_id = <s_travel>-travel_id. "#EC CI_SEL_DEL
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
  ENDMETHOD.


  METHOD _check.
*    rv_is_valid = abap_true.
*
*    IF NOT _check_agency( EXPORTING is_travel        = is_travel
*                                    is_travelx       = is_travelx
*                                    iv_change_mode   = iv_change_mode
*                          CHANGING  ct_messages      = ct_messages ).
*      rv_is_valid = abap_false.
*    ENDIF.
*
*    IF NOT _check_customer( EXPORTING is_travel      = is_travel
*                                      is_travelx     = is_travelx
*                                      iv_change_mode = iv_change_mode
*                            CHANGING  ct_messages    = ct_messages ).
*      rv_is_valid = abap_false.
*    ENDIF.
*
*    IF NOT _check_dates( EXPORTING is_travel      = is_travel
*                                   is_travelx     = is_travelx
*                                   iv_change_mode = iv_change_mode
*                         CHANGING  ct_messages    = ct_messages ).
*      rv_is_valid = abap_false.
*    ENDIF.
*
*    IF NOT _check_status( EXPORTING is_travel      = is_travel
*                                    is_travelx     = is_travelx
*                                    iv_change_mode = iv_change_mode
*                          CHANGING  ct_messages    = ct_messages ).
*      rv_is_valid = abap_false.
*    ENDIF.
*
*    IF NOT _check_currency_code( EXPORTING is_travel      = is_travel
*                                           is_travelx     = is_travelx
*                                           iv_change_mode = iv_change_mode
*                                 CHANGING  ct_messages    = ct_messages ).
*      rv_is_valid = abap_false.
*    ENDIF.
  ENDMETHOD.


ENDCLASS.
