CLASS zcx_das_exceptions DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS:
      gc_msgid TYPE symsgid VALUE 'ZEWM_DAS',

      BEGIN OF appointment_no_key,
        msgid TYPE symsgid VALUE 'ZEWM_DAS',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'mv_app_id',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF appointment_no_key.

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        app_id   TYPE /scwm/dsappid OPTIONAL.

    DATA: mv_app_id  TYPE /scwm/dsappid.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_das_exceptions IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    me->mv_app_id             = app_id.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
