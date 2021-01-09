CLASS lhc_AssignedSlot DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE AssignedSlot.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE AssignedSlot.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE AssignedSlot.

    METHODS read FOR READ
      IMPORTING keys FOR READ AssignedSlot RESULT result.

ENDCLASS.

CLASS lhc_AssignedSlot IMPLEMENTATION.

  METHOD create.
  ENDMETHOD.

  METHOD delete.
  ENDMETHOD.

  METHOD update.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

ENDCLASS.
