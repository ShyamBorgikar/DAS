FUNCTION zewm_bopf_transaction_mgr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  DATA: lo_message          TYPE REF TO /bobf/if_frw_message,
        lo_change           TYPE REF TO /bobf/if_tra_change,
        lo_tra              TYPE REF TO /bobf/if_tra_transaction_mgr,
        lo_tra_slave        TYPE REF TO /bobf/if_tra_slave_trans_mgr,
        lv_rejected         TYPE abap_bool,
        lt_rejecting_bo_key TYPE /bobf/t_frw_key2.

*  lo_tra = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
  lo_tra_slave = /bobf/cl_tra_trans_mgr_factory=>get_slave_transaction_manager( ).
  lo_tra_slave->finalize(
    IMPORTING
      eo_message          = lo_message
      eo_change           = lo_change
      et_rejecting_bo_key = lt_rejecting_bo_key
      ev_rejected         = lv_rejected
  ).
*  CATCH /bobf/cx_frw_contrct_violation.
  lo_tra_slave->check_before_save(
*   EXPORTING
*     iv_abort_first_rejected = abap_true
*   IMPORTING
*     eo_message              =
*     et_rejecting_bo_key     =
*     ev_rejected             =
*     et_failed_bo_node       =
  ).
* CATCH /bobf/cx_frw_contrct_violation.

  lo_tra_slave->adjust_numbers(
    IMPORTING
      eo_change  = lo_change
*     eo_message =
  ).
* CATCH /bobf/cx_frw_contrct_violation.
  lo_tra_slave->on_numbers_adjusted(
    EXPORTING
      io_change  = lo_change
*   IMPORTING
*     eo_message =
*     eo_change  =
  ).
* CATCH /bobf/cx_frw_contrct_violation.

  lo_tra_slave->do_save(
    IMPORTING
      eo_message          = lo_message
      et_rejecting_bo_key = lt_rejecting_bo_key
      ev_rejected         = lv_rejected
  ).
*CATCH /bobf/cx_frw_contrct_violation.
*  lo_tra->save(
**      EXPORTING
**      iv_transaction_pattern = /bobf/if_tra_c=>gc_tp_save_and_continue
*      IMPORTING
*        ev_rejected = lv_rejected
*        eo_change = lo_change
*        eo_message = lo_message
*        et_rejecting_bo_key = lt_rejecting_bo_key
*      ).

ENDFUNCTION.
