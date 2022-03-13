*----------------------------------------------------------------------*
***INCLUDE LZFILE_UTILSO03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9200 OUTPUT.
  SET PF-STATUS '9000_STD'.
* SET TITLEBAR 'xxx'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module INIT_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_9200 OUTPUT.
  TRY.
      IF go_screen_state->mv_grid_inital_rendered EQ abap_false.
        go_screen_objects_col->build_alv_tree( ).
        go_screen_state->set_grid_rendered( abap_true ).
        go_screen_objects_col->reset_text_edit( ).
        go_dd_handler->set_values( ).
*        CATCH lcx_vrm_value.
      ENDIF.
    CATCH lcx_screen_error lcx_vrm_value INTO go_err.
      MESSAGE go_err->get_text( ) TYPE 'E' DISPLAY LIKE 'I'.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module MOD_SCREEN_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_screen_9200 OUTPUT.
  LOOP AT SCREEN INTO DATA(wa_screen).
    IF wa_screen-group1 = 'A0'.
      wa_screen-input = 0.
      MODIFY SCREEN FROM wa_screen.
    ENDIF.
  ENDLOOP.
ENDMODULE.
