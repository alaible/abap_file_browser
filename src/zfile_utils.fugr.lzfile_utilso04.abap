*----------------------------------------------------------------------*
***INCLUDE LZFILE_UTILSO04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module INIT_9300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_9300 OUTPUT.
  TRY.
      IF go_screen_state->mv_grid_inital_rendered EQ abap_false.
        go_screen_objects_simple->build_alv_tree( ).
        go_screen_state->set_grid_rendered( abap_true ).
*        go_screen_objects_simple->reset_text_edit( ).
      ENDIF.
    CATCH lcx_screen_error INTO go_err.
      MESSAGE go_err->get_text( ) TYPE 'E' DISPLAY LIKE 'I'.
  ENDTRY.
ENDMODULE.
