*----------------------------------------------------------------------*
***INCLUDE LZFILE_UTILSI02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300 INPUT.
  CASE ok_code.
    WHEN 'ENTER'.
      TRY.
          go_screen_objects_simple->handle_file_search(
            EXPORTING
                iv_search_term = CONV #( to_lower( p_sear3 ) )
                so_size = so_size[]
                so_chdat = so_chd[]
            CHANGING
                c_curs_info = p_cur ).
        CATCH lcx_screen_error INTO go_err.
          MESSAGE go_err->get_text( ) TYPE 'E' DISPLAY LIKE 'I'.
      ENDTRY.
    WHEN 'F_DROP_FIN'.
      file_txtedit = go_screen_objects_simple->mv_file_txt_edit.
*      BREAK-POINT.
    WHEN 'CRS_DOWN'.
      TRY.
          go_screen_objects_simple->move_cursor_down( CHANGING c_curs_info = p_cur ).
        CATCH lcx_screen_error.
      ENDTRY.
    WHEN 'CRS_UP'.
      TRY.
          go_screen_objects_simple->move_cursor_up( CHANGING c_curs_info = p_cur ).
        CATCH lcx_screen_error.
      ENDTRY.
    WHEN 'SELECTED'.
*      DATA: lo_file TYPE REF TO zcl_as_file.
*      TRY.
*          lo_file ?= go_screen_objects_simple->mo_selected_entity.
*          IF go_screen_state->mv_file_selection_allowed EQ abap_false.
*            MESSAGE 'File Selection not allowed! Please Select Directory' TYPE 'E' DISPLAY LIKE 'I'.
*          ENDIF.
*        CATCH cx_sy_move_cast_error.
*      ENDTRY.
      LEAVE TO SCREEN 0.
    WHEN 'SHOW_ERR'.
      MESSAGE go_screen_objects_simple->mo_exc->get_text( ) TYPE 'I'." DISPLAY LIKE 'I'.
    WHEN 'BTN_CLEAR'.
      CLEAR: so_chd[], so_size[], p_sear3.
    WHEN '%00881000002437848'.
      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
        EXPORTING
          title             = 'Auswahl File-Size'
          text              = 'File-Size'
        TABLES
          range             = so_size
        EXCEPTIONS
          no_range_tab      = 1
          cancelled         = 2
          internal_error    = 3
          invalid_fieldname = 4
          OTHERS            = 5.
*    WHEN '%01281000002437848'.
*      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
*        EXPORTING
*          title             = 'Auswahl File Owner'
*          text              = 'Besitzer'
*        TABLES
*          range             = so_own
*        EXCEPTIONS
*          no_range_tab      = 1
*          cancelled         = 2
*          internal_error    = 3
*          invalid_fieldname = 4.
    WHEN '%01281000002437848'.
      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
        EXPORTING
          title             = 'Auswahl Changed On'
          text              = 'Geändert am'
        TABLES
          range             = so_chd
        EXCEPTIONS
          no_range_tab      = 1
          cancelled         = 2
          internal_error    = 3
          invalid_fieldname = 4.
  ENDCASE.
ENDMODULE.
