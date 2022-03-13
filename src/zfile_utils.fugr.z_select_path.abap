FUNCTION Z_SELECT_PATH .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(IO_BASE_PATH) TYPE REF TO  ZCL_AS_DIRECTORY
*"     REFERENCE(IV_FILE_SEL_ALLOWED) TYPE  ABAP_BOOL DEFAULT ABAP_TRUE
*"  EXPORTING
*"     REFERENCE(ET_COMP) TYPE  ZCL_DIR_CONT=>TT_COMP
*"     REFERENCE(EV_FILNAME) TYPE  STRING
*"     REFERENCE(EV_FULLPATH) TYPE  STRING
*"     REFERENCE(EV_FILE_SELECTED) TYPE  ABAP_BOOL
*"  EXCEPTIONS
*"      CANCELLED
*"----------------------------------------------------------------------
  DATA: lo_file TYPE REF TO zcl_as_file,
        lo_dir  TYPE REF TO zcl_as_directory.
**********************************************************************
  CLEAR: et_comp, ev_file_selected, ev_filname.
  CLEAR: p_sear3, so_chd, so_size, file_txtedit, file_txtedit2, p_cur.
  CLEAR: file_search.
*  p_fil = abap_true.
**********************************************************************

*** Screen-Init
  PERFORM fill_labels.
  PERFORM create_icons.

  io_base_path->cnt_sub_selements( ).

  go_screen_state = NEW #( ).
  go_screen_state->set_file_selection_allowed( iv_file_sel_allowed ).

*** Instanz: Drop-Down Handler
  go_dd_handler = NEW #( ).
  go_dd_handler->set_dd_encoding( VALUE #( name = 'DD_ENCODING' ref_dd_f = REF #( dd_encoding ) ) ).
  go_dd_handler->set_dd_codepage( VALUE #( name = 'DD_CODEPAGE' ref_dd_f = REF #( dd_codepage ) ) ).
  go_dd_handler->set_dd_linefeed( VALUE #( name = 'DD_LINEFEED' ref_dd_f = REF #( dd_linefeed ) ) ).
  go_dd_filt_opt = NEW #( ).
  go_dd_filt_opt->set_dd_filt_opt( VALUE #( name = 'P_FDD' ref_dd_f = REF #( p_fdd ) ) ).
*
  IF go_screen_objects_col IS NOT BOUND. go_screen_objects_col = NEW #( ). ENDIF.

**** Referenz für Basis-Pfad übergeben
  go_screen_objects_col->set_root( io_base_path ).
  go_screen_objects_col->set_file_sel_allowed( iv_file_sel_allowed ).
  go_screen_objects_col->set_dd_handler( go_dd_handler ).
  go_screen_objects_col->set_dd_filt_opt( go_dd_filt_opt )..
  go_screen_objects_col->set_ref_log( REF #( file_txtedit2 ) ).
*  go_screen_objects_col->set_range_f_size( REF #( so_size ) ).

  CALL SCREEN 9200 STARTING AT 5 5 ENDING AT 192 35.

  TRY.
      go_screen_objects_col->delete_all_nodes( ).
    CATCH lcx_screen_error.
  ENDTRY.

  CASE ok_code.
    WHEN 'SELECTED'.
      TRY.
          lo_file ?= go_screen_objects_col->mo_selected_entity.
          et_comp = lo_file->mt_components.
          ev_filname = lo_file->mv_file_name.
          ev_fullpath = lo_file->mv_full_name.
          ev_file_selected = abap_true.
        CATCH cx_sy_move_cast_error.
          lo_dir ?= go_screen_objects_col->mo_selected_entity.
          et_comp = lo_dir->mt_components.
          ev_fullpath = lo_dir->mv_base_path.
      ENDTRY.
    WHEN 'CANC'.
      RAISE cancelled.
  ENDCASE.

ENDFUNCTION.
