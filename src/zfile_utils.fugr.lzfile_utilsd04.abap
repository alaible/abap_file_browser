*&---------------------------------------------------------------------*
*& Include          LZFILE_UTILSD02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_screen_objects
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_screen_objects_col DEFINITION INHERITING FROM lcl_screen_base.
  PUBLIC SECTION.
    TYPES: ref_char_tab TYPE REF TO t_c_text.
    TYPES: tt_ref_nodes TYPE TABLE OF REF TO zcl_dir_cont=>tt_node_key_tm.
    METHODS:
      constructor,
      build_alv_tree RAISING lcx_screen_error zcx_scrn_err zcx_tree_error,
      delete_all_nodes RAISING lcx_screen_error,
      move_cursor_down CHANGING c_curs_info TYPE lcl_match_cursor=>t_cursor_info RAISING zcx_tree_error,
      move_cursor_up CHANGING c_curs_info TYPE lcl_match_cursor=>t_cursor_info RAISING lcx_screen_error zcx_tree_error,
      handle_file_search IMPORTING iv_search_term TYPE string
                                   so_size        TYPE t_range_size
                                   so_chdat       TYPE t_range_chdat
                         CHANGING  c_curs_info    TYPE lcl_match_cursor=>t_cursor_info
                         RAISING   lcx_screen_error zcx_tree_error,
      handle_file_search_nofil IMPORTING iv_search_term TYPE string
                                         so_size        TYPE t_range_size
                                         so_chdat       TYPE t_range_chdat
                               CHANGING  c_curs_info    TYPE lcl_match_cursor=>t_cursor_info
                               RAISING   lcx_screen_error zcx_tree_error,
      handle_file_search_fil IMPORTING iv_search_term TYPE string
                                       so_size        TYPE t_range_size
                                       so_chdat       TYPE t_range_chdat
                             CHANGING  c_curs_info    TYPE lcl_match_cursor=>t_cursor_info
                             RAISING   lcx_screen_error zcx_tree_error,
      refresh_file IMPORTING iv_legacy_mode TYPE abap_bool RAISING zcx_file_err,
      reset_tree CHANGING c_curs_info    TYPE lcl_match_cursor=>t_cursor_info RAISING lcx_screen_error zcx_tree_error,
      reset_text_edit RAISING zcx_scrn_err,
      set_root IMPORTING io_root_dir TYPE REF TO zcl_as_directory,
      set_dd_handler IMPORTING io_dd_handler TYPE REF TO lcl_dd_handler,
      set_file_sel_allowed IMPORTING iv_file_sel_allowed TYPE abap_bool,
      set_dd_filt_opt IMPORTING io_dd_filt_opt TYPE REF TO lcl_dd_filter,
      set_range_f_size IMPORTING ir_range TYPE REF TO t_range_size,
      disable_log,
      set_ref_log IMPORTING ir_log TYPE REF TO char255.
    DATA: mv_alv_constructed TYPE abap_bool.
**********************************************************************
    DATA: mr_range_size TYPE REF TO t_range_size.
*** Referenz auf ausgew. Pfad/File
    DATA: mo_selected_entity        TYPE REF TO zcl_dir_cont READ-ONLY,
          mv_file_txt_edit          TYPE string READ-ONLY,
          mv_file_selection_allowed TYPE abap_bool.
*** Public Error Object
    DATA: mo_exc TYPE REF TO cx_root READ-ONLY.
  PROTECTED SECTION.
    METHODS:
*      init_alv_tree RAISING lcx_screen_error,
*      set_te_read_only,
      set_te_edit_mode,
      init_dd_objects RAISING lcx_screen_error,
      merge_node_tables IMPORTING it_ref TYPE tt_ref_nodes,
      register_events REDEFINITION,
      register_events_toolb REDEFINITION,
      get_hierarchy_hdr REDEFINITION,
      tree_add_colums REDEFINITION,
      add_toolb_buttons REDEFINITION.
    METHODS:
      get_container_name REDEFINITION,
      get_left_splter_width REDEFINITION.
    METHODS:
      perform_search IMPORTING iv_search_term TYPE string
                               i_size         TYPE t_range_size
                               i_chdat        TYPE t_range_chdat
                               i_filter       TYPE abap_bool,
      add_root_node_to_tree RAISING lcx_screen_error zcx_tree_error,
      reset_styles RAISING zcx_tree_error,
      set_styles_from_matches RAISING zcx_tree_error,
      expand_matches RAISING zcx_tree_error,
      reset_visible_entries,
      find_subtree IMPORTING ir_child_nodes TYPE REF TO tt_nodekey RAISING zcx_tree_error,
      find_all_child_elements IMPORTING iv_nodekey TYPE tm_nodekey ir_child_nodes TYPE REF TO tt_nodekey,
      expand_sub_tree RAISING zcx_tree_error,
      collapse_sub_tree RAISING zcx_tree_error.
    METHODS:
      find_all_child_nodes
        IMPORTING
          iv_node_key TYPE zcl_dir_cont=>ty_node_table_tmc-node_key
          ir_matches  TYPE zcl_dir_cont=>tr_node_search,
      find_all_parent_nodes
        IMPORTING
          iv_node_key TYPE zcl_dir_cont=>ty_node_table_tmc-node_key
          ir_matches  TYPE zcl_dir_cont=>tr_node_search.
**********************************************************************
    METHODS:
      log_info IMPORTING i_info TYPE string.
**********************************************************************
    DATA: mr_node_table           TYPE REF TO zcl_dir_cont=>tt_file_alv,
          mr_node_table_tmc       TYPE REF TO zcl_dir_cont=>tty_node_table_tmc,
          mr_search_index         TYPE REF TO zcl_dir_cont=>tty_search_index,
          mr_search_index_size    TYPE REF TO zcl_dir_cont=>tty_search_index_size,
          mr_search_index_changed TYPE REF TO zcl_dir_cont=>tty_search_index_changed.
**********************************************************************
*** Event-Handler column_tree_model
    METHODS:
      handle_drag FOR EVENT drag OF cl_column_tree_model IMPORTING node_key drag_drop_object item_name,
      handle_on_drop FOR EVENT on_drop OF cl_gui_textedit IMPORTING dragdrop_object.
*** Event-Handler
    METHODS:
      handle_node_double_click FOR EVENT node_double_click OF cl_column_tree_model IMPORTING node_key,
      handle_item_double_click FOR EVENT item_double_click OF cl_column_tree_model IMPORTING node_key.
*** Toolbar!
    METHODS:
      on_function_selected FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode.
*    methods:
*      handle_double_click for event double_click of cl_column_tree_model IMPORTING
**********************************************************************
    CONSTANTS: lc_contr_name TYPE string VALUE 'CUST_CONTR2'.
    DATA: mo_dd_handler  TYPE REF TO lcl_dd_handler,
          mo_dd_filt_opt TYPE REF TO lcl_dd_filter,
          mr_log         TYPE REF TO char255.
**********************************************************************
    DATA: mr_matches      TYPE REF TO zcl_dir_cont=>tt_node_search,
          mr_curr_nodekey TYPE REF TO tm_nodekey.
**********************************************************************
*** Drag and Drop Functionality!
    DATA: mo_dd_tree TYPE REF TO cl_dragdrop,
          mo_dd_text TYPE REF TO cl_dragdrop.
    DATA: mv_handle_tree TYPE i,
          mv_handle_file TYPE i.
*** Flag, ob Einträge gefiltert wurden (zum zurücksetzen)
    DATA: mv_filtered     TYPE abap_bool,
          mv_log_disabled TYPE abap_bool.
*** Root Directory
    DATA: mo_root TYPE REF TO zcl_as_directory.
*** Match-Cursor
    DATA: mo_match_cursor TYPE REF TO lcl_match_cursor.
ENDCLASS.

CLASS lcl_screen_objects_col IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mo_match_cursor = NEW #( ).
  ENDMETHOD.
  METHOD set_root.
    mo_root = io_root_dir.
  ENDMETHOD.
  METHOD set_range_f_size.
    mr_range_size = ir_range.
  ENDMETHOD.
  METHOD set_dd_handler.
    mo_dd_handler = io_dd_handler.
  ENDMETHOD.
  METHOD set_dd_filt_opt.
    mo_dd_filt_opt = io_dd_filt_opt.
  ENDMETHOD.
  METHOD set_file_sel_allowed.
    mv_file_selection_allowed = iv_file_sel_allowed.
  ENDMETHOD.
  METHOD build_alv_tree.
    IF mv_alv_constructed EQ abap_false.
      me->init_container( ).
      me->init_splitter( ).
      me->init_splitter_left( ).
      me->init_dd_objects( ).
*** old
*      me->init_alv_tree( ).
      me->init_alv_tree_col( ).
      me->init_toolbar( ).
      me->init_text_edit( ).
      mo_text_edit->set_dragdrop( mo_dd_text ).
**********************************************************************
      SET HANDLER me->handle_on_drop FOR mo_text_edit.
      SET HANDLER me->handle_drag FOR mo_alv_tree.
*      me->set_te_read_only( ).
      mr_node_table = NEW #( ).
*** Flag seltzen -> Initialisierung erfolgt
      mv_alv_constructed = abap_true.
    ENDIF.
    me->add_root_node_to_tree( ).
*    mo_alv_tree->frontend_update( ).
  ENDMETHOD.
  METHOD add_root_node_to_tree.
    IF mr_node_table_tmc IS BOUND. FREE mr_node_table_tmc. ENDIF.
    IF mr_search_index IS BOUND. FREE mr_search_index. ENDIF.
**********************************************************************
    mr_node_table_tmc = NEW #( ).
    mr_search_index = NEW #( ).
    mr_search_index_changed = NEW #( ).
    mr_search_index_size = NEW #( ).
    mr_nodetab_int = NEW #( ).
    mr_itemtab_int = NEW #( ).
    mr_curr_nodekey = NEW #( ).
**********************************************************************
    mo_root->build_node_table(
      EXPORTING
        ir_node_tab     = mr_nodetab_int
        ir_item_tab     = mr_itemtab_int
        iv_parent_key   = space
        ir_current_key  = mr_curr_nodekey
        iv_dd_id        = CONV #( mv_handle_tree )
        ir_node_table   = mr_node_table_tmc
        ir_search_index = mr_search_index
        ir_search_index_changed = mr_search_index_changed
        ir_search_index_size = mr_search_index_size
    ).
    me->add_nodes( ).
    me->add_items( ).
    CHECK lines( mr_nodetab_int->* ) GT 0.
*** Expand Root-Node
    mo_alv_tree->expand_node( node_key = mr_nodetab_int->*[ 1 ]-node_key ).
*    CATCH lcx_screen_error.
  ENDMETHOD.
  METHOD add_toolb_buttons.
    mo_toolbar->add_button( fcode     = 'EXPAND_N'
                            butn_type = cntb_btype_button
                            icon      = icon_expand_all ).
    mo_toolbar->add_button( fcode     = 'COLLPASE_N'
                            butn_type = cntb_btype_button
                            icon      = icon_collapse_all ).
    mo_toolbar->add_button( fcode     = ''
                            butn_type = cntb_btype_sep
                            icon      = '' ).
    mo_toolbar->add_button( fcode     = 'PRINT'
                            butn_type = cntb_btype_button
                            icon      = icon_print ).
  ENDMETHOD.
  METHOD get_container_name.
  ENDMETHOD.
  METHOD get_left_splter_width.
    rv_left_width = 40.
  ENDMETHOD.
  METHOD get_hierarchy_hdr.
    rs_hdr = VALUE #( hierarchy_column_name = 'NODE_NAME'
                      hierarchy_header      = VALUE #(
                                                 t_image = icon_folder
                                                 heading = 'Element-Name'
                                                 tooltip = 'Element-Name'
                                                 width   = 40
                                              )
                     ).      " Name der Spalte im Hierarchie-Bereich

  ENDMETHOD.
  METHOD tree_add_colums.
    mo_alv_tree->add_column( name = 'SIZE' width = 20 header_text = 'Size(KB)' alignment = cl_column_tree_model=>align_right ).
    mo_alv_tree->add_column( name = 'CHANGED' width = 20 header_text = 'Changed On' alignment = cl_column_tree_model=>align_right ).
    mo_alv_tree->add_column( name = 'CNT_SUBEL' width = 20 header_text = 'Cnt Subelements' alignment = cl_column_tree_model=>align_right ).
  ENDMETHOD.
  METHOD reset_tree.
    me->delete_all_nodes( ).
    me->add_root_node_to_tree( ).
    mv_filtered = abap_false.
*    CATCH lcx_screen_error.
    mo_match_cursor->clear_cursor_info(
      CHANGING
        c_cursor_info = c_curs_info
    ).
    me->log_info( |Resetting tree finished| ).
  ENDMETHOD.
  METHOD reset_text_edit.
    DATA: empty_text TYPE t_c_text.
*** leere Tabelle für leeres Text-Edit
    empty_text = VALUE #( ).
    me->set_text_as_table( empty_text ).
*    CATCH zcx_scrn_err. " Allg. Screen-Fehler
  ENDMETHOD.
  METHOD reset_styles.
    LOOP AT mr_node_table_tmc->* ASSIGNING FIELD-SYMBOL(<node_entry>) WHERE highlighted EQ abap_true.
      me->item_set_style(
        EXPORTING
          iv_node_key  = <node_entry>-node_key
          iv_item_name = 'NODE_NAME'
          iv_style     = cl_column_tree_model=>style_default
      ).
*      CATCH zcx_tree_error. " Tree-Error
      me->item_set_style(
        EXPORTING
          iv_node_key  = <node_entry>-node_key
          iv_item_name = 'SIZE'
          iv_style     = cl_column_tree_model=>style_default
      ).
*      CATCH zcx_tree_error. " Tree-Error
    ENDLOOP.
  ENDMETHOD.
  METHOD set_styles_from_matches.
*** Node-Style setzen
    LOOP AT mr_matches->* ASSIGNING FIELD-SYMBOL(<match>).
      IF <match>-highl EQ abap_true.
        me->item_set_style(
          EXPORTING
            iv_node_key       = <match>-node_key
            iv_item_name      = 'NODE_NAME'
            iv_style          = cl_column_tree_model=>style_emphasized_positive
        ).
      ENDIF.
      IF <match>-size EQ abap_true.
        me->item_set_style(
          EXPORTING
            iv_node_key       = <match>-node_key
            iv_item_name      = 'SIZE'
            iv_style          = cl_column_tree_model=>style_emphasized_a
        ).
      ENDIF.
      IF <match>-changed EQ abap_true.
        me->item_set_style(
          EXPORTING
            iv_node_key       = <match>-node_key
            iv_item_name      = 'CHANGED'
            iv_style          = cl_column_tree_model=>style_emphasized_a
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD handle_node_double_click.
**********************************************************************
*** Knoten!
    TYPES: BEGIN OF ty_node_el,
             out_tab_line TYPE zfile_exp_alv_tree,
             node_text    TYPE lvc_value,
             item_layout  TYPE lvc_t_layi,
             node_layout  TYPE  lvc_s_layn,
           END OF ty_node_el.
    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tty_node_table_tmc,
                   <node_el>    TYPE zcl_dir_cont=>ty_node_table_tmc.
    DATA: lo_file TYPE REF TO zcl_as_file.
    DATA: ls_node        TYPE ty_node_el,
          lt_item_layout TYPE lvc_t_laci.
    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
    READ TABLE <node_table> WITH KEY node_key = node_key ASSIGNING <node_el>.
**********************************************************************
    CHECK sy-subrc = 0.
    mo_selected_entity = <node_el>-entity.
    TRY.
        lo_file ?= mo_selected_entity.
        IF mv_file_selection_allowed EQ abap_false.
          MESSAGE 'File Selection not allowed! Please Select Directory' TYPE 'I'.
          RETURN.
        ENDIF.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
    cl_gui_cfw=>set_new_ok_code( EXPORTING new_code = 'SELECTED' ).
**********************************************************************
  ENDMETHOD.
  METHOD on_function_selected.
    DATA: lo_err TYPE REF TO cx_static_check.
    CASE fcode.
      WHEN 'EXPAND_N'.
        TRY.
            me->expand_sub_tree( ).
          CATCH zcx_tree_error INTO lo_err.
            MESSAGE lo_err->get_text( ) TYPE 'I'.
        ENDTRY.
      WHEN 'COLLPASE_N'.
        TRY.
            me->collapse_sub_tree( ).
          CATCH zcx_tree_error INTO lo_err.
            MESSAGE lo_err->get_text( ) TYPE 'I'.
        ENDTRY.
      WHEN 'PRINT'.
    ENDCASE.
  ENDMETHOD.
  METHOD handle_item_double_click.
    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tty_node_table_tmc,
                   <node_el>    TYPE zcl_dir_cont=>ty_node_table_tmc.
    DATA: lo_file TYPE REF TO zcl_as_file.
    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
    READ TABLE <node_table> WITH KEY node_key = node_key ASSIGNING <node_el>.
    CHECK sy-subrc = 0.
    mo_selected_entity = <node_el>-entity.
    TRY.
        lo_file ?= mo_selected_entity.
        IF mv_file_selection_allowed EQ abap_false.
          MESSAGE 'File Selection not allowed! Please Select Directory' TYPE 'I'.
          RETURN.
        ENDIF.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
    cl_gui_cfw=>set_new_ok_code( EXPORTING new_code = 'SELECTED' ).
  ENDMETHOD.
  METHOD handle_drag.
    FIELD-SYMBOLS: <nodetab_tmc> TYPE zcl_dir_cont=>tty_node_table_tmc.
    ASSIGN mr_node_table_tmc->* TO <nodetab_tmc>.
    READ TABLE <nodetab_tmc> WITH KEY node_key = node_key ASSIGNING FIELD-SYMBOL(<node_entry>).
    CHECK sy-subrc = 0.
    me->set_te_edit_mode( ).
    drag_drop_object->object = <node_entry>-entity.
*    BREAK-POINT.
  ENDMETHOD.
  METHOD move_cursor_down.
    mo_match_cursor->go_down( CHANGING c_cursor_info = c_curs_info ).
    me->set_selected_nkey( mo_match_cursor->mv_current_node ).
*    CATCH zcx_tree_error. " Tree-Error
  ENDMETHOD.
  METHOD move_cursor_up.
    mo_match_cursor->go_up( CHANGING c_cursor_info = c_curs_info ).
    me->set_selected_nkey( mo_match_cursor->mv_current_node ).
  ENDMETHOD.
  METHOD handle_on_drop.
    DATA: lo_file       TYPE REF TO zcl_as_file,
          lv_line       TYPE string,
          lt_content    TYPE TABLE OF char1024,
          l_single_line TYPE string.
**********************************************************************
    TRY.
        lo_file ?= dragdrop_object->object.
        OPEN DATASET lo_file->mv_full_name FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
        DO.
          READ DATASET lo_file->mv_full_name INTO lv_line.
          IF sy-subrc <> 0.
            EXIT.
          ELSE.
            APPEND lv_line TO lt_content.
            l_single_line = |{ l_single_line }{ COND #( WHEN sy-index NE 1 THEN cl_abap_char_utilities=>cr_lf ELSE space ) }{ lv_line }|.
          ENDIF.
        ENDDO.
* Datei schließen
        CLOSE DATASET lo_file->mv_full_name.
        DATA(splitted) = zcl_file_scr_utils=>split_string(
                           i_string  = l_single_line
                           i_max_len = 1024
                         ).
*        me->set_text_as_table( lt_content ).
        me->set_text_as_stream( CONV #( splitted ) ).
*        CATCH zcx_scrn_err. " Allg. Screen-Fehler
        mv_file_txt_edit = lo_file->mv_full_name.
*** Set new OK-Code -> Filename is Updated in Dynpro (PAI)
        cl_gui_cfw=>set_new_ok_code( 'F_DROP_FIN' ).
      CATCH cx_root INTO DATA(lo_err).
        mo_exc = lo_err.
        cl_gui_cfw=>set_new_ok_code( 'SHOW_ERR' ).
    ENDTRY.
  ENDMETHOD.
  METHOD refresh_file.
    DATA: lt_content    TYPE TABLE OF char1024,
          lv_line       TYPE string,
          l_single_line TYPE string.
    DATA: lv_enc      TYPE lcl_dd_handler=>t_char_255,
          lv_cp       TYPE lcl_dd_handler=>t_char_255,
          lv_linefeed TYPE lcl_dd_handler=>t_char_255.
**********************************************************************
    TRY.
        lv_enc = mo_dd_handler->get_encoding( ).
      CATCH lcx_no_value_set.
    ENDTRY.
    TRY.
        lv_cp = mo_dd_handler->get_codepage( ).
      CATCH lcx_no_value_set.
    ENDTRY.
    TRY.
        lv_linefeed = mo_dd_handler->get_linefeed( ).
      CATCH lcx_no_value_set.
    ENDTRY.
**********************************************************************
    TRY.
        CASE abap_true.
          WHEN iv_legacy_mode.
*            CHECK lv_linefeed IS NOT INITIAL.
            CASE lv_linefeed.
              WHEN lcl_dd_handler=>c_linefeed-native.
                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
              WHEN lcl_dd_handler=>c_linefeed-smart.
                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
              WHEN lcl_dd_handler=>c_linefeed-unix.
                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
              WHEN lcl_dd_handler=>c_linefeed-windows.
                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
              WHEN OTHERS.
                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS.
            ENDCASE.
**********************************************************************
          WHEN OTHERS.
            CASE lv_enc.
              WHEN lcl_dd_handler=>c_encoding-default.
                CASE lv_linefeed.
                  WHEN lcl_dd_handler=>c_linefeed-native.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-smart.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-windows.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-unix.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
                  WHEN OTHERS.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
                ENDCASE.
              WHEN lcl_dd_handler=>c_encoding-utf_8.
                CASE lv_linefeed.
                  WHEN lcl_dd_handler=>c_linefeed-native.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-smart.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-windows.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-unix.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
                  WHEN OTHERS.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS.
                ENDCASE.
              WHEN lcl_dd_handler=>c_encoding-non_unicode.
                CASE lv_linefeed.
                  WHEN lcl_dd_handler=>c_linefeed-native.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-smart.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-windows.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-unix.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
                  WHEN OTHERS.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS.
                ENDCASE.
              WHEN OTHERS.
                CASE lv_linefeed.
                  WHEN lcl_dd_handler=>c_linefeed-native.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-smart.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-windows.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
                  WHEN lcl_dd_handler=>c_linefeed-unix.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
                  WHEN OTHERS.
                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
                ENDCASE.
            ENDCASE.
        ENDCASE.
        DO.
          READ DATASET mv_file_txt_edit INTO lv_line.
          IF sy-subrc <> 0.
            EXIT.
          ELSE.
            l_single_line = |{ l_single_line }{ COND #( WHEN sy-index NE 1 THEN cl_abap_char_utilities=>cr_lf ELSE space ) }{ lv_line }|.
*            APPEND lv_line TO lt_content.
          ENDIF.
        ENDDO.
* Datei schließen
        CLOSE DATASET mv_file_txt_edit.
        DATA(splitted) = zcl_file_scr_utils=>split_string(
                           i_string  = l_single_line
                           i_max_len = 1024
                         ).
*        me->set_text_as_table( lt_content ).
        me->set_text_as_stream( CONV #( splitted ) ).
*        CATCH zcx_scrn_err. " Allg. Screen-Fehler
        me->log_info( |Reload finished| ).
      CATCH cx_root INTO DATA(lo_err).
*        DATA(lv_txt) = lo_err->get_text( ).
        RAISE EXCEPTION TYPE zcx_file_err
          EXPORTING
*           textid   =
*           previous =
            mv_msgv1 = CONV #( lo_err->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
  METHOD find_all_parent_nodes.
    DATA: lv_parent_node_key TYPE zcl_dir_cont=>ty_node_table_lvc-node_key.
    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tty_node_table_tmc,
                   <node_entry> TYPE zcl_dir_cont=>ty_node_table_tmc.
    FIELD-SYMBOLS: <matches> TYPE zcl_dir_cont=>tt_node_search,
                   <match>   TYPE zcl_dir_cont=>t_node_search.
**********************************************************************
    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
    ASSIGN ir_matches->* TO <matches>.
**********************************************************************
    READ TABLE <node_table> WITH KEY node_key = iv_node_key ASSIGNING <node_entry>.
*** Loop until root node is reached (parent_node_key = space)
    WHILE <node_entry>-parent_node_key NE space.
      lv_parent_node_key = <node_entry>-parent_node_key.
*** Achtung: <matches> ist hashed table
      IF line_exists( <matches>[ node_key = lv_parent_node_key ] ).
        READ TABLE <matches> WITH KEY node_key = lv_parent_node_key ASSIGNING <match>.
        <match>-parent = abap_true.
      ELSE.
        INSERT VALUE #( node_key = lv_parent_node_key parent = abap_true ) INTO TABLE <matches>.
      ENDIF.
      READ TABLE <node_table> WITH KEY node_key = lv_parent_node_key ASSIGNING <node_entry>.
    ENDWHILE.
  ENDMETHOD.
  METHOD find_all_child_nodes.
    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tty_node_table_tmc,
                   <node_entry> TYPE zcl_dir_cont=>ty_node_table_tmc.
    FIELD-SYMBOLS: <matches> TYPE zcl_dir_cont=>tt_node_search,
                   <match>   TYPE zcl_dir_cont=>t_node_search.
    DATA: lo_dir TYPE REF TO zcl_as_directory.
**********************************************************************
    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
    ASSIGN ir_matches->* TO <matches>.

*** Find all nodes where key = parent_node_key
    LOOP AT <node_table> ASSIGNING <node_entry> WHERE parent_node_key = iv_node_key.
      IF line_exists( <matches>[ node_key = <node_entry>-node_key ] ).
        READ TABLE <matches> WITH KEY node_key = <node_entry>-node_key ASSIGNING <match>.
        <match>-child = abap_true.
      ELSE.
        INSERT VALUE #( node_key = <node_entry>-node_key child = abap_true ) INTO TABLE <matches>.
      ENDIF.
*** If node_entry is directory -> call find_all_child_nodes recursively!
      TRY.
          lo_dir ?= <node_entry>-entity.
          me->find_all_child_nodes( iv_node_key = <node_entry>-node_key ir_matches = ir_matches ).
        CATCH cx_sy_move_cast_error.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  METHOD expand_matches.
    DATA: lt_nodes TYPE tt_nodekey.
**********************************************************************
    LOOP AT mr_matches->* ASSIGNING FIELD-SYMBOL(<match>) WHERE parent = abap_true.
      APPEND <match>-node_key TO lt_nodes.
    ENDLOOP.
    me->expand_nodes( lt_nodes ).
  ENDMETHOD.
  METHOD find_all_child_elements.
    FIELD-SYMBOLS: <children> TYPE tt_nodekey,
                   <node_tab> TYPE zcl_dir_cont=>tty_node_table_tmc.
    DATA: mv_node_key TYPE tm_nodekey.
    ASSIGN ir_child_nodes->* TO <children>.
    ASSIGN mr_node_table_tmc->* TO <node_tab>.
**********************************************************************
    mv_node_key = iv_nodekey.
    LOOP AT <node_tab> ASSIGNING FIELD-SYMBOL(<node_entry>) WHERE parent_node_key = mv_node_key.
      IF <node_entry>-is_complex EQ abap_true.
        IF mv_filtered EQ abap_true AND <node_entry>-vis_filtered EQ abap_false. RETURN. ENDIF.
        APPEND <node_entry>-node_key TO <children>.
        me->find_all_child_elements(
          EXPORTING
            iv_nodekey     = <node_entry>-node_key
            ir_child_nodes = ir_child_nodes
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD reset_visible_entries.
*** Reset Flag vis filtered
    LOOP AT me->mr_node_table_tmc->* ASSIGNING FIELD-SYMBOL(<node_entry>) WHERE vis_filtered EQ abap_true.
      <node_entry>-vis_filtered = abap_false.
    ENDLOOP.
  ENDMETHOD.
  METHOD find_subtree.
    DATA: lv_selected_nkey TYPE tm_nodekey.
**********************************************************************
    lv_selected_nkey = me->get_selected_nkey( ).
    CHECK lv_selected_nkey IS NOT INITIAL.
    CHECK mr_node_table_tmc->*[ node_key = lv_selected_nkey ]-is_complex EQ abap_true.
    APPEND lv_selected_nkey TO ir_child_nodes->*.
    me->find_all_child_elements(
      EXPORTING
        iv_nodekey     = lv_selected_nkey
        ir_child_nodes = ir_child_nodes
    ).
  ENDMETHOD.
  METHOD collapse_sub_tree.
    DATA: lr_children TYPE REF TO tt_nodekey.
**********************************************************************
    lr_children = NEW #( ).
    me->find_subtree( lr_children ).
    LOOP AT lr_children->* ASSIGNING FIELD-SYMBOL(<child>).
      me->collapse_node( <child> ).
    ENDLOOP.
  ENDMETHOD.
  METHOD expand_sub_tree.
    DATA: lr_children TYPE REF TO tt_nodekey.
    lr_children = NEW #( ).
    me->find_subtree( lr_children ).
    me->expand_nodes( CONV #( lr_children->* ) ).
  ENDMETHOD.
  METHOD handle_file_search.
    TRY.
        CASE mo_dd_filt_opt->get_fil_opt( ).
          WHEN lcl_dd_filter=>c_filt_opt-filter_all
            OR lcl_dd_filter=>c_filt_opt-filter_file_only.
            IF iv_search_term IS INITIAL AND so_size[] IS INITIAL AND so_chdat IS INITIAL.
              me->reset_tree( CHANGING c_curs_info = c_curs_info ).
*              CATCH lcx_screen_error.
            ELSE.
              me->handle_file_search_fil(
                EXPORTING
                  iv_search_term = iv_search_term
                  so_size        = so_size[]
                  so_chdat       = so_chdat[]
                CHANGING
                  c_curs_info    = c_curs_info
              ).
*            CATCH lcx_screen_error.
            ENDIF.
          WHEN lcl_dd_filter=>c_filt_opt-nofilter.
            me->handle_file_search_nofil(
              EXPORTING
                iv_search_term = iv_search_term
                so_size        = so_size[]
                so_chdat       = so_chdat[]
              CHANGING
                c_curs_info    = c_curs_info
            ).
        ENDCASE.
      CATCH lcx_no_value_set.
    ENDTRY.
  ENDMETHOD.
  METHOD handle_file_search_nofil.
**********************************************************************
*** Knoten!
    TYPES: BEGIN OF ty_node_el,
             out_tab_line TYPE zfile_exp_alv_tree,
             node_text    TYPE lvc_value,
             item_layout  TYPE lvc_t_layi,
             node_layout  TYPE  lvc_s_layn,
           END OF ty_node_el.
**********************************************************************
    FIELD-SYMBOLS: <search_index>  TYPE zcl_dir_cont=>tty_search_index,
                   <node_tab_line> TYPE zcl_dir_cont=>ty_node_table_tmc.
    FIELD-SYMBOLS: <search_matches> TYPE zcl_dir_cont=>tt_node_search,
                   <match>          TYPE zcl_dir_cont=>t_node_search.
    DATA: ls_node        TYPE ty_node_el,
          lt_item_layout TYPE lvc_t_laci.
**********************************************************************
    DATA: lt_check_nodes TYPE TABLE OF tm_nodekey.
**********************************************************************
*** if tree was previously filtered via handle_file_search_fil,
*** the tree is rebuilt so that all elements are added and therefore
*** are available for highlighting
    IF mv_filtered = abap_true.
      me->delete_all_nodes( ).
      me->add_root_node_to_tree( ).
      mv_filtered = abap_false.
    ENDIF.
*** Initial Styles zurücksetzen!
    me->reset_styles( ).
*    CATCH lcx_screen_error.
    mo_alv_tree->collapse_all_nodes( ).
    me->expand_node( mr_nodetab_int->*[ 1 ]-node_key ).
*    CATCH zcx_tree_error. " Tree-Error
**********************************************************************
    me->perform_search(
      EXPORTING
        iv_search_term = iv_search_term
        i_size         = so_size[]
        i_chdat        = so_chdat[]
        i_filter       = abap_false
    ).
**********************************************************************
    lt_check_nodes = VALUE #( FOR match IN mr_matches->* ( match-node_key ) ).
    LOOP AT lt_check_nodes ASSIGNING FIELD-SYMBOL(<_node>).
      me->find_all_parent_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
*      me->find_all_child_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
    ENDLOOP.
    me->set_styles_from_matches( ).
*CATCH lcx_screen_error.
    me->expand_matches( ).
*    CATCH lcx_screen_error.
    IF lines( mr_matches->* ) GE 1.
      mo_match_cursor->init_from_ref( EXPORTING ir_ref = mr_matches CHANGING c_cursor_info = c_curs_info ).
      mo_alv_tree->set_selected_node( mo_match_cursor->mv_current_node ).
    ELSEIF iv_search_term IS NOT INITIAL.
      mo_match_cursor->set_empty_result( CHANGING c_cursor_info = c_curs_info ).
    ENDIF.
**********************************************************************
    cl_gui_cfw=>flush( ).
  ENDMETHOD.
  METHOD handle_file_search_fil.
    FIELD-SYMBOLS: <search_index>      TYPE zcl_dir_cont=>tty_search_index,
                   <search_index_size> TYPE zcl_dir_cont=>tty_search_index_size,
                   <node_tab>          TYPE zcl_dir_cont=>tty_node_table_tmc,
                   <node_tab_line>     TYPE zcl_dir_cont=>ty_node_table_tmc.
    FIELD-SYMBOLS: <search_matches> TYPE zcl_dir_cont=>tt_node_search,
                   <match>          TYPE zcl_dir_cont=>t_node_search.
    DATA: lt_item_layout TYPE lvc_t_laci,
          lt_check_nodes TYPE TABLE OF tm_nodekey.
**********************************************************************
*    IF iv_search_term IS INITIAL.
*      me->delete_all_nodes( ).
*      me->add_root_node_to_tree( ).
**      RETURN.
**      CATCH lcx_screen_error.
*    ENDIF.
**********************************************************************
    me->perform_search(
      EXPORTING
        iv_search_term = iv_search_term
        i_size         = so_size[]
        i_chdat        = so_chdat[]
        i_filter       = abap_true
    ).
    lt_check_nodes = VALUE #( FOR match IN mr_matches->* ( match-node_key ) ).
    LOOP AT lt_check_nodes ASSIGNING FIELD-SYMBOL(<_node>).
      me->find_all_parent_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
      TRY.
          IF mo_dd_filt_opt->get_fil_opt( ) EQ lcl_dd_filter=>c_filt_opt-filter_file_only.
            me->find_all_child_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
          ENDIF.
        CATCH lcx_no_value_set.
      ENDTRY.
    ENDLOOP.
    me->delete_all_nodes( ).
    me->reset_visible_entries( ).
*    CATCH lcx_screen_error.
    FREE: mr_nodetab_int, mr_itemtab_int.
    mr_nodetab_int = NEW #( ).
    mr_itemtab_int = NEW #( ).
    mo_root->build_node_table_fil(
      EXPORTING
        ir_node_tab     = mr_nodetab_int
        ir_item_tab     = mr_itemtab_int
        iv_dd_id        = CONV #( mv_handle_tree )
        ir_matches      = mr_matches
        ir_node_table   = mr_node_table_tmc
    ).
    me->add_nodes( ).
    me->add_items( ).
    me->expand_matches( ).
*    CATCH zcx_tree_error. " Tree-Error
    IF lines( mr_matches->* ) GE 1.
      mo_match_cursor->init_from_ref( EXPORTING ir_ref = mr_matches CHANGING c_cursor_info = c_curs_info ).
      me->set_selected_nkey( mo_match_cursor->mv_current_node ).
*      CATCH zcx_tree_error. " Tree-Error
    ELSE.
      mo_match_cursor->set_empty_result( CHANGING c_cursor_info = c_curs_info ).
    ENDIF.
    mv_filtered = abap_true.
**********************************************************************
    cl_gui_cfw=>flush( ).
  ENDMETHOD.
  METHOD perform_search.
**********************************************************************
    FIELD-SYMBOLS: <search_index>  TYPE zcl_dir_cont=>tty_search_index,
                   <node_tab_line> TYPE zcl_dir_cont=>ty_node_table_tmc.
    FIELD-SYMBOLS: <search_matches> TYPE zcl_dir_cont=>tt_node_search,
                   <match>          TYPE zcl_dir_cont=>t_node_search.
**********************************************************************
    IF mr_matches IS NOT BOUND. mr_matches = NEW #( ). ENDIF.
    ASSIGN mr_matches->* TO <search_matches>.
    CLEAR <search_matches>.
**********************************************************************
    ASSIGN me->mr_search_index->* TO <search_index>.
*    cl_gui_column_tree=>style_emphasized_positive
    LOOP AT <search_index> ASSIGNING FIELD-SYMBOL(<search_item>).
      IF iv_search_term IS NOT INITIAL AND matches( val = <search_item>-index regex = |.*{ iv_search_term }.*| ).
        INSERT VALUE #( node_key = <search_item>-node_key_tm highl = abap_true ) INTO TABLE <search_matches>.
*        APPEND <search_item>-node_key_tm TO <nodes_to_high>.
        IF i_filter EQ abap_false.
          READ TABLE mr_node_table_tmc->* WITH KEY node_key = <search_item>-node_key_tm ASSIGNING <node_tab_line>.
          <node_tab_line>-highlighted = abap_true.
          UNASSIGN <node_tab_line>.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF i_size[] IS NOT INITIAL.
      LOOP AT mr_search_index_size->* ASSIGNING FIELD-SYMBOL(<index_size>) WHERE file_size IN i_size.
        IF NOT line_exists( <search_matches>[ node_key = <index_size>-node_key_tm ] ).
          INSERT VALUE #( node_key = <index_size>-node_key_tm size = abap_true ) INTO TABLE <search_matches>.
        ELSE.
          READ TABLE <search_matches> WITH KEY node_key = <index_size>-node_key_tm ASSIGNING <match>.
          <match>-size = abap_true.
        ENDIF.
        IF i_filter EQ abap_false.
          READ TABLE mr_node_table_tmc->* WITH KEY node_key = <index_size>-node_key_tm ASSIGNING <node_tab_line>.
          <node_tab_line>-highlighted = abap_true.
          UNASSIGN <node_tab_line>.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF i_chdat[] IS NOT INITIAL.
      LOOP AT mr_search_index_changed->* ASSIGNING FIELD-SYMBOL(<index_chdat>) WHERE changed IN i_chdat.
        IF NOT line_exists( <search_matches>[ node_key = <index_chdat>-node_key_tm ] ).
          INSERT VALUE #( node_key = <index_chdat>-node_key_tm changed = abap_true ) INTO TABLE <search_matches>.
        ELSE.
          READ TABLE <search_matches> WITH KEY node_key = <index_chdat>-node_key_tm ASSIGNING <match>.
          <match>-changed = abap_true.
        ENDIF.
        IF i_filter = abap_false.
          READ TABLE mr_node_table_tmc->* WITH KEY node_key = <index_chdat>-node_key_tm ASSIGNING <node_tab_line>.
          <node_tab_line>-highlighted = abap_true.
          UNASSIGN <node_tab_line>.
        ENDIF.
      ENDLOOP.
    ENDIF.
**********************************************************************
*** Search-/Selection citeria are chained (logical AND)
*** Delete where not all criteria are satisfied, but only if the criteria
*** is filled via selection screen in subscreen
    CASE strlen( iv_search_term ).
      WHEN 0.
        CASE lines( i_size[] ).
          WHEN 0.
          WHEN OTHERS.
            CASE lines( i_chdat[] ).
              WHEN 0.
              WHEN OTHERS.
                DELETE mr_matches->* WHERE NOT ( size EQ abap_true AND changed EQ abap_true ).
            ENDCASE.
        ENDCASE.
      WHEN OTHERS.
        CASE lines( i_size[] ).
          WHEN 0.
            CASE lines( i_chdat[] ).
              WHEN 0.
              WHEN OTHERS.
                DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND changed EQ abap_true ).
            ENDCASE.
          WHEN OTHERS.
            CASE lines( i_chdat[] ).
              WHEN 0.
                DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND size EQ abap_true ).
              WHEN OTHERS.
                DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND size EQ abap_true AND changed EQ abap_true ).
            ENDCASE.
*            DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND size EQ abap_true ).
        ENDCASE.
    ENDCASE.
  ENDMETHOD.
  METHOD delete_all_nodes.
    mo_alv_tree->delete_all_nodes( ).
  ENDMETHOD.
  METHOD merge_node_tables.
    TYPES: BEGIN OF ty_map_nodes,
             index    TYPE i,
             node_ref TYPE REF TO zcl_dir_cont=>tt_node_key_tm,
           END OF ty_map_nodes.
    FIELD-SYMBOLS: <nodes> TYPE zcl_dir_cont=>tt_node_key_tm.
    DATA: lt_all_nodes TYPE zcl_dir_cont=>tt_node_key_tm,
          lt_node_map  TYPE TABLE OF ty_map_nodes.
    DATA: lv_cntr TYPE i.
    DATA: lt_nodes_to_del TYPE RANGE OF tm_nodekey.
**********************************************************************
    LOOP AT it_ref ASSIGNING FIELD-SYMBOL(<ref>).
      IF lines( <ref>->* ) = 0. CONTINUE. ENDIF.
      lt_node_map = VALUE #( BASE lt_node_map ( index = sy-tabix node_ref = <ref> ) ).
    ENDLOOP.
    LOOP AT lt_node_map ASSIGNING FIELD-SYMBOL(<node_map>).
      lv_cntr = lv_cntr + 1.
      LOOP AT <node_map>-node_ref->* ASSIGNING FIELD-SYMBOL(<node>).
        CLEAR lt_nodes_to_del.
        LOOP AT lt_node_map ASSIGNING FIELD-SYMBOL(<node_check>) WHERE index <> lv_cntr.
          READ TABLE <node_check>-node_ref->* WITH KEY table_line = <node> TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            lt_nodes_to_del = VALUE #( BASE lt_nodes_to_del ( option = 'EQ' sign = 'I' low = <node> ) ).
          ENDIF.
        ENDLOOP.
        IF lt_nodes_to_del IS NOT INITIAL.
          DELETE <node_map>-node_ref->* WHERE table_line IN lt_nodes_to_del.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD register_events.
    DATA: lt_events TYPE cntl_simple_events.
    mo_alv_tree->get_registered_events(
      IMPORTING
            events     = lt_events           " simple_events
    ).
    APPEND VALUE #( eventid = cl_column_tree_model=>eventid_item_double_click ) TO lt_events.
    APPEND VALUE #( eventid = cl_column_tree_model=>eventid_node_double_click ) TO lt_events.
    mo_alv_tree->set_registered_events(
      EXPORTING
        events                    = lt_events                 " Eventtabelle
      EXCEPTIONS
        illegal_event_combination = 1                " ILLEGAL_EVENT_COMBINATION
        unknown_event             = 2                " "
        OTHERS                    = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_tree_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*** Handler Registrieren
    SET HANDLER me->handle_node_double_click FOR mo_alv_tree.
    SET HANDLER me->handle_item_double_click FOR mo_alv_tree.
  ENDMETHOD.
  METHOD register_events_toolb.
    TYPES: t_events TYPE STANDARD TABLE OF cntl_simple_event WITH DEFAULT KEY.
    DATA: lt_events TYPE t_events.
**********************************************************************
    lt_events = VALUE #( ( eventid = cl_gui_toolbar=>m_id_function_selected
                           appl_event = abap_true ) ).
    mo_toolbar->set_registered_events( lt_events ).
    SET HANDLER me->on_function_selected FOR mo_toolbar.
  ENDMETHOD.
  METHOD init_dd_objects.
    CREATE OBJECT mo_dd_tree.
    CREATE OBJECT mo_dd_text.
    mo_dd_tree->add(
      EXPORTING
        flavor          = 'FILE'           " Name der Klasse/Type
        dragsrc         = abap_true        " ? DragSource
        droptarget      = abap_false       " ? DropTarget
        effect          = cl_dragdrop=>copy " ? Move/Copy
*        effect_in_ctrl  = usedefaulteffect " ? Defaultverhalten f. DragDrop innerhalb des Ctrls
      EXCEPTIONS
        already_defined = 1                " Der angegebene Name ist bereits im Behavior enthalten
        obj_invalid     = 2                " Object wurde bereits über Destroy invalidiert
        OTHERS          = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_screen_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    mo_dd_text->add(
      EXPORTING
        flavor          = 'FILE'           " Name der Klasse/Type
        dragsrc         = abap_false        " ? DragSource
        droptarget      = abap_true       " ? DropTarget
        effect          = cl_dragdrop=>copy " ? Move/Copy
*        effect_in_ctrl  = usedefaulteffect " ? Defaultverhalten f. DragDrop innerhalb des Ctrls
      EXCEPTIONS
        already_defined = 1                " Der angegebene Name ist bereits im Behavior enthalten
        obj_invalid     = 2                " Object wurde bereits über Destroy invalidiert
        OTHERS          = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_screen_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
**********************************************************************
*** Handles generiern
    mo_dd_text->get_handle( IMPORTING handle = mv_handle_file ).  " Handle dieses Behaviors
    mo_dd_tree->get_handle( IMPORTING handle = mv_handle_tree ).  " Handle dieses Behaviors
  ENDMETHOD.
  METHOD set_te_edit_mode.
    mo_text_edit->set_readonly_mode(
      EXPORTING
        readonly_mode          = 0             " readonly mode; eq 0: OFF ; ne 0: ON
*      EXCEPTIONS
*        error_cntl_call_method = 1                " Error while setting readonly mode!
*        invalid_parameter      = 2                " INVALID_PARAMETER
*        others                 = 3
).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD set_ref_log.
    mr_log = ir_log.
  ENDMETHOD.
  METHOD disable_log.
    mv_log_disabled = abap_true.
  ENDMETHOD.
  METHOD log_info.
    FIELD-SYMBOLS: <log> TYPE char255.
    CHECK mv_log_disabled = abap_false.
    ASSIGN mr_log->* TO <log>.
**********************************************************************
    <log> = |{ sy-uzeit TIME = USER } -- INFO: { i_info }|.
  ENDMETHOD.
ENDCLASS.
