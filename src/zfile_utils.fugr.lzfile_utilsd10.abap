*&---------------------------------------------------------------------*
*& Include          LZFILE_UTILSD10
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_screen_objects
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*CLASS lcl_screen_objects_col DEFINITION.
*  PUBLIC SECTION.
*    TYPES: ty_c30 TYPE c LENGTH 30.
*    TYPES: t_c_text TYPE TABLE OF char90 WITH DEFAULT KEY.
*    TYPES: ref_char_tab TYPE REF TO t_c_text.
*    TYPES: tt_ref_nodes TYPE TABLE OF REF TO zcl_dir_cont=>tt_node_key_tm.
*    METHODS:
*      constructor,
*      build_alv_tree RAISING lcx_screen_error,
*      delete_all_nodes RAISING lcx_screen_error,
*      move_cursor_down CHANGING c_curs_info TYPE lcl_match_cursor=>t_cursor_info RAISING lcx_screen_error,
*      move_cursor_up CHANGING c_curs_info TYPE lcl_match_cursor=>t_cursor_info RAISING lcx_screen_error,
*      handle_file_search IMPORTING iv_search_term TYPE string
*                                   so_size        TYPE t_range_size
*                                   so_chdat       TYPE t_range_chdat
*                         CHANGING  c_curs_info    TYPE lcl_match_cursor=>t_cursor_info
*                         RAISING   lcx_screen_error,
*      handle_file_search_fil IMPORTING iv_search_term TYPE string
*                                       so_size        TYPE t_range_size
*                                       so_chdat       TYPE t_range_chdat
*                             CHANGING  c_curs_info    TYPE lcl_match_cursor=>t_cursor_info
*                             RAISING   lcx_screen_error,
*      refresh_file IMPORTING iv_legacy_mode TYPE abap_bool RAISING zcx_file_err,
*      reset_tree CHANGING c_curs_info    TYPE lcl_match_cursor=>t_cursor_info RAISING lcx_screen_error,
*      reset_text_edit RAISING lcx_screen_error,
*      set_root IMPORTING io_root_dir TYPE REF TO zcl_as_directory,
*      set_dd_handler IMPORTING io_dd_handler TYPE REF TO lcl_dd_handler,
*      set_file_sel_allowed IMPORTING iv_file_sel_allowed TYPE abap_bool,
*      set_range_f_size IMPORTING ir_range TYPE REF TO t_range_size,
*      disable_log,
*      set_ref_log IMPORTING ir_log TYPE REF TO char255.
*    DATA: mv_alv_constructed TYPE abap_bool.
***********************************************************************
*    DATA: mr_range_size TYPE REF TO t_range_size.
**** Referenz auf ausgew. Pfad/File
*    DATA: mo_selected_entity        TYPE REF TO zcl_dir_cont READ-ONLY,
*          mv_file_txt_edit          TYPE string READ-ONLY,
*          mv_file_selection_allowed TYPE abap_bool.
**** Public Error Object
*    DATA: mo_exc TYPE REF TO cx_root READ-ONLY.
*  PROTECTED SECTION.
*
*    METHODS:
*      init_alv_tree RAISING lcx_screen_error,
*      init_text_edit RAISING lcx_screen_error,
*      set_te_read_only,
*      set_te_edit_mode,
*      init_container RAISING lcx_screen_error,
*      init_splitter RAISING lcx_screen_error,
*      init_dd_objects RAISING lcx_screen_error,
*      merge_node_tables IMPORTING it_ref TYPE tt_ref_nodes,
*      register_events RAISING lcx_screen_error.
*    METHODS:
*      add_root_node_to_tree RAISING lcx_screen_error,
*      add_nodes RAISING lcx_screen_error,
*      add_items RAISING lcx_screen_error,
*      reset_styles RAISING lcx_screen_error,
*      set_styles_from_matches RAISING lcx_screen_error,
*      expand_nodes RAISING lcx_screen_error.
*    METHODS:
*      find_all_child_nodes
*        IMPORTING
*          iv_node_key TYPE zcl_dir_cont=>ty_node_table_tmc-node_key
*          ir_matches  TYPE zcl_dir_cont=>tr_node_search,
*      expand_all_parent_node
*        IMPORTING
*          iv_node_key TYPE zcl_dir_cont=>ty_node_table_tmc-node_key
*          ir_matches  TYPE zcl_dir_cont=>tr_node_search.
***********************************************************************
*    METHODS:
*      log_info IMPORTING i_info TYPE string.
***********************************************************************
*    DATA: mr_node_table           TYPE REF TO zcl_dir_cont=>tt_file_alv,
*          mr_node_table_tmc       TYPE REF TO zcl_dir_cont=>tty_node_table_tmc,
*          mr_search_index         TYPE REF TO zcl_dir_cont=>tty_search_index,
*          mr_search_index_size    TYPE REF TO zcl_dir_cont=>tty_search_index_size,
*          mr_search_index_changed TYPE REF TO zcl_dir_cont=>tty_search_index_changed.
***********************************************************************
**** Event-Handler column_tree_model
*    METHODS:
*      handle_drag FOR EVENT drag OF cl_column_tree_model IMPORTING node_key drag_drop_object item_name,
*      handle_on_drop FOR EVENT on_drop OF cl_gui_textedit IMPORTING dragdrop_object.
**** Event-Handler
*    METHODS:
*      handle_node_double_click FOR EVENT node_double_click OF cl_column_tree_model IMPORTING node_key,
*      handle_item_double_click FOR EVENT item_double_click OF cl_column_tree_model IMPORTING node_key.
**    methods:
**      handle_double_click for event double_click of cl_column_tree_model IMPORTING
***********************************************************************
*    CONSTANTS: lc_contr_name TYPE string VALUE 'CUST_CONTR2'.
*    DATA: mo_cust_control     TYPE REF TO cl_gui_custom_container,
*          mo_splitter_control TYPE REF TO cl_gui_splitter_container,
*          mo_docking_left     TYPE REF TO cl_gui_docking_container,
*          mo_docking_right    TYPE REF TO cl_gui_docking_container,
*          mo_alv_tree         TYPE REF TO cl_column_tree_model,
*          mo_text_edit        TYPE REF TO cl_gui_textedit,
*          mo_dd_handler       TYPE REF TO lcl_dd_handler,
*          mr_log              TYPE REF TO char255.
***********************************************************************
*    DATA: mr_nodetab_int  TYPE REF TO treemcnota,
*          mr_itemtab_int  TYPE REF TO treemcitac,
*          mr_matches      TYPE REF TO zcl_dir_cont=>tt_node_search,
*          mr_curr_nodekey TYPE REF TO tm_nodekey.
***********************************************************************
**** Drag and Drop Functionality!
*    DATA: mo_dd_tree TYPE REF TO cl_dragdrop,
*          mo_dd_text TYPE REF TO cl_dragdrop.
*    DATA: mv_handle_tree TYPE i,
*          mv_handle_file TYPE i.
**** Flag, ob Einträge gefiltert wurden (zum zurücksetzen)
*    DATA: mv_filtered     TYPE abap_bool,
*          mv_log_disabled TYPE abap_bool.
**** Root Directory
*    DATA: mo_root TYPE REF TO zcl_as_directory.
**** Match-Cursor
*    DATA: mo_match_cursor TYPE REF TO lcl_match_cursor.
*ENDCLASS.
*
*CLASS lcl_screen_objects_col IMPLEMENTATION.
*  METHOD constructor.
*    mo_match_cursor = NEW #( ).
*  ENDMETHOD.
*  METHOD set_root.
*    mo_root = io_root_dir.
*  ENDMETHOD.
*  METHOD set_range_f_size.
*    mr_range_size = ir_range.
*  ENDMETHOD.
*  METHOD set_dd_handler.
*    mo_dd_handler = io_dd_handler.
*  ENDMETHOD.
*  METHOD set_file_sel_allowed.
*    mv_file_selection_allowed = iv_file_sel_allowed.
*  ENDMETHOD.
*  METHOD build_alv_tree.
*    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tt_file_alv.
*    IF mv_alv_constructed EQ abap_false.
*      me->init_container( ).
**      me->init_splitter( ).
*      me->init_dd_objects( ).
*      me->init_alv_tree( ).
*      me->init_text_edit( ).
**      me->set_te_read_only( ).
*      mr_node_table = NEW #( ).
*      ASSIGN mr_node_table->* TO <node_table>.
**** Flag seltzen -> Initialisierung erfolgt
*      mv_alv_constructed = abap_true.
*    ENDIF.
*    me->add_root_node_to_tree( ).
**    mo_alv_tree->frontend_update( ).
*  ENDMETHOD.
*  METHOD add_root_node_to_tree.
*    IF mr_node_table_tmc IS BOUND. FREE mr_node_table_tmc. ENDIF.
*    IF mr_search_index IS BOUND. FREE mr_search_index. ENDIF.
***********************************************************************
*    mr_node_table_tmc = NEW #( ).
*    mr_search_index = NEW #( ).
*    mr_search_index_changed = NEW #( ).
*    mr_search_index_size = NEW #( ).
*    mr_nodetab_int = NEW #( ).
*    mr_itemtab_int = NEW #( ).
*    mr_curr_nodekey = NEW #( ).
***********************************************************************
*    mo_root->build_node_table(
*      EXPORTING
*        ir_node_tab     = mr_nodetab_int
*        ir_item_tab     = mr_itemtab_int
*        iv_parent_key   = space
*        ir_current_key  = mr_curr_nodekey
*        iv_dd_id        = CONV #( mv_handle_tree )
*        ir_node_table   = mr_node_table_tmc
*        ir_search_index = mr_search_index
*        ir_search_index_changed = mr_search_index_changed
*        ir_search_index_size = mr_search_index_size
*    ).
*    me->add_nodes( ).
*    me->add_items( ).
*    CHECK lines( mr_nodetab_int->* ) GT 0.
**** Expand Root-Node
*    mo_alv_tree->expand_node( node_key = mr_nodetab_int->*[ 1 ]-node_key ).
**    CATCH lcx_screen_error.
*  ENDMETHOD.
*  METHOD reset_tree.
*    me->delete_all_nodes( ).
*    me->add_root_node_to_tree( ).
**    CATCH lcx_screen_error.
*    mo_match_cursor->clear_cursor_info(
*      CHANGING
*        c_cursor_info = c_curs_info
*    ).
*    me->log_info( |Resetting tree finished| ).
*  ENDMETHOD.
*  METHOD reset_text_edit.
*    DATA: lt_empty TYPE TABLE OF char1024.
**** leere Tabelle für leeres Text-Edit
*    lt_empty = VALUE #( ).
*    mo_text_edit->set_text_as_r3table(
*      EXPORTING
*        table           = lt_empty         " table with text
*      EXCEPTIONS
*        error_dp        = 1                " Error while sending R/3 table to TextEdit control!
*        error_dp_create = 2                " ERROR_DP_CREATE
*        OTHERS          = 3
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD add_nodes.
*    mo_alv_tree->add_nodes(
*      EXPORTING
*        node_table          = mr_nodetab_int->*     " Knoten-Tabelle
*      EXCEPTIONS
*        error_in_node_table = 1                " Knotentabelle fehlerhaft
*        OTHERS              = 2
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD add_items.
*    mo_alv_tree->add_items(
*     EXPORTING
*       item_table          = mr_itemtab_int->*     " Items
*     EXCEPTIONS
*       node_not_found      = 1                " Knotenschlüssel existiert nicht
*       error_in_item_table = 2                " ITEM_TABLE enthält einen fehlerhaften Eintrag
*       OTHERS              = 3
*   ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD reset_styles.
*    LOOP AT mr_node_table_tmc->* ASSIGNING FIELD-SYMBOL(<node_entry>) WHERE highlighted EQ abap_true.
*      mo_alv_tree->item_set_style(
*        EXPORTING
*          node_key       = <node_entry>-node_key   " Schlüssel des Knotens
*          item_name      = 'NODE_NAME'     " Name des Items
*          style          = cl_column_tree_model=>style_default " siehe Methodendokumentation
*      ).
*      mo_alv_tree->item_set_style(
*        EXPORTING
*          node_key       = <node_entry>-node_key   " Schlüssel des Knotens
*          item_name      = 'SIZE'     " Name des Items
*          style          = cl_column_tree_model=>style_default " siehe Methodendokumentation
*      ).
*    ENDLOOP.
*  ENDMETHOD.
*  METHOD set_styles_from_matches.
**** Node-Style setzen
*    LOOP AT mr_matches->* ASSIGNING FIELD-SYMBOL(<match>).
*      IF <match>-highl EQ abap_true.
*        mo_alv_tree->item_set_style(
*          EXPORTING
*            node_key       = <match>-node_key  " Schlüssel des Knotens
*            item_name      = 'NODE_NAME'     " Name des Items
*            style          = cl_column_tree_model=>style_emphasized_positive " siehe Methodendokumentation
*        ).
*      ENDIF.
*      IF <match>-size EQ abap_true.
*        mo_alv_tree->item_set_style(
*          EXPORTING
*            node_key       = <match>-node_key  " Schlüssel des Knotens
*            item_name      = 'SIZE'     " Name des Items
*            style          = cl_column_tree_model=>style_emphasized_a " siehe Methodendokumentation
*        ).
*      ENDIF.
*      IF <match>-changed EQ abap_true.
*        mo_alv_tree->item_set_style(
*          EXPORTING
*            node_key       = <match>-node_key  " Schlüssel des Knotens
*            item_name      = 'CHANGED'     " Name des Items
*            style          = cl_column_tree_model=>style_emphasized_a " siehe Methodendokumentation
*        ).
*      ENDIF.
*    ENDLOOP.
*  ENDMETHOD.
*  METHOD expand_nodes.
*    DATA: lt_nodes TYPE TABLE OF tm_nodekey.
*    LOOP AT mr_matches->* ASSIGNING FIELD-SYMBOL(<match>) WHERE parent = abap_true.
*      APPEND <match>-node_key TO lt_nodes.
*    ENDLOOP.
***********************************************************************
*    mo_alv_tree->expand_nodes(
*      EXPORTING
*        node_key_table          = lt_nodes           " Tabelle mit Knotenschlüsseln
*      EXCEPTIONS
*        error_in_node_key_table = 1                " unbekannter Knotenschlüssel in NODE_KEY_TABLE
*        OTHERS                  = 2
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD handle_node_double_click.
***********************************************************************
**** Knoten!
*    TYPES: BEGIN OF ty_node_el,
*             out_tab_line TYPE zfile_exp_alv_tree,
*             node_text    TYPE lvc_value,
*             item_layout  TYPE lvc_t_layi,
*             node_layout  TYPE  lvc_s_layn,
*           END OF ty_node_el.
*    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tty_node_table_tmc,
*                   <node_el>    TYPE zcl_dir_cont=>ty_node_table_tmc.
*    DATA: lo_file TYPE REF TO zcl_as_file.
*    DATA: ls_node        TYPE ty_node_el,
*          lt_item_layout TYPE lvc_t_laci.
*    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
*    READ TABLE <node_table> WITH KEY node_key = node_key ASSIGNING <node_el>.
***********************************************************************
*    CHECK sy-subrc = 0.
*    mo_selected_entity = <node_el>-entity.
*    TRY.
*        lo_file ?= mo_selected_entity.
*        IF mv_file_selection_allowed EQ abap_false.
*          MESSAGE 'File Selection not allowed! Please Select Directory' TYPE 'I'.
*          RETURN.
*        ENDIF.
*      CATCH cx_sy_move_cast_error.
*    ENDTRY.
*    cl_gui_cfw=>set_new_ok_code( EXPORTING new_code = 'SELECTED' ).
***********************************************************************
*  ENDMETHOD.
*  METHOD handle_item_double_click.
*    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tty_node_table_tmc,
*                   <node_el>    TYPE zcl_dir_cont=>ty_node_table_tmc.
*    DATA: lo_file TYPE REF TO zcl_as_file.
*    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
*    READ TABLE <node_table> WITH KEY node_key = node_key ASSIGNING <node_el>.
*    CHECK sy-subrc = 0.
*    mo_selected_entity = <node_el>-entity.
*    TRY.
*        lo_file ?= mo_selected_entity.
*        IF mv_file_selection_allowed EQ abap_false.
*          MESSAGE 'File Selection not allowed! Please Select Directory' TYPE 'I'.
*          RETURN.
*        ENDIF.
*      CATCH cx_sy_move_cast_error.
*    ENDTRY.
*    cl_gui_cfw=>set_new_ok_code( EXPORTING new_code = 'SELECTED' ).
*  ENDMETHOD.
*  METHOD handle_drag.
*    FIELD-SYMBOLS: <nodetab_tmc> TYPE zcl_dir_cont=>tty_node_table_tmc.
*    ASSIGN mr_node_table_tmc->* TO <nodetab_tmc>.
*    READ TABLE <nodetab_tmc> WITH KEY node_key = node_key ASSIGNING FIELD-SYMBOL(<node_entry>).
*    CHECK sy-subrc = 0.
*    me->set_te_edit_mode( ).
*    drag_drop_object->object = <node_entry>-entity.
**    BREAK-POINT.
*  ENDMETHOD.
*  METHOD move_cursor_down.
*    mo_match_cursor->go_down( CHANGING c_cursor_info = c_curs_info ).
*    mo_alv_tree->set_selected_node(
*      EXPORTING
*        node_key                   =  mo_match_cursor->mv_current_node " Schlüssel des Knotens
*      EXCEPTIONS
*        single_node_selection_only = 1                " nur bei Knoten-Einfachselektion erlaubt
*        node_not_found             = 2                " Knoten mit Schlüssel NODE_KEY nicht gefunden
*        OTHERS                     = 3
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD move_cursor_up.
*    mo_match_cursor->go_up( CHANGING c_cursor_info = c_curs_info ).
*    mo_alv_tree->set_selected_node(
*      EXPORTING
*        node_key                   =  mo_match_cursor->mv_current_node " Schlüssel des Knotens
*      EXCEPTIONS
*        single_node_selection_only = 1                " nur bei Knoten-Einfachselektion erlaubt
*        node_not_found             = 2                " Knoten mit Schlüssel NODE_KEY nicht gefunden
*        OTHERS                     = 3
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD handle_on_drop.
*    DATA: lo_file    TYPE REF TO zcl_as_file,
*          lv_line    TYPE string,
*          lt_content TYPE TABLE OF char1024.
***********************************************************************
*    TRY.
*        lo_file ?= dragdrop_object->object.
*        OPEN DATASET lo_file->mv_full_name FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
*        DO.
*          READ DATASET lo_file->mv_full_name INTO lv_line.
*          IF sy-subrc <> 0.
*            EXIT.
*          ELSE.
*            APPEND lv_line TO lt_content.
*          ENDIF.
*        ENDDO.
** Datei schließen
*        CLOSE DATASET lo_file->mv_full_name.
*        mo_text_edit->set_text_as_r3table( table = lt_content ).
*        mv_file_txt_edit = lo_file->mv_full_name.
**** Set new OK-Code -> Filename is Updated in Dynpro (PAI)
*        cl_gui_cfw=>set_new_ok_code( 'F_DROP_FIN' ).
*      CATCH cx_root INTO DATA(lo_err).
*        mo_exc = lo_err.
*        cl_gui_cfw=>set_new_ok_code( 'SHOW_ERR' ).
*    ENDTRY.
*  ENDMETHOD.
*  METHOD refresh_file.
*    DATA: lt_content TYPE TABLE OF char1024,
*          lv_line    TYPE string.
*    DATA: lv_enc      TYPE lcl_dd_handler=>t_char_255,
*          lv_cp       TYPE lcl_dd_handler=>t_char_255,
*          lv_linefeed TYPE lcl_dd_handler=>t_char_255.
***********************************************************************
*    TRY.
*        lv_enc = mo_dd_handler->get_encoding( ).
*      CATCH lcx_no_value_set.
*    ENDTRY.
*    TRY.
*        lv_cp = mo_dd_handler->get_codepage( ).
*      CATCH lcx_no_value_set.
*    ENDTRY.
*    TRY.
*        lv_linefeed = mo_dd_handler->get_linefeed( ).
*      CATCH lcx_no_value_set.
*    ENDTRY.
***********************************************************************
*    TRY.
*        CASE abap_true.
*          WHEN iv_legacy_mode.
**            CHECK lv_linefeed IS NOT INITIAL.
*            CASE lv_linefeed.
*              WHEN lcl_dd_handler=>c_linefeed-native.
*                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
*              WHEN lcl_dd_handler=>c_linefeed-smart.
*                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
*              WHEN lcl_dd_handler=>c_linefeed-unix.
*                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
*              WHEN lcl_dd_handler=>c_linefeed-windows.
*                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
*              WHEN OTHERS.
*                OPEN DATASET mv_file_txt_edit FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_cp IGNORING CONVERSION ERRORS.
*            ENDCASE.
***********************************************************************
*          WHEN OTHERS.
*            CASE lv_enc.
*              WHEN lcl_dd_handler=>c_encoding-default.
*                CASE lv_linefeed.
*                  WHEN lcl_dd_handler=>c_linefeed-native.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-smart.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-windows.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-unix.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
*                  WHEN OTHERS.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
*                ENDCASE.
*              WHEN lcl_dd_handler=>c_encoding-utf_8.
*                CASE lv_linefeed.
*                  WHEN lcl_dd_handler=>c_linefeed-native.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-smart.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-windows.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-unix.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
*                  WHEN OTHERS.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING UTF-8 IGNORING CONVERSION ERRORS.
*                ENDCASE.
*              WHEN lcl_dd_handler=>c_encoding-non_unicode.
*                CASE lv_linefeed.
*                  WHEN lcl_dd_handler=>c_linefeed-native.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-smart.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-windows.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-unix.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
*                  WHEN OTHERS.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING NON-UNICODE IGNORING CONVERSION ERRORS.
*                ENDCASE.
*              WHEN OTHERS.
*                CASE lv_linefeed.
*                  WHEN lcl_dd_handler=>c_linefeed-native.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH NATIVE LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-smart.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH SMART LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-windows.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH WINDOWS LINEFEED.
*                  WHEN lcl_dd_handler=>c_linefeed-unix.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS WITH UNIX LINEFEED.
*                  WHEN OTHERS.
*                    OPEN DATASET mv_file_txt_edit FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
*                ENDCASE.
*            ENDCASE.
*        ENDCASE.
*        DO.
*          READ DATASET mv_file_txt_edit INTO lv_line.
*          IF sy-subrc <> 0.
*            EXIT.
*          ELSE.
*            APPEND lv_line TO lt_content.
*          ENDIF.
*        ENDDO.
** Datei schließen
*        CLOSE DATASET mv_file_txt_edit.
*        mo_text_edit->set_text_as_r3table( table = lt_content ).
*        me->log_info( |Reload finished| ).
*      CATCH cx_root INTO DATA(lo_err).
**        DATA(lv_txt) = lo_err->get_text( ).
*        RAISE EXCEPTION TYPE zcx_file_err
*          EXPORTING
**           textid   =
**           previous =
*            mv_msgv1 = CONV #( lo_err->get_text( ) ).
*    ENDTRY.
*  ENDMETHOD.
*  METHOD expand_all_parent_node.
*    DATA: lv_parent_node_key TYPE zcl_dir_cont=>ty_node_table_lvc-node_key.
*    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tty_node_table_tmc,
*                   <node_entry> TYPE zcl_dir_cont=>ty_node_table_tmc.
*    FIELD-SYMBOLS: <matches> TYPE zcl_dir_cont=>tt_node_search,
*                   <match>   TYPE zcl_dir_cont=>t_node_search.
***********************************************************************
*    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
*    ASSIGN ir_matches->* TO <matches>.
***********************************************************************
*    READ TABLE <node_table> WITH KEY node_key = iv_node_key ASSIGNING <node_entry>.
**** Loop until root node is reached (parent_node_key = space)
*    WHILE <node_entry>-parent_node_key NE space.
*      lv_parent_node_key = <node_entry>-parent_node_key.
**** Achtung: <matches> ist hashed table
*      IF line_exists( <matches>[ node_key = lv_parent_node_key ] ).
*        READ TABLE <matches> WITH KEY node_key = lv_parent_node_key ASSIGNING <match>.
*        <match>-parent = abap_true.
*      ELSE.
*        INSERT VALUE #( node_key = lv_parent_node_key parent = abap_true ) INTO TABLE <matches>.
*      ENDIF.
*      READ TABLE <node_table> WITH KEY node_key = lv_parent_node_key ASSIGNING <node_entry>.
*    ENDWHILE.
*  ENDMETHOD.
*  METHOD find_all_child_nodes.
*    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tty_node_table_tmc,
*                   <node_entry> TYPE zcl_dir_cont=>ty_node_table_tmc.
*    FIELD-SYMBOLS: <matches> TYPE zcl_dir_cont=>tt_node_search,
*                   <match>   TYPE zcl_dir_cont=>t_node_search.
*    DATA: lo_dir TYPE REF TO zcl_as_directory.
***********************************************************************
*    ASSIGN me->mr_node_table_tmc->* TO <node_table>.
*    ASSIGN ir_matches->* TO <matches>.
*
**** Find all nodes where key = parent_node_key
*    LOOP AT <node_table> ASSIGNING <node_entry> WHERE parent_node_key = iv_node_key.
*      IF line_exists( <matches>[ node_key = <node_entry>-node_key ] ).
*        READ TABLE <matches> WITH KEY node_key = <node_entry>-node_key ASSIGNING <match>.
*        <match>-child = abap_true.
*      ELSE.
*        INSERT VALUE #( node_key = <node_entry>-node_key child = abap_true ) INTO TABLE <matches>.
*      ENDIF.
**** If node_entry is directory -> call find_all_child_nodes recursively!
*      TRY.
*          lo_dir ?= <node_entry>-entity.
*          me->find_all_child_nodes( iv_node_key = <node_entry>-node_key ir_matches = ir_matches ).
*        CATCH cx_sy_move_cast_error.
*      ENDTRY.
*    ENDLOOP.
*  ENDMETHOD.
*  METHOD handle_file_search.
***********************************************************************
**** Knoten!
*    TYPES: BEGIN OF ty_node_el,
*             out_tab_line TYPE zfile_exp_alv_tree,
*             node_text    TYPE lvc_value,
*             item_layout  TYPE lvc_t_layi,
*             node_layout  TYPE  lvc_s_layn,
*           END OF ty_node_el.
***********************************************************************
*    FIELD-SYMBOLS: <search_index>  TYPE zcl_dir_cont=>tty_search_index,
*                   <node_tab_line> TYPE zcl_dir_cont=>ty_node_table_tmc.
*    FIELD-SYMBOLS: <search_matches> TYPE zcl_dir_cont=>tt_node_search,
*                   <match>          TYPE zcl_dir_cont=>t_node_search.
*    DATA: ls_node        TYPE ty_node_el,
*          lt_item_layout TYPE lvc_t_laci.
***********************************************************************
*    DATA: lt_check_nodes TYPE TABLE OF tm_nodekey.
***********************************************************************
**** if tree was previously filtered via handle_file_search_fil,
**** the tree is rebuilt so that all elements are added and therefore
**** are available for highlighting
*    IF mv_filtered = abap_true.
*      me->delete_all_nodes( ).
*      me->add_root_node_to_tree( ).
*      mv_filtered = abap_false.
*    ENDIF.
**** Initial Styles zurücksetzen!
*    me->reset_styles( ).
**    CATCH lcx_screen_error.
*    mo_alv_tree->collapse_all_nodes( ).
*    mo_alv_tree->expand_node( node_key = mr_nodetab_int->*[ 1 ]-node_key ).
***********************************************************************
*    IF mr_matches IS NOT BOUND. mr_matches = NEW #( ). ENDIF.
*    ASSIGN mr_matches->* TO <search_matches>.
*    CLEAR <search_matches>.
***********************************************************************
*    ASSIGN me->mr_search_index->* TO <search_index>.
**    cl_gui_column_tree=>style_emphasized_positive
*    LOOP AT <search_index> ASSIGNING FIELD-SYMBOL(<search_item>).
*      IF iv_search_term IS NOT INITIAL AND matches( val = <search_item>-index regex = |.*{ iv_search_term }.*| ).
*        INSERT VALUE #( node_key = <search_item>-node_key_tm highl = abap_true ) INTO TABLE <search_matches>.
**        APPEND <search_item>-node_key_tm TO <nodes_to_high>.
*        READ TABLE mr_node_table_tmc->* WITH KEY node_key = <search_item>-node_key_tm ASSIGNING <node_tab_line>.
*        <node_tab_line>-highlighted = abap_true.
*        UNASSIGN <node_tab_line>.
*      ENDIF.
*    ENDLOOP.
*    IF so_size[] IS NOT INITIAL.
*      LOOP AT mr_search_index_size->* ASSIGNING FIELD-SYMBOL(<index_size>) WHERE file_size IN so_size.
*        IF NOT line_exists( <search_matches>[ node_key = <index_size>-node_key_tm ] ).
*          INSERT VALUE #( node_key = <index_size>-node_key_tm size = abap_true ) INTO TABLE <search_matches>.
*        ELSE.
*          READ TABLE <search_matches> WITH KEY node_key = <index_size>-node_key_tm ASSIGNING <match>.
*          <match>-size = abap_true.
*        ENDIF.
*        READ TABLE mr_node_table_tmc->* WITH KEY node_key = <index_size>-node_key_tm ASSIGNING <node_tab_line>.
*        <node_tab_line>-highlighted = abap_true.
*        UNASSIGN <node_tab_line>.
*      ENDLOOP.
*    ENDIF.
*    IF so_chdat[] IS NOT INITIAL.
*      LOOP AT mr_search_index_changed->* ASSIGNING FIELD-SYMBOL(<index_chdat>) WHERE changed IN so_chdat.
*        IF NOT line_exists( <search_matches>[ node_key = <index_chdat>-node_key_tm ] ).
*          INSERT VALUE #( node_key = <index_chdat>-node_key_tm changed = abap_true ) INTO TABLE <search_matches>.
*        ELSE.
*          READ TABLE <search_matches> WITH KEY node_key = <index_chdat>-node_key_tm ASSIGNING <match>.
*          <match>-changed = abap_true.
*        ENDIF.
*        READ TABLE mr_node_table_tmc->* WITH KEY node_key = <index_chdat>-node_key_tm ASSIGNING <node_tab_line>.
*        <node_tab_line>-highlighted = abap_true.
*        UNASSIGN <node_tab_line>.
*      ENDLOOP.
*    ENDIF.
***********************************************************************
**** Search-/Selection citeria are chained (logical AND)
**** Delete where not all criteria are satisfied, but only if the criteria
**** is filled via selection screen in subscreen
*    CASE strlen( iv_search_term ).
*      WHEN 0.
*        CASE lines( so_size[] ).
*          WHEN 0.
*          WHEN OTHERS.
*            CASE lines( so_chdat[] ).
*              WHEN 0.
*              WHEN OTHERS.
*                DELETE mr_matches->* WHERE NOT ( size EQ abap_true AND changed EQ abap_true ).
*            ENDCASE.
*        ENDCASE.
*      WHEN OTHERS.
*        CASE lines( so_size[] ).
*          WHEN 0.
*            CASE lines( so_chdat[] ).
*              WHEN 0.
*              WHEN OTHERS.
*                DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND changed EQ abap_true ).
*            ENDCASE.
*          WHEN OTHERS.
*            CASE lines( so_chdat[] ).
*              WHEN 0.
*                DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND size EQ abap_true ).
*              WHEN OTHERS.
*                DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND size EQ abap_true AND changed EQ abap_true ).
*            ENDCASE.
**            DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND size EQ abap_true ).
*        ENDCASE.
*    ENDCASE.
***********************************************************************
*    lt_check_nodes = VALUE #( FOR match IN mr_matches->* ( match-node_key ) ).
*    LOOP AT lt_check_nodes ASSIGNING FIELD-SYMBOL(<_node>).
*      me->expand_all_parent_node( iv_node_key = <_node> ir_matches = mr_matches ).
**      me->find_all_child_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
*    ENDLOOP.
*    me->set_styles_from_matches( ).
**CATCH lcx_screen_error.
*    me->expand_nodes( ).
**    CATCH lcx_screen_error.
*    IF lines( mr_matches->* ) GE 1.
*      mo_match_cursor->init_from_ref( EXPORTING ir_ref = mr_matches CHANGING c_cursor_info = c_curs_info ).
*      mo_alv_tree->set_selected_node( mo_match_cursor->mv_current_node ).
*    ELSE.
*      mo_match_cursor->set_empty_result( CHANGING c_cursor_info = c_curs_info ).
*    ENDIF.
***********************************************************************
*    cl_gui_cfw=>flush( ).
*  ENDMETHOD.
*  METHOD handle_file_search_fil.
*    FIELD-SYMBOLS: <search_index>      TYPE zcl_dir_cont=>tty_search_index,
*                   <search_index_size> TYPE zcl_dir_cont=>tty_search_index_size,
*                   <node_tab>          TYPE zcl_dir_cont=>tty_node_table_tmc,
*                   <node_tab_line>     TYPE zcl_dir_cont=>ty_node_table_tmc.
*    FIELD-SYMBOLS: <search_matches> TYPE zcl_dir_cont=>tt_node_search,
*                   <match>          TYPE zcl_dir_cont=>t_node_search.
*    DATA: lt_item_layout TYPE lvc_t_laci,
*          lt_check_nodes TYPE TABLE OF tm_nodekey.
***********************************************************************
**    IF iv_search_term IS INITIAL.
**      me->delete_all_nodes( ).
**      me->add_root_node_to_tree( ).
***      RETURN.
***      CATCH lcx_screen_error.
**    ENDIF.
***********************************************************************
*    IF mr_matches IS NOT BOUND. mr_matches = NEW #( ). ENDIF.
*    ASSIGN mr_matches->* TO <search_matches>.
*    CLEAR <search_matches>.
***********************************************************************
**    ASSIGN lr_node_to_highl->* TO <nodes_to_high>.
*    ASSIGN me->mr_search_index->* TO <search_index>.
*    LOOP AT <search_index> ASSIGNING FIELD-SYMBOL(<search_item>).
*      IF iv_search_term IS NOT INITIAL AND matches( val = <search_item>-index regex = |.*{ iv_search_term }.*| ).
*        INSERT VALUE #( node_key = <search_item>-node_key_tm highl = abap_true ) INTO TABLE <search_matches>.
*        READ TABLE mr_node_table_tmc->* WITH KEY node_key = <search_item>-node_key_tm ASSIGNING <node_tab_line>.
*        <node_tab_line>-highlighted = abap_true.
*        UNASSIGN <node_tab_line>.
*      ENDIF.
*    ENDLOOP.
**    ASSIGN mr_range_size->* TO <sel_size>.
*    IF so_size[] IS NOT INITIAL.
*      LOOP AT mr_search_index_size->* ASSIGNING FIELD-SYMBOL(<index_size>) WHERE file_size IN so_size.
*        IF NOT line_exists( <search_matches>[ node_key = <index_size>-node_key_tm ] ).
*          INSERT VALUE #( node_key = <index_size>-node_key_tm size = abap_true ) INTO TABLE <search_matches>.
*        ELSE.
*          READ TABLE <search_matches> WITH KEY node_key = <index_size>-node_key_tm ASSIGNING <match>.
*          <match>-size = abap_true.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*    IF so_chdat[] IS NOT INITIAL.
*      LOOP AT mr_search_index_changed->* ASSIGNING FIELD-SYMBOL(<index_chdat>) WHERE changed IN so_chdat.
*        IF NOT line_exists( <search_matches>[ node_key = <index_chdat>-node_key_tm ] ).
*          INSERT VALUE #( node_key = <index_chdat>-node_key_tm changed = abap_true ) INTO TABLE <search_matches>.
*        ELSE.
*          READ TABLE <search_matches> WITH KEY node_key = <index_chdat>-node_key_tm ASSIGNING <match>.
*          <match>-changed = abap_true.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
**    me->merge_node_tables( it_ref = VALUE #( ( lr_node_to_highl ) ( lr_node_size ) ) ).
***********************************************************************
**** Search-/Selection citeria are chained (logical AND)
**** Delete where not all criteria are satisfied, but only if the criteria
**** is filled via selection screen in subscreen
*    CASE strlen( iv_search_term ).
*      WHEN 0.
*        CASE lines( so_size[] ).
*          WHEN 0.
*          WHEN OTHERS.
*            CASE lines( so_chdat[] ).
*              WHEN 0.
*              WHEN OTHERS.
*                DELETE mr_matches->* WHERE NOT ( size EQ abap_true AND changed EQ abap_true ).
*            ENDCASE.
*        ENDCASE.
*      WHEN OTHERS.
*        CASE lines( so_size[] ).
*          WHEN 0.
*            CASE lines( so_chdat[] ).
*              WHEN 0.
*              WHEN OTHERS.
*                DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND changed EQ abap_true ).
*            ENDCASE.
*          WHEN OTHERS.
*            CASE lines( so_chdat[] ).
*              WHEN 0.
*                DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND size EQ abap_true ).
*              WHEN OTHERS.
*                DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND size EQ abap_true AND changed EQ abap_true ).
*            ENDCASE.
**            DELETE mr_matches->* WHERE NOT ( highl EQ abap_true AND size EQ abap_true ).
*        ENDCASE.
*    ENDCASE.
*    lt_check_nodes = VALUE #( FOR match IN mr_matches->* ( match-node_key ) ).
*    LOOP AT lt_check_nodes ASSIGNING FIELD-SYMBOL(<_node>).
*      me->expand_all_parent_node( iv_node_key = <_node> ir_matches = mr_matches ).
**      me->find_all_child_nodes( iv_node_key = <_node> ir_matches = mr_matches ).
*    ENDLOOP.
*    me->delete_all_nodes( ).
**    CATCH lcx_screen_error.
*    FREE: mr_nodetab_int, mr_itemtab_int.
*    mr_nodetab_int = NEW #( ).
*    mr_itemtab_int = NEW #( ).
*    mo_root->build_node_table_fil(
*      EXPORTING
*        ir_node_tab     = mr_nodetab_int
*        ir_item_tab     = mr_itemtab_int
*        iv_dd_id        = CONV #( mv_handle_tree )
*        ir_matches      = mr_matches
*    ).
*    me->add_nodes( ).
*    me->add_items( ).
*    me->expand_nodes( ).
**    CATCH lcx_screen_error.
*    IF lines( mr_matches->* ) GE 1.
*      mo_match_cursor->init_from_ref( EXPORTING ir_ref = mr_matches CHANGING c_cursor_info = c_curs_info ).
*      mo_alv_tree->set_selected_node( mo_match_cursor->mv_current_node ).
*    ELSE.
*      mo_match_cursor->set_empty_result( CHANGING c_cursor_info = c_curs_info ).
*    ENDIF.
*    mv_filtered = abap_true.
***********************************************************************
*    cl_gui_cfw=>flush( ).
*  ENDMETHOD.
*  METHOD delete_all_nodes.
*    mo_alv_tree->delete_all_nodes( ).
*  ENDMETHOD.
*  METHOD merge_node_tables.
*    TYPES: BEGIN OF ty_map_nodes,
*             index    TYPE i,
*             node_ref TYPE REF TO zcl_dir_cont=>tt_node_key_tm,
*           END OF ty_map_nodes.
*    FIELD-SYMBOLS: <nodes> TYPE zcl_dir_cont=>tt_node_key_tm.
*    DATA: lt_all_nodes TYPE zcl_dir_cont=>tt_node_key_tm,
*          lt_node_map  TYPE TABLE OF ty_map_nodes.
*    DATA: lv_cntr TYPE i.
*    DATA: lt_nodes_to_del TYPE RANGE OF tm_nodekey.
***********************************************************************
*    LOOP AT it_ref ASSIGNING FIELD-SYMBOL(<ref>).
*      IF lines( <ref>->* ) = 0. CONTINUE. ENDIF.
*      lt_node_map = VALUE #( BASE lt_node_map ( index = sy-tabix node_ref = <ref> ) ).
*    ENDLOOP.
*    LOOP AT lt_node_map ASSIGNING FIELD-SYMBOL(<node_map>).
*      lv_cntr = lv_cntr + 1.
*      LOOP AT <node_map>-node_ref->* ASSIGNING FIELD-SYMBOL(<node>).
*        CLEAR lt_nodes_to_del.
*        LOOP AT lt_node_map ASSIGNING FIELD-SYMBOL(<node_check>) WHERE index <> lv_cntr.
*          READ TABLE <node_check>-node_ref->* WITH KEY table_line = <node> TRANSPORTING NO FIELDS.
*          IF sy-subrc <> 0.
*            lt_nodes_to_del = VALUE #( BASE lt_nodes_to_del ( option = 'EQ' sign = 'I' low = <node> ) ).
*          ENDIF.
*        ENDLOOP.
*        IF lt_nodes_to_del IS NOT INITIAL.
*          DELETE <node_map>-node_ref->* WHERE table_line IN lt_nodes_to_del.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*  ENDMETHOD.
*  METHOD register_events.
*    DATA: lt_events TYPE cntl_simple_events.
*    mo_alv_tree->get_registered_events(
*      IMPORTING
*            events     = lt_events           " simple_events
*    ).
*    APPEND VALUE #( eventid = cl_column_tree_model=>eventid_item_double_click ) TO lt_events.
*    APPEND VALUE #( eventid = cl_column_tree_model=>eventid_node_double_click ) TO lt_events.
*    mo_alv_tree->set_registered_events(
*      EXPORTING
*        events                    = lt_events                 " Eventtabelle
*      EXCEPTIONS
*        illegal_event_combination = 1                " ILLEGAL_EVENT_COMBINATION
*        unknown_event             = 2                " "
*        OTHERS                    = 3
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
**** Handler Registrieren
*    SET HANDLER me->handle_node_double_click FOR mo_alv_tree.
*    SET HANDLER me->handle_item_double_click FOR mo_alv_tree.
*  ENDMETHOD.
*  METHOD init_container.
*    CREATE OBJECT mo_docking_right
*      EXPORTING
**       parent    =                  " Parent container
**       repid     =                  " Report to which this docking control is linked to
**       dynnr     =                  " Dynpro to which this docking control is linked to
*        side      = cl_gui_docking_container=>dock_at_right     " Side to which this control is docked to
*        extension = 530               " Extension of this  control
**       style     =                  " Windows style attributes applied to this docking container
**       lifetime  = lifetime_default " Lifetime
**       caption   =                  " Caption
**       metric    = 0                " Metric
*        ratio     = 62                 " Prozent des Dynpros: gewinnt gegen EXTENSION
*      EXCEPTIONS
*        OTHERS    = 6.
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*    CREATE OBJECT mo_docking_left
*      EXPORTING
*        side      = cl_gui_docking_container=>dock_at_bottom     " Side to which this control is docked to
*        extension = 200               " Extension of this  control
**       style     =                  " Windows style attributes applied to this docking container
**       lifetime  = lifetime_default " Lifetime
**       caption   =                  " Caption
**       metric    = 0                " Metric
*        ratio     = 60                 " Prozent des Dynpros: gewinnt gegen EXTENSION
*      EXCEPTIONS
*        OTHERS    = 6.
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD init_alv_tree.
*    CREATE OBJECT mo_alv_tree
*      EXPORTING
*        node_selection_mode   = cl_column_tree_model=>node_sel_mode_single                 " Knoten: Einfach- oder Mehrfachselektion
**       hide_selection        =                  " Sichtbarkeit der Selektion
*        hierarchy_column_name = 'NODE_NAME'      " Name der Spalte im Hierarchie-Bereich
*        item_selection        = abap_true         " Selektierbarkeit einzelner Items
*        hierarchy_header      = VALUE #(
*                                  t_image = icon_folder
*                                  heading = 'Element-Name'
*                                  tooltip = 'Element-Name'
*                                  width   = 40
*                                )
*      EXCEPTIONS
*        OTHERS                = 1.
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*    mo_alv_tree->add_column( name = 'SIZE' width = 20 header_text = 'Size(KB)' alignment = cl_column_tree_model=>align_right ).
*    mo_alv_tree->add_column( name = 'CHANGED' width = 20 header_text = 'Changed On' alignment = cl_column_tree_model=>align_right ).
*    mo_alv_tree->add_column( name = 'CNT_SUBEL' width = 20 header_text = 'Cnt Subelements' alignment = cl_column_tree_model=>align_right ).
*    mo_alv_tree->create_tree_control( parent = mo_docking_left ).
*    me->register_events( ).
**    CATCH lcx_screen_error.
*    SET HANDLER me->handle_drag FOR mo_alv_tree.
*  ENDMETHOD.
*  METHOD init_dd_objects.
*    CREATE OBJECT mo_dd_tree.
*    CREATE OBJECT mo_dd_text.
*    mo_dd_tree->add(
*      EXPORTING
*        flavor          = 'FILE'           " Name der Klasse/Type
*        dragsrc         = abap_true        " ? DragSource
*        droptarget      = abap_false       " ? DropTarget
*        effect          = cl_dragdrop=>copy " ? Move/Copy
**        effect_in_ctrl  = usedefaulteffect " ? Defaultverhalten f. DragDrop innerhalb des Ctrls
*      EXCEPTIONS
*        already_defined = 1                " Der angegebene Name ist bereits im Behavior enthalten
*        obj_invalid     = 2                " Object wurde bereits über Destroy invalidiert
*        OTHERS          = 3
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*    mo_dd_text->add(
*      EXPORTING
*        flavor          = 'FILE'           " Name der Klasse/Type
*        dragsrc         = abap_false        " ? DragSource
*        droptarget      = abap_true       " ? DropTarget
*        effect          = cl_dragdrop=>copy " ? Move/Copy
**        effect_in_ctrl  = usedefaulteffect " ? Defaultverhalten f. DragDrop innerhalb des Ctrls
*      EXCEPTIONS
*        already_defined = 1                " Der angegebene Name ist bereits im Behavior enthalten
*        obj_invalid     = 2                " Object wurde bereits über Destroy invalidiert
*        OTHERS          = 3
*    ).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
***********************************************************************
**** Handles generiern
*    mo_dd_text->get_handle( IMPORTING handle = mv_handle_file ).  " Handle dieses Behaviors
*    mo_dd_tree->get_handle( IMPORTING handle = mv_handle_tree ).  " Handle dieses Behaviors
*  ENDMETHOD.
*  METHOD init_splitter.
*  ENDMETHOD.
*  METHOD init_text_edit.
*    CREATE OBJECT mo_text_edit
*      EXPORTING
*        parent                 = mo_docking_right         " Parent-Container
*      EXCEPTIONS
*        error_cntl_create      = 1                        " Error while performing creation of TextEdit control!
*        error_cntl_init        = 2                        " Error while initializing TextEdit control!
*        error_cntl_link        = 3                        " Error while linking TextEdit control!
*        error_dp_create        = 4                        " Error while creating DataProvider control!
*        gui_type_not_supported = 5                        " This type of GUI is not supported!
*        OTHERS                 = 6.
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE lcx_screen_error
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*    mo_text_edit->set_dragdrop( mo_dd_text ).
*    SET HANDLER me->handle_on_drop FOR mo_text_edit.
*  ENDMETHOD.
*  METHOD set_te_read_only.
*    mo_text_edit->set_readonly_mode(
**      EXPORTING
**        readonly_mode          = true             " readonly mode; eq 0: OFF ; ne 0: ON
**      EXCEPTIONS
**        error_cntl_call_method = 1                " Error while setting readonly mode!
**        invalid_parameter      = 2                " INVALID_PARAMETER
**        others                 = 3
*    ).
*    IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD set_te_edit_mode.
*    mo_text_edit->set_readonly_mode(
*      EXPORTING
*        readonly_mode          = 0             " readonly mode; eq 0: OFF ; ne 0: ON
**      EXCEPTIONS
**        error_cntl_call_method = 1                " Error while setting readonly mode!
**        invalid_parameter      = 2                " INVALID_PARAMETER
**        others                 = 3
*).
*    IF sy-subrc <> 0.
**     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
*  METHOD set_ref_log.
*    mr_log = ir_log.
*  ENDMETHOD.
*  METHOD disable_log.
*    mv_log_disabled = abap_true.
*  ENDMETHOD.
*  METHOD log_info.
*    FIELD-SYMBOLS: <log> TYPE char255.
*    CHECK mv_log_disabled = abap_false.
*    ASSIGN mr_log->* TO <log>.
***********************************************************************
*    <log> = |{ sy-uzeit TIME = USER } -- INFO: { i_info }|.
*  ENDMETHOD.
*ENDCLASS.
