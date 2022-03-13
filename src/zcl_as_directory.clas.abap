CLASS zcl_as_directory DEFINITION PUBLIC INHERITING FROM zcl_dir_cont CREATE PUBLIC .
  PUBLIC SECTION.
    TYPES: tt_content TYPE TABLE OF REF TO zcl_dir_cont WITH EMPTY KEY.
    METHODS:
      constructor
        IMPORTING io_dir_service TYPE REF TO zif_dir_service
                  iv_base_path TYPE string
                  iv_is_root TYPE abap_bool it_comp TYPE zcl_dir_cont=>tt_comp,
      read_dir_content IMPORTING iv_continue_if_err TYPE abap_bool RAISING zcx_file_err,
      get_content RETURNING VALUE(rt_content) TYPE tt_content,
      build_node_table REDEFINITION,
      build_node_table_fil REDEFINITION.
    METHODS:
      cnt_sub_selements RETURNING VALUE(rv_cnt) TYPE i.
**********************************************************************
    CLASS-METHODS:
      create_from_log_path
        IMPORTING i_logpath    TYPE pathintern
                  i_p1         TYPE string OPTIONAL
                  i_p2         TYPE string OPTIONAL
                  i_p3         TYPE string OPTIONAL
        RETURNING VALUE(r_dir) TYPE REF TO zcl_as_directory
        RAISING   zcx_file_err,
      create_from_phys_path
        IMPORTING i_physpath   TYPE string
        RETURNING VALUE(r_dir) TYPE REF TO zcl_as_directory
        RAISING   zcx_file_err.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_dir_cont.
*             INCLUDE TYPE salfldir.
             INCLUDE TYPE eps2fili.
             TYPES:   is_dir TYPE abap_bool,
           END OF ty_dir_cont.
    METHODS:
      read_directory_content IMPORTING iv_continue_if_err TYPE abap_bool RAISING zcx_file_err,
      read_sub_elements IMPORTING iv_continue_if_err TYPE abap_bool RAISING zcx_file_err,
      read_dir_cont_via_ext. "RAISING zcx_file_err.
    DATA: mo_fs_path    TYPE REF TO cl_fs_path.
    DATA: mt_content  TYPE TABLE OF REF TO zcl_dir_cont,
          mv_dir_name TYPE string.
    DATA: mt_file_list     TYPE zcl_dir_cont=>tty_eps2fili,
          mt_file_list_rzl TYPE TABLE OF ty_dir_cont.
    DATA: mt_err_list TYPE TABLE OF string.
**********************************************************************
    DATA: mv_cnt_subelements TYPE i,
          mv_is_root         TYPE abap_bool,
          mv_err_read        TYPE abap_bool.
**********************************************************************
*** Directory-Service -> reads content, is injectible for test-scenarios
    DATA: mo_directory_service TYPE REF TO zif_dir_service.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AS_DIRECTORY IMPLEMENTATION.


  METHOD build_node_table.
    FIELD-SYMBOLS: <nodetab_int>          TYPE treemcnota,
                   <itemtab_int>          TYPE treemcitac,
                   <search_index>         TYPE zcl_dir_cont=>tty_search_index,
                   <search_index_size>    TYPE zcl_dir_cont=>tty_search_index_size,
                   <search_index_changed> TYPE zcl_dir_cont=>tty_search_index_changed,
                   <current_nodekey>      TYPE tm_nodekey.
    FIELD-SYMBOLS: <nodetab_tmc> TYPE tty_node_table_tmc.
    DATA: lv_nkey_asint TYPE i.
**********************************************************************
    ASSIGN ir_current_key->* TO <current_nodekey>.
    lv_nkey_asint = CONV #( <current_nodekey> ).
    mv_alv_tree_node_tmc = |{ lv_nkey_asint + 1 }|.
    mv_alv_tree_node_tmc_par = iv_parent_key.
    <current_nodekey> = mv_alv_tree_node_tmc.
    ASSIGN ir_node_tab->* TO <nodetab_int>.
    <nodetab_int> = VALUE #( BASE <nodetab_int>
                              (
                                node_key = mv_alv_tree_node_tmc
                                relatkey = iv_parent_key
                                isfolder = abap_true
                                relatship = cl_tree_model=>relat_last_child
                                style = COND #( WHEN mv_err_read EQ abap_true
                                                  THEN cl_column_tree_model=>style_emphasized_negative
                                                  ELSE cl_column_tree_model=>style_default )
                              )
                           ).
    ASSIGN ir_item_tab->* TO <itemtab_int>.
    <itemtab_int> = VALUE #( BASE <itemtab_int>
                              (
                                item_name = 'NODE_NAME'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = COND #( WHEN iv_parent_key IS INITIAL THEN me->mv_base_path ELSE me->mv_dir_name )
                              )
                              (
                                item_name = 'SIZE'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = |{ mv_size NUMBER = USER }|
                              )
                              (
                                item_name = 'CHANGED'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = COND #( WHEN mv_changed IS NOT INITIAL THEN |{ mv_changed DATE = USER }| ELSE space )
                              )
                              (
                                item_name = 'CNT_SUBEL'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = |{ mv_cnt_subelements NUMBER = USER }|
                              )
                           ).
**********************************************************************
*** Node-Table
    ASSIGN ir_node_table->* TO <nodetab_tmc>.
    <nodetab_tmc> = VALUE #( BASE <nodetab_tmc>
                              (
                                node_key = mv_alv_tree_node_tmc
                                parent_node_key = iv_parent_key
                                entity = me
                                is_complex = abap_true
*                               highlighted     TYPE abap_bool,
                              )
                           ).
**** Suchindex!
    ASSIGN ir_search_index->* TO <search_index>.
    ASSIGN ir_search_index_changed->* TO <search_index_changed>.
    ASSIGN ir_search_index_size->* TO <search_index_size>.
*    INSERT VALUE #( index = |{ to_lower( mv_dir_name ) }| node_key = mv_alv_tree_node_lvc ) INTO <search_index>.
    <search_index> = VALUE #( BASE <search_index> ( index = |{ to_lower( mv_dir_name ) }| node_key_tm = mv_alv_tree_node_tmc ) ).
    <search_index_size> = VALUE #( BASE <search_index_size> ( file_size = mv_size node_key_tm = mv_alv_tree_node_tmc ) ).
    <search_index_changed> = VALUE #( BASE <search_index_changed> ( changed = mv_changed node_key_tm = mv_alv_tree_node_tmc ) ).

*** Alles Kind-Elemente einfüge
    LOOP AT me->mt_content ASSIGNING FIELD-SYMBOL(<cont>).
      <cont>->build_node_table(
        EXPORTING
          ir_node_tab     = ir_node_tab
          ir_item_tab     = ir_item_tab
          iv_parent_key   = mv_alv_tree_node_tmc
          ir_current_key  = ir_current_key
          iv_dd_id        = iv_dd_id
          ir_node_table   = ir_node_table
          ir_search_index = ir_search_index
          ir_search_index_size = ir_search_index_size
          ir_search_index_changed = ir_search_index_changed
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD build_node_table_fil.
    FIELD-SYMBOLS: <nodetab_int> TYPE treemcnota,
                   <itemtab_int> TYPE treemcitac.
    FIELD-SYMBOLS: <match>      TYPE t_node_search,
                   <node_table> TYPE zcl_dir_cont=>tty_node_table_tmc.
**********************************************************************
*** Check if Node is relevant after filtering!
    READ TABLE ir_matches->* WITH KEY node_key = mv_alv_tree_node_tmc ASSIGNING <match>.
    CHECK sy-subrc = 0.
**********************************************************************
    ASSIGN ir_node_tab->* TO <nodetab_int>.
    <nodetab_int> = VALUE #( BASE <nodetab_int>
                              (
                                node_key = mv_alv_tree_node_tmc
                                relatkey = mv_alv_tree_node_tmc_par
                                isfolder = abap_true
                                relatship = cl_tree_model=>relat_last_child
                                style = COND #( WHEN mv_err_read EQ abap_true
                                                  THEN cl_column_tree_model=>style_emphasized_negative
                                                  ELSE cl_column_tree_model=>style_default )
                              )
                           ).
    ASSIGN ir_item_tab->* TO <itemtab_int>.
    <itemtab_int> = VALUE #( BASE <itemtab_int>
                              (
                                item_name = 'NODE_NAME'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                style = COND #(
                                          WHEN <match>-highl EQ abap_true "line_exists( <highl_nodes>[ table_line = mv_alv_tree_node_tmc ] )
                                          THEN cl_column_tree_model=>style_emphasized_positive
                                          ELSE cl_column_tree_model=>style_default )
                                text = COND #( WHEN mv_alv_tree_node_tmc_par IS INITIAL THEN me->mv_base_path ELSE me->mv_dir_name )
                              )
                              (
                                item_name = 'SIZE'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                style = COND #(
                                          WHEN <match>-size EQ abap_true "line_exists( <size_nodes>[ table_line = mv_alv_tree_node_tmc ] )
                                          THEN cl_column_tree_model=>style_emphasized_a
                                          ELSE cl_column_tree_model=>style_default )
                                text = |{ mv_size NUMBER = USER }|
                              )
                              (
                                item_name = 'CHANGED'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                style = COND #(
                                          WHEN <match>-changed EQ abap_true "line_exists( <highl_nodes>[ table_line = mv_alv_tree_node_tmc ] )
                                          THEN cl_column_tree_model=>style_emphasized_a
                                          ELSE cl_column_tree_model=>style_default )
                                text = COND #( WHEN mv_changed IS NOT INITIAL THEN |{ mv_changed DATE = USER }| ELSE space )
                              )
                              (
                                item_name = 'CNT_SUBEL'
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                text = |{ mv_cnt_subelements NUMBER = USER }|
                              )
                           ).
    ASSIGN ir_node_table->* TO <node_table>.
    READ TABLE <node_table> WITH KEY node_key = mv_alv_tree_node_tmc ASSIGNING FIELD-SYMBOL(<node_entry>).
    <node_entry>-vis_filtered = abap_true.

*** Alles Kind-Elemente einfüge
    LOOP AT me->mt_content ASSIGNING FIELD-SYMBOL(<cont>).
      <cont>->build_node_table_fil(
        EXPORTING
          ir_node_tab     = ir_node_tab
          ir_item_tab     = ir_item_tab
          iv_dd_id        = iv_dd_id
          ir_matches      = ir_matches
          ir_node_table   = ir_node_table
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD cnt_sub_selements.
    DATA: lo_dir TYPE REF TO zcl_as_directory.
    LOOP AT mt_content ASSIGNING FIELD-SYMBOL(<cont>).
      TRY.
          lo_dir ?= <cont>.
*** +1 for folder
          ADD 1 TO mv_cnt_subelements.
          mv_cnt_subelements = mv_cnt_subelements + lo_dir->cnt_sub_selements( ).
        CATCH cx_sy_move_cast_error.
*** +1 for file
          ADD 1 TO mv_cnt_subelements.
      ENDTRY.
    ENDLOOP.
    rv_cnt = mv_cnt_subelements.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( iv_base_path ).
    mo_directory_service = io_dir_service.
    TRY.
        mo_fs_path = zcl_fs_path=>create_ext( name = iv_base_path
*                   force_absolute = abap_false
*                   path_kind      = ' '
                     ).
        mv_base_path = mo_fs_path->get_path_name( ).
        mt_components = it_comp.
        mv_dir_name = mo_fs_path->get_file_name( ).
        IF iv_is_root EQ abap_false.
          APPEND mv_dir_name TO mt_components.
        ENDIF.
        mv_is_root = iv_is_root.
      CATCH cx_smart_path_syntax.
    ENDTRY.
*    mv_dir_name = iv_dir_name.
  ENDMETHOD.


  METHOD create_from_log_path.
    DATA(log_path) = NEW zcl_log_path( i_logpath ).
    IF i_p1 IS NOT INITIAL.
      log_path->set_param1( i_p1 ).
    ENDIF.
    IF i_p2 IS NOT INITIAL.
      log_path->set_param2( i_p2 ).
    ENDIF.
    IF i_p3 IS NOT INITIAL.
      log_path->set_param3( i_p3 ).
    ENDIF.
    r_dir = NEW #( io_dir_service = NEW zcl_phys_dir_service( ) iv_base_path = log_path->mv_phys_path it_comp = VALUE #( ) iv_is_root = abap_true ).
    r_dir->read_dir_content( abap_true ).
  ENDMETHOD.


  METHOD create_from_phys_path.
    r_dir = NEW #( io_dir_service = NEW zcl_phys_dir_service( ) iv_base_path = i_physpath it_comp = VALUE #( ) iv_is_root = abap_true ).
    r_dir->read_dir_content( abap_true ).
  ENDMETHOD.


  METHOD get_content.
    rt_content = mt_content.
  ENDMETHOD.


  METHOD read_directory_content.
    DATA: lt_files     TYPE TABLE OF salfldir,
          lv_path_name TYPE string.
*    CALL FUNCTION 'RZL_READ_DIR'...
*    mt_file_list_rzl = CORRESPONDING #( lt_files ).
**********************************************************************
    me->read_dir_cont_via_ext( ).
**********************************************************************
    DELETE mt_file_list WHERE name = '..' OR name = '.'.
*    LOOP AT mt_file_list ASSIGNING FIELD-SYMBOL(<file>).
*      IF <file>-name EQ '.' OR <file>-name EQ '..'. CONTINUE. ENDIF.
*      lv_path_name = |{ mv_base_path }{ mo_fs_path->parse_separator_actual }{ <file>-name }|.
*      IF strlen( lv_path_name ) > 128. APPEND lv_path_name TO mt_err_list. ENDIF.
*      CALL FUNCTION 'PFL_CHECK_DIRECTORY'...                    = 6.
*      CASE sy-subrc.
*        WHEN 0.
*          <file>-is_dir = abap_true.
*        WHEN OTHERS.
*          <file>-is_dir = abap_false.
*      ENDCASE.
*    ENDLOOP.
  ENDMETHOD.


  METHOD read_dir_content.
    me->read_directory_content( iv_continue_if_err ).
    me->read_sub_elements( iv_continue_if_err ).
*    CATCH zcx_file_err. " zcx_file_err
  ENDMETHOD.


  METHOD read_dir_cont_via_ext.
    DATA: lv_dir_name      TYPE epsf-epsdirnam,
          lv_file_counter  TYPE epsf-epsfilsiz,
          lv_error_counter TYPE epsf-epsfilsiz.

    TRY.
        mo_directory_service->read_directory_content(
          EXPORTING
            iv_dir_name            = CONV eps2filnam( mv_base_path )
*       file_mask              = space
          IMPORTING
            dir_name               = lv_dir_name
            file_counter           = lv_file_counter
            error_counter          = lv_error_counter
            e_dir_list             = mt_file_list
        ).
      CATCH zcx_file_err. " zcx_file_err
        mv_err_read = abap_true.
    ENDTRY.
  ENDMETHOD.


  METHOD read_sub_elements.
    DATA: lo_sub_dir TYPE REF TO zcl_as_directory,
          lo_file    TYPE REF TO zcl_as_file.
    LOOP AT mt_file_list ASSIGNING FIELD-SYMBOL(<sub_dir>) WHERE is_dir EQ abap_true.
      lo_sub_dir = NEW #( io_dir_service = mo_directory_service iv_base_path = |{ mv_base_path }{ mo_fs_path->parse_separator_actual }{ <sub_dir>-name }| it_comp = mt_components iv_is_root = abap_false ).
      lo_sub_dir->read_dir_content( iv_continue_if_err ).
      lo_sub_dir->set_size( CONV #( <sub_dir>-size / 1000 ) ).
      lo_sub_dir->set_changed( CONV #( |{ <sub_dir>-mtim+6(4) }{ <sub_dir>-mtim+3(2) }{ <sub_dir>-mtim(2) }| ) ).
      APPEND lo_sub_dir TO mt_content.
    ENDLOOP.
    LOOP AT mt_file_list ASSIGNING FIELD-SYMBOL(<file>) WHERE is_dir = abap_false.
      lo_file = NEW #( iv_base_path = |{ mv_base_path }{ mo_fs_path->parse_separator_actual }{ <file>-name }| it_comp = mt_components ).
      lo_file->set_size( CONV #( <file>-size / 1000 ) ).
      lo_file->set_changed( CONV #( |{ <file>-mtim+6(4) }{ <file>-mtim+3(2) }{ <file>-mtim(2) }| ) ).
      APPEND lo_file TO mt_content.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
