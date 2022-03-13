CLASS zcl_as_file DEFINITION INHERITING FROM zcl_dir_cont PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_base_path TYPE string it_comp TYPE zcl_dir_cont=>tt_comp,
      build_node_table REDEFINITION,
      build_node_table_fil REDEFINITION.
    DATA: mv_file_name TYPE string READ-ONLY,
          mv_full_name TYPE string READ-ONLY.
  PROTECTED SECTION.
    DATA: mo_fs_file TYPE REF TO cl_fs_path.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AS_FILE IMPLEMENTATION.


  METHOD build_node_table.
    FIELD-SYMBOLS: <nodetab_int>          TYPE treemcnota,
                   <itemtab_int>          TYPE treemcitac,
                   <search_index>         TYPE zcl_dir_cont=>tty_search_index,
                   <search_index_size>    TYPE zcl_dir_cont=>tty_search_index_size,
                   <search_index_changed> TYPE zcl_dir_cont=>tty_search_index_changed,
                   <current_nodekey>      TYPE tm_nodekey.
    FIELD-SYMBOLS: <nodetab_tmc> TYPE zcl_dir_cont=>tty_node_table_tmc.
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
                                isfolder = abap_false
                                relatship = cl_tree_model=>relat_last_child
                                dragdropid = iv_dd_id
                              )
                           ).
    ASSIGN ir_item_tab->* TO <itemtab_int>.
    <itemtab_int> = VALUE #( BASE <itemtab_int>
                              (
                                node_key = mv_alv_tree_node_tmc
                                class = cl_column_tree_model=>item_class_text
                                item_name = 'NODE_NAME'
                                text = CONV #( me->mv_file_name )
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
                           ).
**********************************************************************
*** Node-Table
    ASSIGN ir_node_table->* TO <nodetab_tmc>.
    <nodetab_tmc> = VALUE #( BASE <nodetab_tmc>
                              (
                                node_key = mv_alv_tree_node_tmc
                                parent_node_key = iv_parent_key
                                entity = me
*                               highlighted     TYPE abap_bool,
*                               is_complex      TYPE abap_bool,

                              )
                           ).
*** Suchindex!
    ASSIGN ir_search_index->* TO <search_index>.
    ASSIGN ir_search_index_changed->* TO <search_index_changed>.
    ASSIGN ir_search_index_size->* TO <search_index_size>.
*    INSERT VALUE #( index = |{ to_lower( mv_dir_name ) }| node_key = mv_alv_tree_node_lvc ) INTO <search_index>.
    <search_index> = VALUE #( BASE <search_index> ( index = |{ to_lower( mv_file_name ) }| node_key_tm = mv_alv_tree_node_tmc ) ).
    <search_index_size> = VALUE #( BASE <search_index_size> ( file_size = mv_size node_key_tm = mv_alv_tree_node_tmc ) ).
    <search_index_changed> = VALUE #( BASE <search_index_changed> ( changed = mv_changed node_key_tm = mv_alv_tree_node_tmc ) ).
  ENDMETHOD.


  METHOD build_node_table_fil.
    FIELD-SYMBOLS: <nodetab_int> TYPE treemcnota,
                   <itemtab_int> TYPE treemcitac,
                   <node_table>  TYPE zcl_dir_cont=>tty_node_table_tmc.
    FIELD-SYMBOLS: <parent_nodes> TYPE tt_node_key_tm,
                   <highl_nodes>  TYPE tt_node_key_tm,
                   <size_nodes>   TYPE tt_node_key_tm,
                   <child_nodes>  TYPE tt_node_key_tm,
                   <match>        TYPE t_node_search.
**********************************************************************
*    ASSIGN ir_parent_nodes->* TO <parent_nodes>.
*    ASSIGN ir_highl_nodes->* TO <highl_nodes>.
*    ASSIGN ir_child_nodes->* TO <child_nodes>.
*    ASSIGN ir_size_nodes->* to <size_nodes>.
**** Only add to node_table if node_key is contained in ir_highl_nodes
*    IF NOT line_exists( <highl_nodes>[ table_line = mv_alv_tree_node_tmc ] )
*      AND NOT line_exists( <child_nodes>[ table_line = mv_alv_tree_node_tmc ] )
*      AND NOT line_exists( <size_nodes>[ table_line = mv_alv_tree_node_tmc ] ).
*      RETURN.
*    ENDIF.

    READ TABLE ir_matches->* WITH KEY node_key = mv_alv_tree_node_tmc ASSIGNING <match>.

    CHECK sy-subrc = 0.

    ASSIGN ir_node_tab->* TO <nodetab_int>.
    <nodetab_int> = VALUE #( BASE <nodetab_int>
                              (
                                node_key = mv_alv_tree_node_tmc
                                relatkey = mv_alv_tree_node_tmc_par
                                isfolder = abap_false
                                relatship = cl_tree_model=>relat_last_child
                                dragdropid = iv_dd_id
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
                                text = mv_file_name
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
                                          WHEN <match>-changed EQ abap_true "line_exists( <size_nodes>[ table_line = mv_alv_tree_node_tmc ] )
                                          THEN cl_column_tree_model=>style_emphasized_a
                                          ELSE cl_column_tree_model=>style_default )
                                text = COND #( WHEN mv_changed IS NOT INITIAL THEN |{ mv_changed DATE = USER }| ELSE space )
                              )
                           ).
    ASSIGN ir_node_table->* TO <node_table>.
    READ TABLE <node_table> WITH KEY node_key = mv_alv_tree_node_tmc ASSIGNING FIELD-SYMBOL(<node_entry>).
    <node_entry>-vis_filtered = abap_true.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( iv_base_path ).
    mo_fs_file = cl_fs_path=>create( name = iv_base_path ).
    mv_base_path = mo_fs_file->get_path_component( ).
    mv_file_name = mo_fs_file->get_file_name( ).
    mt_components = it_comp.
*                 CATCH cx_smart_path_syntax.
    mv_full_name = |{ mv_base_path }{ mo_fs_file->parse_separator_actual }{ mv_file_name }|.
  ENDMETHOD.
ENDCLASS.
