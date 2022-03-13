CLASS zcl_dir_cont DEFINITION PUBLIC ABSTRACT.
  PUBLIC SECTION.
**********************************************************************
*** Für cl_salv_tree
**********************************************************************
    TYPES: BEGIN OF ty_node_content,
             node_name TYPE string,
           END OF ty_node_content.
    TYPES: BEGIN OF ty_node_table,
             node_key        TYPE salv_de_node_key,
             entity          TYPE REF TO zcl_dir_cont,
             parent_node_key TYPE salv_de_node_key,
             is_complex      TYPE abap_bool,
           END OF ty_node_table.
    TYPES: tty_node_table TYPE HASHED TABLE OF ty_node_table WITH UNIQUE KEY node_key.
    TYPES: BEGIN OF ty_node_table_lvc,
             node_key        TYPE lvc_nkey,
             entity          TYPE REF TO zcl_dir_cont,
             parent_node_key TYPE lvc_nkey,
             highlighted     TYPE abap_bool,
             is_complex      TYPE abap_bool,
           END OF ty_node_table_lvc.
    TYPES: tty_node_table_lvc TYPE HASHED TABLE OF ty_node_table_lvc WITH UNIQUE KEY node_key.
**********************************************************************
*** Für Tree-Model
    TYPES: BEGIN OF ty_node_table_tmc,
             node_key        TYPE tm_nodekey,
             entity          TYPE REF TO zcl_dir_cont,
             parent_node_key TYPE tm_nodekey,
             highlighted     TYPE abap_bool,
             is_complex      TYPE abap_bool,
             vis_filtered    TYPE abap_bool,
           END OF ty_node_table_tmc.
    TYPES: tty_node_table_tmc TYPE HASHED TABLE OF ty_node_table_tmc WITH UNIQUE KEY node_key.
**********************************************************************
    TYPES: BEGIN OF ty_search_index,
             node_key    TYPE lvc_nkey,
             node_key_tm TYPE tm_nodekey,
             index       TYPE string,
             file_size   TYPE i,
             changed     TYPE dats,
           END OF ty_search_index.
    TYPES: tty_search_index TYPE SORTED TABLE OF ty_search_index WITH NON-UNIQUE KEY index.
    TYPES: tty_search_index_size TYPE SORTED TABLE OF ty_search_index WITH NON-UNIQUE KEY file_size.
    TYPES: tty_search_index_changed TYPE SORTED TABLE OF ty_search_index WITH NON-UNIQUE KEY changed.
    TYPES: tt_comp TYPE TABLE OF string.
    TYPES: tt_node_key TYPE TABLE OF lvc_nkey WITH DEFAULT KEY.
    TYPES: tt_node_key_tm TYPE TABLE OF tm_nodekey WITH DEFAULT KEY.
**********************************************************************
*** Type für Knotensuche
    TYPES: BEGIN OF t_node_search,
             node_key TYPE tm_nodekey,
             highl    TYPE abap_bool,
             size     TYPE abap_bool,
             changed  TYPE abap_bool,
             parent   TYPE abap_bool,
             child    TYPE abap_bool,
           END OF t_node_search.
    TYPES: tt_node_search TYPE HASHED TABLE OF t_node_search WITH UNIQUE KEY node_key.
    TYPES: tr_node_search TYPE REF TO tt_node_search.
**********************************************************************
    TYPES: BEGIN OF ty_eps2fili.
             INCLUDE TYPE eps2fili.
             TYPES: is_dir TYPE abap_bool,
           END OF ty_eps2fili.
    TYPES: tty_eps2fili TYPE TABLE OF ty_eps2fili WITH EMPTY KEY.
**********************************************************************

**********************************************************************
*** Für cl_gui_simple_tree
**********************************************************************
    TYPES: tt_file_alv TYPE TABLE OF zfile_exp_alv_tree WITH EMPTY KEY.
*    TYPES: tt_file_alv TYPE TABLE OF mtreesnode WITH EMPTY KEY.

**********************************************************************

    DATA: mv_alv_tree_node         TYPE salv_de_node_key READ-ONLY,
          mv_alv_tree_node_lvc     TYPE lvc_nkey READ-ONLY,
          mv_alv_tree_node_tmc     TYPE tm_nodekey READ-ONLY,
          mv_alv_tree_node_tmc_par TYPE tm_nodekey READ-ONLY,
          mv_node_name             TYPE string READ-ONLY,
          mv_size                  TYPE i READ-ONLY,
          mv_changed               TYPE dats READ-ONLY.
    DATA: mt_components TYPE TABLE OF string READ-ONLY.
    METHODS:
      constructor IMPORTING iv_base_path TYPE string,
      set_node_name IMPORTING iv_node_name TYPE string,
      set_size IMPORTING iv_size TYPE i,
      set_changed IMPORTING iv_changed TYPE dats,
*      get_components RETURNING VALUE(rt_comp) TYPE tt_comp,
*      add_to_node ABSTRACT IMPORTING io_salv_tree      TYPE REF TO cl_salv_tree
*                                     iv_parent_key     TYPE salv_de_node_key
*                                     io_ref_node_table TYPE REF TO tty_node_table OPTIONAL
*                           RAISING   cx_salv_msg,
*      add_to_node_alv_tree ABSTRACT IMPORTING io_alv_tree     TYPE REF TO cl_gui_alv_tree
*                                              iv_parent_key   TYPE lvc_nkey
*                                              ir_node_table   TYPE REF TO tty_node_table_lvc
*                                              ir_search_index TYPE REF TO tty_search_index,
*      add_to_node_alv_tree_fil ABSTRACT IMPORTING io_alv_tree     TYPE REF TO cl_gui_alv_tree
*                                                  iv_parent_key   TYPE lvc_nkey
*                                                  ir_node_table   TYPE REF TO tty_node_table_lvc
*                                                  ir_search_index TYPE REF TO tty_search_index
*                                                  ir_parent_nodes TYPE REF TO tt_node_key
*                                                  ir_highl_nodes  TYPE REF TO tt_node_key,
      build_node_table ABSTRACT IMPORTING ir_node_tab             TYPE REF TO treemcnota
                                          ir_item_tab             TYPE REF TO treemcitac
                                          iv_parent_key           TYPE tm_nodekey
                                          ir_current_key          TYPE REF TO tm_nodekey
                                          iv_dd_id                TYPE treemcnodt-dragdropid
                                          ir_node_table           TYPE REF TO tty_node_table_tmc
                                          ir_search_index         TYPE REF TO tty_search_index
                                          ir_search_index_size    TYPE REF TO tty_search_index_size
                                          ir_search_index_changed TYPE REF TO tty_search_index_changed,
      build_node_table_fil ABSTRACT IMPORTING ir_node_tab TYPE REF TO treemcnota
                                              ir_item_tab TYPE REF TO treemcitac
                                              iv_dd_id    TYPE treemcnodt-dragdropid
                                              ir_matches  TYPE REF TO tt_node_search
                                              ir_node_table           TYPE REF TO tty_node_table_tmc.
    DATA: mv_base_path TYPE string READ-ONLY.
ENDCLASS.



CLASS ZCL_DIR_CONT IMPLEMENTATION.


  METHOD constructor.
    mv_base_path = iv_base_path.
  ENDMETHOD.


  METHOD set_changed.
    mv_changed = iv_changed.
  ENDMETHOD.


  METHOD set_node_name.
    mv_node_name = iv_node_name.
  ENDMETHOD.


  METHOD set_size.
    mv_size = iv_size.
  ENDMETHOD.
ENDCLASS.
