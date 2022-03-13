CLASS ltc_as_directory DEFINITION DEFERRED.
CLASS zcl_as_directory DEFINITION LOCAL FRIENDS ltc_as_directory.

CLASS ltc_as_directory DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_As_Directory
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_AS_DIRECTORY
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_as_directory.  "class under test
    CLASS-DATA:
      mock_dir_service TYPE REF TO zcl_mock_dir_service.
    CLASS-METHODS: class_setup.
    METHODS: build_node_table_fil FOR TESTING RAISING zcx_file_err.
    METHODS: build_node_table FOR TESTING RAISING zcx_file_err.
    METHODS: read_directory_content FOR TESTING.
**********************************************************************
    METHODS:
      init_fcut_from_mock_data.
ENDCLASS.       "ltc_As_Directory


CLASS ltc_as_directory IMPLEMENTATION.

  METHOD class_setup.
*** setup mock_dir_service
    mock_dir_service = NEW #( ).
    mock_dir_service->set_mock_data( ltc_test_data=>dir_mock_data ).
  ENDMETHOD.
  METHOD build_node_table_fil.
    DATA: node_tab             TYPE REF TO treemcnota,
          item_tab             TYPE REF TO treemcitac,
          parent_key           TYPE tm_nodekey,
          current_key          TYPE REF TO tm_nodekey,
          dd_id                TYPE int2,
          node_table           TYPE REF TO zcl_dir_cont=>tty_node_table_tmc,
          search_index         TYPE REF TO zcl_dir_cont=>tty_search_index,
          search_index_size    TYPE REF TO zcl_dir_cont=>tty_search_index_size,
          search_index_changed TYPE REF TO zcl_dir_cont=>tty_search_index_changed..
**********************************************************************
    me->init_fcut_from_mock_data( ).
    f_cut->read_dir_content( iv_continue_if_err = abap_true ).
**********************************************************************
    node_tab = NEW #( ).
    item_tab = NEW #( ).
    current_key = NEW #( ).
    node_table = NEW #( ).
    search_index = NEW #( ).
    search_index_size = NEW #( ).
    search_index_changed = NEW #( ).
**********************************************************************
    TRY.
        f_cut->build_node_table(
            ir_node_tab = node_tab
            ir_item_tab = item_tab
            iv_parent_key = parent_key
            ir_current_key = current_key
            iv_dd_id = dd_id
            ir_node_table = node_table
            ir_search_index = search_index
            ir_search_index_size = search_index_size
            ir_search_index_changed = search_index_changed ).
**********************************************************************
        CLEAR: node_tab, item_tab.
        node_tab = NEW #( ).
        item_tab = NEW #( ).
        f_cut->build_node_table_fil(
          EXPORTING
            ir_node_tab   = node_tab
            ir_item_tab   = item_tab
            iv_dd_id      = dd_id
            ir_matches    = REF #( ltc_test_data=>matches )
            ir_node_table = node_table
        ).
        LOOP AT node_table->* ASSIGNING FIELD-SYMBOL(<node_entry>).
          CLEAR <node_entry>-entity.
        ENDLOOP.
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = node_table->*
            exp                  = ltc_test_data=>node_tab_filtered
        ).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = node_tab->*
            exp                  = ltc_test_data=>node_tab_fil
        ).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = item_tab->*
            exp                  = ltc_test_data=>item_tab_fil
        ).
      CATCH zcx_file_err INTO DATA(err).
    ENDTRY.
  ENDMETHOD.
  METHOD build_node_table.
    DATA: node_tab             TYPE REF TO treemcnota,
          item_tab             TYPE REF TO treemcitac,
          parent_key           TYPE tm_nodekey,
          current_key          TYPE REF TO tm_nodekey,
          dd_id                TYPE int2,
          node_table           TYPE REF TO zcl_dir_cont=>tty_node_table_tmc,
          search_index         TYPE REF TO zcl_dir_cont=>tty_search_index,
          search_index_size    TYPE REF TO zcl_dir_cont=>tty_search_index_size,
          search_index_changed TYPE REF TO zcl_dir_cont=>tty_search_index_changed.
**********************************************************************
    me->init_fcut_from_mock_data( ).
    f_cut->read_dir_content( iv_continue_if_err = abap_true ).
**********************************************************************
    node_tab = NEW #( ).
    item_tab = NEW #( ).
    current_key = NEW #( ).
    node_table = NEW #( ).
    search_index = NEW #( ).
    search_index_size = NEW #( ).
    search_index_changed = NEW #( ).
**********************************************************************
    TRY.
        f_cut->build_node_table(
            ir_node_tab = node_tab
            ir_item_tab = item_tab
            iv_parent_key = parent_key
            ir_current_key = current_key
            iv_dd_id = dd_id
            ir_node_table = node_table
            ir_search_index = search_index
            ir_search_index_size = search_index_size
            ir_search_index_changed = search_index_changed ).
      CATCH zcx_file_err INTO DATA(err).
    ENDTRY.
**********************************************************************
    LOOP AT node_table->* ASSIGNING FIELD-SYMBOL(<node_line>).
      CLEAR <node_line>-entity.
    ENDLOOP.
*    BREAK-POINT.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = node_table->*
        exp                  = ltc_test_data=>node_tab_compare
    ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = search_index->*
        exp                  = ltc_test_data=>search_index_compare
    ).
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  = search_index_size->*
*        exp                  = ltc_test_data=>search_index_size_cmp
*    ).
  ENDMETHOD.
  METHOD read_directory_content.
    DATA: sub_dir TYPE REF TO zcl_as_directory.
    TRY.
        me->init_fcut_from_mock_data( ).
        f_cut->read_dir_content( iv_continue_if_err = abap_true ).
        cl_abap_unit_assert=>assert_equals( act = lines( f_cut->mt_content ) exp = 2 ).
        cl_abap_unit_assert=>assert_equals( act = boolc( f_cut->mt_content[ 1 ] IS INSTANCE OF zcl_as_directory ) exp = abap_true ).
        cl_abap_unit_assert=>assert_equals( act = boolc( f_cut->mt_content[ 2 ] IS INSTANCE OF zcl_as_directory ) exp = abap_true ).
        sub_dir ?= f_cut->mt_content[ 1 ].
        cl_abap_unit_assert=>assert_equals( act = lines( sub_dir->mt_content ) exp = 4 ).
        sub_dir ?= f_cut->mt_content[ 2 ].
        cl_abap_unit_assert=>assert_equals( act = lines( sub_dir->mt_content ) exp = 2 ).
      CATCH zcx_file_err INTO DATA(err). " zcx_file_err
        cl_abap_unit_assert=>fail( msg = |Error in testmethod read_directory_content: { err->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.
  METHOD init_fcut_from_mock_data.
    CREATE OBJECT f_cut
      EXPORTING
        io_dir_service = mock_dir_service
        iv_base_path   = CONV #( ltc_test_data=>dir_mock_data[ 1 ]-i_dir_name )
        iv_is_root     = abap_true
        it_comp        = VALUE #( ).
  ENDMETHOD.
ENDCLASS.
