CLASS ltc_log_file DEFINITION DEFERRED.
CLASS zcl_log_file DEFINITION LOCAL FRIENDS ltc_log_file.

CLASS ltc_log_file DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Log_File
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_LOG_FILE
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
      f_cut TYPE REF TO zcl_log_file.  "class under test
    CLASS-DATA osql_test_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS:
      read_file_raw FOR TESTING,
      expand_file FOR TESTING.
    METHODS:
      init_cut_linux_base RAISING zcx_file_err.
ENDCLASS.       "ltc_Log_File


CLASS ltc_log_file IMPLEMENTATION.

  METHOD class_setup.
    osql_test_environment = cl_osql_test_environment=>create( VALUE #( ( 'PATH' ) ( 'OPSYSTEM' ) ( 'FILENAMECI' ) ) ).

    osql_test_environment->insert_test_data( ltc_test_data=>path_data ).
    osql_test_environment->insert_test_data( ltc_test_data=>opsys_data ).
    osql_test_environment->insert_test_data( ltc_test_data=>filedata ).
  ENDMETHOD.

  METHOD class_teardown.
  ENDMETHOD.

  METHOD read_file_raw.
    TRY.
        me->init_cut_linux_base( ).
        f_cut->read_file_raw(  ).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = f_cut->mv_path_raw
            exp                  = '/home/all/test/data/<FILENAME>'
        ).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = f_cut->mv_file_raw
            exp                  = '<PARAM_1>_file.<PARAM_2>'
        ).
      CATCH zcx_file_err INTO DATA(err). " zcx_file_err
        cl_abap_unit_assert=>fail( msg = |Error in test read_file_raw: { err->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD expand_file.
    TRY.
        me->init_cut_linux_base( ).
        f_cut->set_p1( 'para_1' ).
        f_cut->set_p2( 'para_2' ).
        f_cut->read_path_expanded_p( ).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = f_cut->mv_path_expanded
            exp                  = '/home/all/test/data/para_1_file.para_2'
        ).
      CATCH zcx_file_err INTO DATA(err). " zcx_file_err
        cl_abap_unit_assert=>fail( msg = |Error in test expand_file: { err->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD init_cut_linux_base.
    CREATE OBJECT f_cut
      EXPORTING
        iv_file  = 'ZTEST_LOG_FILE'
        iv_opsys = 'Linux'.
*    CATCH zcx_file_err. " zcx_file_err
  ENDMETHOD.
ENDCLASS.
