CLASS ltc_log_path DEFINITION DEFERRED.
CLASS zcl_log_path DEFINITION LOCAL FRIENDS ltc_log_path.

CLASS ltc_log_path DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltc_Log_Path
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_LOG_PATH
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_log_path.  "class under test
    CLASS-DATA osql_test_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS: class_setup.
    METHODS:
      read_phys_path FOR TESTING,
      read_path_raw FOR TESTING.
    METHODS:
      init_cut_linux_base RAISING zcx_file_err.
ENDCLASS.       "ltc_Log_Path


CLASS ltc_log_path IMPLEMENTATION.

  METHOD class_setup.
    IF CONV i( sy-saprl ) < 752.
      cl_abap_unit_assert=>abort( msg = |Cannot perform unit test since sap release is below 752| ).
    ENDIF.

    osql_test_environment = cl_osql_test_environment=>create( VALUE #( ( 'PATH' ) ( 'OPSYSTEM' ) ) ).

    osql_test_environment->insert_test_data( ltc_test_data=>path_data ).
    osql_test_environment->insert_test_data( ltc_test_data=>opsys_data ).
  ENDMETHOD.


  METHOD read_phys_path.
    TRY.
        me->init_cut_linux_base( ).
        f_cut->read_phys_path( ).
*** Check for correct expansion
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = f_cut->mv_phys_path
            exp                  = |/home/{ sy-sysid }/test/data/|
        ).
      CATCH zcx_file_err INTO DATA(err). " zcx_file_err
        cl_abap_unit_assert=>fail( msg = |File Error in Testmethod read_phys_path: { err->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD read_path_raw.
    TRY.
        me->init_cut_linux_base( ).
        f_cut->read_path_raw( ).
*** Check for correct unexpanded path
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = f_cut->mv_path_raw
            exp                  = |/home/<SYSID>/test/data/<FILENAME>|
        ).
      CATCH zcx_file_err INTO DATA(err). " zcx_file_err
        cl_abap_unit_assert=>fail( msg = |File Error in Testmethod read_path_raw: { err->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD init_cut_linux_base.
    CREATE OBJECT f_cut
      EXPORTING
        iv_path  = 'ZTEST_BASE_PATH'
        iv_opsys = 'Linux'.
  ENDMETHOD.
ENDCLASS.
