*&---------------------------------------------------------------------*
*& Report ZTEST_GENERATE_MOCK_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_generate_mock_data.

PARAMETERS: p_path TYPE string DEFAULT '/home/A4H/' LOWER CASE.

START-OF-SELECTION.
  DATA: lo_dir_mock    TYPE REF TO zcl_as_directory_mock,
        lo_dir_service TYPE REF TO zcl_phys_dir_service.
  DATA: lr_mock_data TYPE REF TO zif_dir_service=>tt_mock_dir_data.
  lr_mock_data = NEW #( ).
  lo_dir_service = NEW #( ).
  CREATE OBJECT lo_dir_mock
    EXPORTING
      io_dir_service = lo_dir_service
      iv_base_path   = p_path
      iv_is_root     = abap_true
      it_comp        = VALUE #( ).

  lo_dir_mock->set_mock_ref( ir_ref = lr_mock_data ).
  TRY.
      lo_dir_mock->read_dir_content( iv_continue_if_err = abap_true ).
    CATCH zcx_file_err. " zcx_file_err
  ENDTRY.

  BREAK-POINT.
