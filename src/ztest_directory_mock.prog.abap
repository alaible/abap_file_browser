*&---------------------------------------------------------------------*
*& Report ZTEST_DIRECTORY_MOCK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_directory_mock.
DATA: mock_data        TYPE zif_dir_service=>tt_mock_dir_data.
INCLUDE ztest_directory_mocktd99.
*PARAMETERS: p_path TYPE string DEFAULT '/home/A4H/' LOWER CASE.

START-OF-SELECTION.
  DATA: lo_mock_dir_service TYPE REF TO zcl_mock_dir_service,
        lo_as_directory     TYPE REF TO zcl_as_directory.
  DATA: base_path TYPE string.

  base_path = VALUE #( mock_data[ 1 ]-i_dir_name OPTIONAL ).

  IF base_path IS INITIAL.
    MESSAGE 'no basepath found type' TYPE 'E'.
  ENDIF.

  lo_mock_dir_service = NEW #( ).
  lo_mock_dir_service->set_mock_data( mock_data ).

  CREATE OBJECT lo_as_directory
    EXPORTING
      io_dir_service = lo_mock_dir_service
      iv_base_path   = CONV #( base_path )
      iv_is_root     = abap_true
      it_comp        = VALUE #( ).

  lo_as_directory->read_dir_content( iv_continue_if_err = abap_true ).

*    CATCH zcx_file_err. " zcx_file_err
  CALL FUNCTION 'Z_SELECT_PATH_SIMPLE'
    EXPORTING
      io_base_path = lo_as_directory   " Directory auf dem AS
    EXCEPTIONS
      cancelled    = 1                " Auswahl abgebrochen
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Abbort' TYPE 'I'.
  ENDIF.
