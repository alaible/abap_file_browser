CLASS zcl_mock_dir_service DEFINITION PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES: zif_dir_service.
    ALIASES read_directory_content FOR zif_dir_service~read_directory_content.
    METHODS: set_mock_data IMPORTING it_mock_data TYPE zif_dir_service=>tt_mock_dir_data.
  PROTECTED SECTION.
    DATA: mt_mock_data TYPE zif_dir_service=>tt_mock_dir_data.
ENDCLASS.



CLASS ZCL_MOCK_DIR_SERVICE IMPLEMENTATION.


  METHOD read_directory_content.
    IF line_exists( mt_mock_data[ i_dir_name = iv_dir_name ] ).
      e_dir_list = mt_mock_data[ i_dir_name = iv_dir_name ]-e_dir_list.
    ENDIF.
  ENDMETHOD.


  METHOD set_mock_data.
    mt_mock_data = it_mock_data.
  ENDMETHOD.
ENDCLASS.
