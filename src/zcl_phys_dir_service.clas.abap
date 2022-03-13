CLASS zcl_phys_dir_service DEFINITION PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES: zif_dir_service.
ENDCLASS.



CLASS ZCL_PHYS_DIR_SERVICE IMPLEMENTATION.


  METHOD zif_dir_service~read_directory_content.
    CALL FUNCTION 'Z_EPS2_DIR_GET_LISTING'
      EXPORTING
        iv_dir_name            = iv_dir_name
*       file_mask              = space
      IMPORTING
        dir_name               = dir_name
        file_counter           = file_counter
        error_counter          = error_counter
        e_dir_list             = e_dir_list
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.
    IF sy-subrc <> 0.
*      mv_err_read = abap_true.
      RAISE EXCEPTION TYPE zcx_file_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
