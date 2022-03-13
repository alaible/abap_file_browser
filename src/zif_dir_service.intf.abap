INTERFACE zif_dir_service PUBLIC.
  TYPES: BEGIN OF t_mock_directory_data,
           i_dir_name    TYPE eps2filnam,
           dir_name      TYPE epsf-epsdirnam,
           file_counter  TYPE  epsf-epsfilsiz,
           error_counter TYPE  epsf-epsfilsiz,
           e_dir_list    TYPE  zcl_dir_cont=>tty_eps2fili,
         END OF t_mock_directory_data.
  TYPES: tt_mock_dir_data TYPE SORTED TABLE OF t_mock_directory_data WITH UNIQUE KEY i_dir_name.
  METHODS: read_directory_content IMPORTING iv_dir_name   TYPE  eps2filnam
                                            file_mask     TYPE  epsf-epsfilnam DEFAULT space
                                  EXPORTING dir_name      TYPE  epsf-epsdirnam
                                            file_counter  TYPE  epsf-epsfilsiz
                                            error_counter TYPE  epsf-epsfilsiz
                                            e_dir_list    TYPE  zcl_dir_cont=>tty_eps2fili
                                  RAISING   zcx_file_err.
ENDINTERFACE.
