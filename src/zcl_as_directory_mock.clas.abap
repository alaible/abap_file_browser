CLASS zcl_as_directory_mock DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_as_directory.
  PUBLIC SECTION.
    METHODS:
      set_mock_ref IMPORTING ir_ref TYPE REF TO zif_dir_service=>tt_mock_dir_data.
    DATA: mr_mock_table TYPE REF TO zif_dir_service=>tt_mock_dir_data READ-ONLY.
  PROTECTED SECTION.
    METHODS:
      read_sub_elements REDEFINITION,
      read_dir_cont_via_ext REDEFINITION.
ENDCLASS.



CLASS ZCL_AS_DIRECTORY_MOCK IMPLEMENTATION.


  METHOD read_dir_cont_via_ext.
    DATA: lv_dir_name      TYPE epsf-epsdirnam,
          lv_file_counter  TYPE epsf-epsfilsiz,
          lv_error_counter TYPE epsf-epsfilsiz.
    FIELD-SYMBOLS: <mock_table> TYPE zif_dir_service=>tt_mock_dir_data.
    TRY.
        mo_directory_service->read_directory_content(
          EXPORTING
            iv_dir_name            = CONV eps2filnam( mv_base_path )
*       file_mask              = space
          IMPORTING
            dir_name               = lv_dir_name
            file_counter           = lv_file_counter
            error_counter          = lv_error_counter
            e_dir_list             = mt_file_list
        ).
        ASSIGN mr_mock_table->* TO <mock_table>.
        INSERT VALUE #( i_dir_name = mv_base_path dir_name = lv_dir_name e_dir_list = mt_file_list ) INTO TABLE <mock_table>.
      CATCH zcx_file_err. " zcx_file_err
        mv_err_read = abap_true.
    ENDTRY.
  ENDMETHOD.


  METHOD read_sub_elements.
    DATA: lo_sub_dir TYPE REF TO zcl_as_directory_mock,
          lo_file    TYPE REF TO zcl_as_file.
    LOOP AT mt_file_list ASSIGNING FIELD-SYMBOL(<sub_dir>) WHERE is_dir EQ abap_true.
      lo_sub_dir = NEW #( io_dir_service = mo_directory_service iv_base_path = |{ mv_base_path }{ mo_fs_path->parse_separator_actual }{ <sub_dir>-name }| it_comp = mt_components iv_is_root = abap_false ).
*** Referenz auf Tabelle durchreichen (->Durch Rekursion fortschreiben)
      lo_sub_dir->set_mock_ref( ir_ref = mr_mock_table ).
      lo_sub_dir->read_dir_content( iv_continue_if_err ).
      lo_sub_dir->set_size( CONV #( <sub_dir>-size / 1000 ) ).
      lo_sub_dir->set_changed( CONV #( |{ <sub_dir>-mtim+6(4) }{ <sub_dir>-mtim+3(2) }{ <sub_dir>-mtim(2) }| ) ).
      APPEND lo_sub_dir TO mt_content.
    ENDLOOP.
*** -> beim generieren der Mock-Daten ist das nicht notwendig!
*    LOOP AT mt_file_list ASSIGNING FIELD-SYMBOL(<file>) WHERE is_dir = abap_false.
*      lo_file = NEW #( iv_base_path = |{ mv_base_path }{ mo_fs_path->parse_separator_actual }{ <file>-name }| it_comp = mt_components ).
*      lo_file->set_size( CONV #( <file>-size / 1000 ) ).
*      lo_file->set_changed( CONV #( |{ <file>-mtim+6(4) }{ <file>-mtim+3(2) }{ <file>-mtim(2) }| ) ).
*      APPEND lo_file TO mt_content.
*    ENDLOOP.
  ENDMETHOD.


  METHOD set_mock_ref.
    mr_mock_table = ir_ref.
  ENDMETHOD.
ENDCLASS.
