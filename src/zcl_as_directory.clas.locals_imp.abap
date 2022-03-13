*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltc_test_data DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      dir_mock_data         TYPE zif_dir_service=>tt_mock_dir_data,
      node_tab_compare      TYPE zcl_dir_cont=>tty_node_table_tmc,
      node_tab_filtered     TYPE zcl_dir_cont=>tty_node_table_tmc,
      search_index_compare  TYPE zcl_dir_cont=>tty_search_index,
      search_index_size_cmp TYPE zcl_dir_cont=>tty_search_index_size,
      node_tab_fil          TYPE treemcnota,
      item_tab_fil          TYPE treemcitac,
      matches               TYPE zcl_dir_cont=>tt_node_search.
    CLASS-METHODS:
      class_constructor.
  PRIVATE SECTION.
    CLASS-METHODS:
      init_mock_data,
      init_node_tabl,
      init_node_tab_filtered,
      init_search_index_compare,
      init_search_index_size_cmp,
      init_node_tab_fil,
      init_item_tab_fil,
      init_matches.
ENDCLASS.

CLASS ltc_test_data IMPLEMENTATION.
  METHOD class_constructor.
    init_mock_data( ).
    init_node_tabl( ).
    init_node_tab_filtered( ).
    init_search_index_compare( ).
    init_search_index_size_cmp( ).
    init_matches( ).
    init_node_tab_fil( ).
    init_item_tab_fil( ).
  ENDMETHOD.
  METHOD init_mock_data.
    dir_mock_data = VALUE #(
                      (
                          i_dir_name = '/home/all/data/testdir'
                          dir_name = ''
                          file_counter = 0
                          error_counter = 0
                          e_dir_list = VALUE #(
                                (
                                        name = '.'
                                        size = '4096'
                                        mtim = '16.02.2022 05:57:59'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = '..'
                                        size = '4096'
                                        mtim = '16.02.2022 05:56:47'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = 'subdir1'
                                        size = '4096'
                                        mtim = '16.02.2022 06:09:59'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = 'subdir2'
                                        size = '4096'
                                        mtim = '16.02.2022 06:16:32'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                          )
                      )
                      (
                          i_dir_name = '/home/all/data/testdir/subdir1'
                          dir_name = ''
                          file_counter = 0
                          error_counter = 0
                          e_dir_list = VALUE #(
                                (
                                        name = '.'
                                        size = '4096'
                                        mtim = '16.02.2022 06:09:59'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = '..'
                                        size = '4096'
                                        mtim = '16.02.2022 05:57:59'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = 'dir'
                                        size = '4096'
                                        mtim = '16.02.2022 06:09:59'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = 'subfolder1'
                                        size = '4096'
                                        mtim = '16.02.2022 06:10:18'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = 'testfile.txt'
                                        size = '0'
                                        mtim = '16.02.2022 06:09:43'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = ''
                                )
                                (
                                        name = 'testfile2.txt'
                                        size = '0'
                                        mtim = '16.02.2022 06:09:50'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = ''
                                )
                          )
                      )
                      (
                          i_dir_name = '/home/all/data/testdir/subdir1/dir'
                          dir_name = ''
                          file_counter = 0
                          error_counter = 0
                          e_dir_list = VALUE #(
                                (
                                        name = '.'
                                        size = '4096'
                                        mtim = '16.02.2022 06:09:59'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = '..'
                                        size = '4096'
                                        mtim = '16.02.2022 06:09:59'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                          )
                      )
                      (
                          i_dir_name = '/home/all/data/testdir/subdir1/subfolder1'
                          dir_name = ''
                          file_counter = 0
                          error_counter = 0
                          e_dir_list = VALUE #(
                                (
                                        name = '.'
                                        size = '4096'
                                        mtim = '16.02.2022 06:10:18'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = '..'
                                        size = '4096'
                                        mtim = '16.02.2022 06:09:59'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = 'dummy.txt'
                                        size = '0'
                                        mtim = '16.02.2022 06:10:06'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = ''
                                )
                                (
                                        name = 'longfile.txt'
                                        size = '0'
                                        mtim = '16.02.2022 06:10:18'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = ''
                                )
                          )
                      )
                      (
                          i_dir_name = '/home/all/data/testdir/subdir2'
                          dir_name = ''
                          file_counter = 0
                          error_counter = 0
                          e_dir_list = VALUE #(
                                (
                                        name = '.'
                                        size = '4096'
                                        mtim = '16.02.2022 06:16:32'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = '..'
                                        size = '4096'
                                        mtim = '16.02.2022 05:57:59'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = '.abapgit.xml'
                                        size = '289'
                                        mtim = '11.05.2021 20:17:06'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = ''
                                )
                                (
                                        name = 'test'
                                        size = '4096'
                                        mtim = '14.03.2021 16:03:37'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                          )
                      )
                      (
                          i_dir_name = '/home/all/data/testdir/subdir2/test'
                          dir_name = ''
                          file_counter = 0
                          error_counter = 0
                          e_dir_list = VALUE #(
                                (
                                        name = '.'
                                        size = '4096'
                                        mtim = '14.03.2021 16:03:37'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = '..'
                                        size = '4096'
                                        mtim = '16.02.2022 06:16:32'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = 'another_file.txt'
                                        size = '0'
                                        mtim = '14.03.2021 06:05:41'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = ''
                                )
                                (
                                        name = 'long_dir_name'
                                        size = '4096'
                                        mtim = '14.03.2021 16:03:44'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                          )
                      )
                      (
                          i_dir_name = '/home/all/data/testdir/subdir2/test/long_dir_name'
                          dir_name = ''
                          file_counter = 0
                          error_counter = 0
                          e_dir_list = VALUE #(
                                (
                                        name = '.'
                                        size = '4096'
                                        mtim = '14.03.2021 16:03:44'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = '..'
                                        size = '4096'
                                        mtim = '14.03.2021 16:03:37'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = 'even_longer'
                                        size = '4096'
                                        mtim = '23.05.2021 01:48:26'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                          )
                      )
                      (
                          i_dir_name = '/home/all/data/testdir/subdir2/test/long_dir_name/even_longer'
                          dir_name = ''
                          file_counter = 0
                          error_counter = 0
                          e_dir_list = VALUE #(
                                (
                                        name = '.'
                                        size = '4096'
                                        mtim = '23.05.2021 01:48:26'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = '..'
                                        size = '4096'
                                        mtim = '14.03.2021 16:03:44'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = 'X'
                                )
                                (
                                        name = 'dump.file'
                                        size = '45'
                                        mtim = '23.05.2021 01:48:26'
                                        owner = 'all'
                                        rc = '0000'
                                        is_dir = ''
                                )
                          )
                      )
                    ).
  ENDMETHOD.
  METHOD init_node_tabl.
    node_tab_compare  = VALUE #(
                           (
                               node_key = `1`
*                              entity = REF_FOR::OBJ::{O:36*\CLASS=ZCL_AS_DIRECTORY}
                               parent_node_key = ``
                               highlighted = ''
                               is_complex = 'X'
                               vis_filtered = ''
                           )
                           (
                               node_key = `2`
*                              entity = REF_FOR::OBJ::{O:38*\CLASS=ZCL_AS_DIRECTORY}
                               parent_node_key = `1`
                               highlighted = ''
                               is_complex = 'X'
                               vis_filtered = ''
                           )
                           (
                               node_key = `3`
*                              entity = REF_FOR::OBJ::{O:40*\CLASS=ZCL_AS_DIRECTORY}
                               parent_node_key = `2`
                               highlighted = ''
                               is_complex = 'X'
                               vis_filtered = ''
                           )
                           (
                               node_key = `4`
*                              entity = REF_FOR::OBJ::{O:42*\CLASS=ZCL_AS_DIRECTORY}
                               parent_node_key = `2`
                               highlighted = ''
                               is_complex = 'X'
                               vis_filtered = ''
                           )
                           (
                               node_key = `5`
*                              entity = REF_FOR::OBJ::{O:44*\CLASS=ZCL_AS_FILE}
                               parent_node_key = `4`
                               highlighted = ''
                               is_complex = ''
                               vis_filtered = ''
                           )
                           (
                               node_key = `6`
*                              entity = REF_FOR::OBJ::{O:46*\CLASS=ZCL_AS_FILE}
                               parent_node_key = `4`
                               highlighted = ''
                               is_complex = ''
                               vis_filtered = ''
                           )
                           (
                               node_key = `7`
*                              entity = REF_FOR::OBJ::{O:48*\CLASS=ZCL_AS_FILE}
                               parent_node_key = `2`
                               highlighted = ''
                               is_complex = ''
                               vis_filtered = ''
                           )
                           (
                               node_key = `8`
*                              entity = REF_FOR::OBJ::{O:50*\CLASS=ZCL_AS_FILE}
                               parent_node_key = `2`
                               highlighted = ''
                               is_complex = ''
                               vis_filtered = ''
                           )
                           (
                               node_key = `9`
*                              entity = REF_FOR::OBJ::{O:52*\CLASS=ZCL_AS_DIRECTORY}
                               parent_node_key = `1`
                               highlighted = ''
                               is_complex = 'X'
                               vis_filtered = ''
                           )
                           (
                               node_key = `10`
*                              entity = REF_FOR::OBJ::{O:54*\CLASS=ZCL_AS_DIRECTORY}
                               parent_node_key = `9`
                               highlighted = ''
                               is_complex = 'X'
                               vis_filtered = ''
                           )
                           (
                               node_key = `11`
*                              entity = REF_FOR::OBJ::{O:56*\CLASS=ZCL_AS_DIRECTORY}
                               parent_node_key = `10`
                               highlighted = ''
                               is_complex = 'X'
                               vis_filtered = ''
                           )
                           (
                               node_key = `12`
*                              entity = REF_FOR::OBJ::{O:58*\CLASS=ZCL_AS_DIRECTORY}
                               parent_node_key = `11`
                               highlighted = ''
                               is_complex = 'X'
                               vis_filtered = ''
                           )
                           (
                               node_key = `13`
*                              entity = REF_FOR::OBJ::{O:60*\CLASS=ZCL_AS_FILE}
                               parent_node_key = `12`
                               highlighted = ''
                               is_complex = ''
                               vis_filtered = ''
                           )
                           (
                               node_key = `14`
*                              entity = REF_FOR::OBJ::{O:62*\CLASS=ZCL_AS_FILE}
                               parent_node_key = `10`
                               highlighted = ''
                               is_complex = ''
                               vis_filtered = ''
                           )
                           (
                               node_key = `15`
*                              entity = REF_FOR::OBJ::{O:64*\CLASS=ZCL_AS_FILE}
                               parent_node_key = `9`
                               highlighted = ''
                               is_complex = ''
                               vis_filtered = ''
                           )
                          ).
  ENDMETHOD.
  METHOD init_search_index_compare.
    search_index_compare  = VALUE #(
                              (
                                  node_key = ''
                                  node_key_tm = `15`
                                  index = `.abapgit.xml`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `14`
                                  index = `another_file.txt`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `3`
                                  index = `dir`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `5`
                                  index = `dummy.txt`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `13`
                                  index = `dump.file`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `12`
                                  index = `even_longer`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `11`
                                  index = `long_dir_name`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `6`
                                  index = `longfile.txt`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `2`
                                  index = `subdir1`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `9`
                                  index = `subdir2`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `4`
                                  index = `subfolder1`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `10`
                                  index = `test`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `1`
                                  index = `testdir`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `7`
                                  index = `testfile.txt`
                                  file_size = 0
                                  changed = '00000000'
                              )
                              (
                                  node_key = ''
                                  node_key_tm = `8`
                                  index = `testfile2.txt`
                                  file_size = 0
                                  changed = '00000000'
                              )
                            ).
  ENDMETHOD.
  METHOD init_search_index_size_cmp.
    search_index_size_cmp  = VALUE #(
                                (
                                    node_key = ''
                                    node_key_tm = `15`
                                    index = ``
                                    file_size = 0
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `14`
                                    index = ``
                                    file_size = 0
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `13`
                                    index = ``
                                    file_size = 0
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `8`
                                    index = ``
                                    file_size = 0
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `7`
                                    index = ``
                                    file_size = 0
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `6`
                                    index = ``
                                    file_size = 0
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `5`
                                    index = ``
                                    file_size = 0
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `1`
                                    index = ``
                                    file_size = 0
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `12`
                                    index = ``
                                    file_size = 4
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `11`
                                    index = ``
                                    file_size = 4
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `10`
                                    index = ``
                                    file_size = 4
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `9`
                                    index = ``
                                    file_size = 4
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `4`
                                    index = ``
                                    file_size = 4
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `3`
                                    index = ``
                                    file_size = 4
                                    changed = '00000000'
                                )
                                (
                                    node_key = ''
                                    node_key_tm = `2`
                                    index = ``
                                    file_size = 4
                                    changed = '00000000'
                                )
                              ).
  ENDMETHOD.
  METHOD init_matches.
    matches  = VALUE #(
                  (
                      node_key = `14`
                      highl = 'X'
                      size = ''
                      changed = ''
                      parent = ''
                      child = ''
                  )
                  (
                      node_key = `13`
                      highl = 'X'
                      size = ''
                      changed = ''
                      parent = ''
                      child = ''
                  )
                  (
                      node_key = `6`
                      highl = 'X'
                      size = ''
                      changed = ''
                      parent = ''
                      child = ''
                  )
                  (
                      node_key = `7`
                      highl = 'X'
                      size = ''
                      changed = ''
                      parent = ''
                      child = ''
                  )
                  (
                      node_key = `8`
                      highl = 'X'
                      size = ''
                      changed = ''
                      parent = ''
                      child = ''
                  )
                  (
                      node_key = `10`
                      highl = ''
                      size = ''
                      changed = ''
                      parent = 'X'
                      child = ''
                  )
                  (
                      node_key = `9`
                      highl = ''
                      size = ''
                      changed = ''
                      parent = 'X'
                      child = ''
                  )
                  (
                      node_key = `1`
                      highl = ''
                      size = ''
                      changed = ''
                      parent = 'X'
                      child = ''
                  )
                ).
  ENDMETHOD.
  METHOD init_node_tab_filtered.
    node_tab_filtered = VALUE #(
                          (
                              node_key = `1`
*                              entity = REF_FOR::OBJ::{O:71*\CLASS=ZCL_AS_DIRECTORY}
                              parent_node_key = ``
                              highlighted = ''
                              is_complex = 'X'
                              vis_filtered = 'X'
                          )
                          (
                              node_key = `2`
*                              entity = REF_FOR::OBJ::{O:73*\CLASS=ZCL_AS_DIRECTORY}
                              parent_node_key = `1`
                              highlighted = ''
                              is_complex = 'X'
                              vis_filtered = ''
                          )
                          (
                              node_key = `3`
*                              entity = REF_FOR::OBJ::{O:75*\CLASS=ZCL_AS_DIRECTORY}
                              parent_node_key = `2`
                              highlighted = ''
                              is_complex = 'X'
                              vis_filtered = ''
                          )
                          (
                              node_key = `4`
*                              entity = REF_FOR::OBJ::{O:77*\CLASS=ZCL_AS_DIRECTORY}
                              parent_node_key = `2`
                              highlighted = ''
                              is_complex = 'X'
                              vis_filtered = ''
                          )
                          (
                              node_key = `5`
*                              entity = REF_FOR::OBJ::{O:79*\CLASS=ZCL_AS_FILE}
                              parent_node_key = `4`
                              highlighted = ''
                              is_complex = ''
                              vis_filtered = ''
                          )
                          (
                              node_key = `6`
*                              entity = REF_FOR::OBJ::{O:81*\CLASS=ZCL_AS_FILE}
                              parent_node_key = `4`
                              highlighted = ''
                              is_complex = ''
                              vis_filtered = ''
                          )
                          (
                              node_key = `7`
*                              entity = REF_FOR::OBJ::{O:83*\CLASS=ZCL_AS_FILE}
                              parent_node_key = `2`
                              highlighted = ''
                              is_complex = ''
                              vis_filtered = ''
                          )
                          (
                              node_key = `8`
*                              entity = REF_FOR::OBJ::{O:85*\CLASS=ZCL_AS_FILE}
                              parent_node_key = `2`
                              highlighted = ''
                              is_complex = ''
                              vis_filtered = ''
                          )
                          (
                              node_key = `9`
*                              entity = REF_FOR::OBJ::{O:87*\CLASS=ZCL_AS_DIRECTORY}
                              parent_node_key = `1`
                              highlighted = ''
                              is_complex = 'X'
                              vis_filtered = 'X'
                          )
                          (
                              node_key = `10`
*                              entity = REF_FOR::OBJ::{O:89*\CLASS=ZCL_AS_DIRECTORY}
                              parent_node_key = `9`
                              highlighted = ''
                              is_complex = 'X'
                              vis_filtered = 'X'
                          )
                          (
                              node_key = `11`
*                              entity = REF_FOR::OBJ::{O:91*\CLASS=ZCL_AS_DIRECTORY}
                              parent_node_key = `10`
                              highlighted = ''
                              is_complex = 'X'
                              vis_filtered = ''
                          )
                          (
                              node_key = `12`
*                              entity = REF_FOR::OBJ::{O:93*\CLASS=ZCL_AS_DIRECTORY}
                              parent_node_key = `11`
                              highlighted = ''
                              is_complex = 'X'
                              vis_filtered = ''
                          )
                          (
                              node_key = `13`
*                              entity = REF_FOR::OBJ::{O:95*\CLASS=ZCL_AS_FILE}
                              parent_node_key = `12`
                              highlighted = ''
                              is_complex = ''
                              vis_filtered = ''
                          )
                          (
                              node_key = `14`
*                              entity = REF_FOR::OBJ::{O:97*\CLASS=ZCL_AS_FILE}
                              parent_node_key = `10`
                              highlighted = ''
                              is_complex = ''
                              vis_filtered = 'X'
                          )
                          (
                              node_key = `15`
*                              entity = REF_FOR::OBJ::{O:99*\CLASS=ZCL_AS_FILE}
                              parent_node_key = `9`
                              highlighted = ''
                              is_complex = ''
                              vis_filtered = ''
                          )
                        ).
  ENDMETHOD.
  METHOD init_node_tab_fil.
    node_tab_fil  = VALUE #(
                      (
                          node_key = `1`
                          relatkey = ``
                          relatship = 1
                          hidden = ''
                          disabled = ''
                          isfolder = 'X'
                          n_image = ''
                          exp_image = ''
                          style = 1
                          no_branch = ''
                          expander = ''
                          dragdropid = '0 '
*                          userobject = REF_FOR::OBJ::{O:initial}
                          itemsincom = ''
                      )
                      (
                          node_key = `9`
                          relatkey = `1`
                          relatship = 1
                          hidden = ''
                          disabled = ''
                          isfolder = 'X'
                          n_image = ''
                          exp_image = ''
                          style = 1
                          no_branch = ''
                          expander = ''
                          dragdropid = '0 '
*                          userobject = REF_FOR::OBJ::{O:initial}
                          itemsincom = ''
                      )
                      (
                          node_key = `10`
                          relatkey = `9`
                          relatship = 1
                          hidden = ''
                          disabled = ''
                          isfolder = 'X'
                          n_image = ''
                          exp_image = ''
                          style = 1
                          no_branch = ''
                          expander = ''
                          dragdropid = '0 '
*                          userobject = REF_FOR::OBJ::{O:initial}
                          itemsincom = ''
                      )
                      (
                          node_key = `14`
                          relatkey = `10`
                          relatship = 1
                          hidden = ''
                          disabled = ''
                          isfolder = ''
                          n_image = ''
                          exp_image = ''
                          style = 0
                          no_branch = ''
                          expander = ''
                          dragdropid = '0 '
*                          userobject = REF_FOR::OBJ::{O:initial}
                          itemsincom = ''
                      )
                    ).
  ENDMETHOD.
  METHOD init_item_tab_fil.
    item_tab_fil = VALUE #(
                      (
                          node_key = `1`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `/home/all/data/testdir`
                      )
                      (
                          node_key = `1`
                          item_name = 'SIZE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `0`
                      )
                      (
                          node_key = `1`
                          item_name = 'CHANGED'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = ``
                      )
                      (
                          node_key = `1`
                          item_name = 'CNT_SUBEL'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `0`
                      )
                      (
                          node_key = `9`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `subdir2`
                      )
                      (
                          node_key = `9`
                          item_name = 'SIZE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `4`
                      )
                      (
                          node_key = `9`
                          item_name = 'CHANGED'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `16.02.2022`
                      )
                      (
                          node_key = `9`
                          item_name = 'CNT_SUBEL'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `0`
                      )
                      (
                          node_key = `10`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `test`
                      )
                      (
                          node_key = `10`
                          item_name = 'SIZE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `4`
                      )
                      (
                          node_key = `10`
                          item_name = 'CHANGED'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `14.03.2021`
                      )
                      (
                          node_key = `10`
                          item_name = 'CNT_SUBEL'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 0
                          txtisqinfo = ''
                          text = `0`
                      )
                      (
                          node_key = `14`
                          item_name = 'NODE_NAME'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 6
                          txtisqinfo = ''
                          text = `another_file.txt`
                      )
                      (
                          node_key = `14`
                          item_name = 'SIZE'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `0`
                      )
                      (
                          node_key = `14`
                          item_name = 'CHANGED'
                          class = 2
                          font = 0
                          disabled = ''
                          editable = ''
                          hidden = ''
                          t_image = ''
                          chosen = ''
                          style = 1
                          txtisqinfo = ''
                          text = `14.03.2021`
                      )
                    ).
  ENDMETHOD.
ENDCLASS.
