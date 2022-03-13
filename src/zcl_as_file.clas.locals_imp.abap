*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltc_test_data DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      dir_mock_data     TYPE zif_dir_service=>tt_mock_dir_data,
      node_tab_compare  TYPE zcl_dir_cont=>tty_node_table_tmc,
      item_tab          TYPE treemcitac,
      node_tab          TYPE treemcnota,
      node_tab_filtered TYPE zcl_dir_cont=>tty_node_table_tmc,
      matches           TYPE zcl_dir_cont=>tt_node_search.
    CLASS-METHODS:
      class_constructor.
  PRIVATE SECTION.
    CLASS-METHODS:
      init_mock_data,
      init_node_tabl,
      init_item_tab,
      init_nod_tab,
      init_node_tab_filtered,
      init_matches.
ENDCLASS.

CLASS ltc_test_data IMPLEMENTATION.
  METHOD class_constructor.
    init_mock_data( ).
    init_node_tabl( ).
    init_item_tab( ).
    init_nod_tab( ).
    init_node_tab_filtered( ).
    init_matches( ).
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
                              node_key = `5`
*                              entity = REF_FOR::OBJ::{O:48*\CLASS=ZCL_AS_FILE}
                              parent_node_key = `4`
                              highlighted = ''
                              is_complex = ''
                              vis_filtered = ''
                          )
                        ).
  ENDMETHOD.
  METHOD init_item_tab.
    item_tab  = VALUE #(
                  (
                      node_key = `5`
                      item_name = 'NODE_NAME'
                      class = 2
                      font = 0
                      disabled = ''
                      editable = ''
                      hidden = ''
                      t_image = ''
                      chosen = ''
                      style = 0
                      txtisqinfo = ''
                      text = `testfile.txt`
                  )
                  (
                      node_key = `5`
                      item_name = 'SIZE'
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
                      node_key = `5`
                      item_name = 'CHANGED'
                      class = 2
                      font = 0
                      disabled = ''
                      editable = ''
                      hidden = ''
                      t_image = ''
                      chosen = ''
                      style = 0
                      txtisqinfo = ''
                      text = `16.02.2022`
                  )
                ).
  ENDMETHOD.
  METHOD init_nod_tab.
    node_tab  = VALUE #(
                   (
                       node_key = `5`
                       relatkey = `4`
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
*                       userobject = ref_for::obj::{o:initial}
                       itemsincom = ''
                   )
                 ).
  ENDMETHOD.
  METHOD init_matches.
    matches = VALUE #(
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
ENDCLASS.
