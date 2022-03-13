*&---------------------------------------------------------------------*
*& Include          ZTEST_DIRECTORY_MOCKTD99
*&---------------------------------------------------------------------*
mock_data  = VALUE #(
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
