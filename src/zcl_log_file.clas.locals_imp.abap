*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltc_test_data DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
    CLASS-DATA:
      filedata   TYPE TABLE OF filenameci,
      path_data   TYPE TABLE OF path,
      opsys_data TYPE TABLE OF opsystem.
ENDCLASS.

CLASS ltc_test_data IMPLEMENTATION.
  METHOD class_constructor.
    filedata = VALUE #(
                 (
                     fileintern = 'ZTEST_LOG_FILE'
                     fileextern = '<PARAM_1>_file.<PARAM_2>'
                     fileformat = 'ASC'
                     appli_tr01 = ''
                     pathintern = 'ZTEST_BASE_PATH'
                 )
                 (
                     fileintern = 'ZTEST_LOG_FILE1'
                     fileextern = 'test_<PARAM_2>.file.<PARAM_3>.name'
                     fileformat = 'ASC'
                     appli_tr01 = ''
                     pathintern = 'ZTEST_BASE_PATH'
                 )
                 (
                     fileintern = 'ZTEST_LOG_FILE2'
                     fileextern = 'log.dump<PARAM_2>.file'
                     fileformat = ''
                     appli_tr01 = ''
                     pathintern = 'ZTEST_BASE_PATH2'
                 )
                 (
                     fileintern = 'Z_VAR_LOG_F'
                     fileextern = 'test.file'
                     fileformat = ''
                     appli_tr01 = ''
                     pathintern = 'Z_VAR_LOG'
                 )
               ).
    path_data = VALUE #(
                  (
                    pathintern = 'ZTEST_BASE_PATH'
                    filesys = 'UNIX'
                    pathextern = '/home/all/test/data/<FILENAME>'
                  )
                ).
    opsys_data  = VALUE #(
                    (
                        opsys = 'AIX'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'BOS/X'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'DOS'
                        filesys = 'DOS'
                    )
                    (
                        opsys = 'HP-UX'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'Linux'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'MC'
                        filesys = 'MACINTOSH'
                    )
                    (
                        opsys = 'MF'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'OS/400'
                        filesys = 'AS/400'
                    )
                    (
                        opsys = 'OS400'
                        filesys = 'AS/400'
                    )
                    (
                        opsys = 'OSF/1'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'OSF1'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'PM'
                        filesys = 'DOS'
                    )
                    (
                        opsys = 'Relia'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'ReliantUNI'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'SINIX'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'SunOS'
                        filesys = 'UNIX'
                    )
                    (
                        opsys = 'WN'
                        filesys = 'DOS'
                    )
                    (
                        opsys = 'WN32'
                        filesys = 'WINDOWS NT'
                    )
                    (
                        opsys = 'WN32_95'
                        filesys = 'WINDOWS NT'
                    )
                    (
                        opsys = 'WN32_98'
                        filesys = 'WINDOWS NT'
                    )
                    (
                        opsys = 'Windows 2K'
                        filesys = 'WINDOWS NT'
                    )
                    (
                        opsys = 'Windows NT'
                        filesys = 'WINDOWS NT'
                    )
                  ).
  ENDMETHOD.
ENDCLASS.
