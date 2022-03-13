*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltc_test_data DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
    CLASS-DATA: opsys_data TYPE TABLE OF opsystem,
                path_data  TYPE TABLE OF path.
ENDCLASS.

CLASS ltc_test_data IMPLEMENTATION.
  METHOD class_constructor.
    path_data = VALUE #(
                  (
                    pathintern = 'ZTEST_BASE_PATH'
                    filesys = 'UNIX'
                    pathextern = '/home/<SYSID>/test/data/<FILENAME>'
                  )
                  (
                    pathintern = 'ZTEST_BASE_PATH'
                    filesys = 'WINDOWS'
                    pathextern = 'C:\drive\<SYSID>\test\data\<FILENAME>'
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
