FUNCTION z_eps2_dir_get_listing.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IV_DIR_NAME) TYPE  EPS2FILNAM
*"     VALUE(FILE_MASK) TYPE  EPSF-EPSFILNAM DEFAULT SPACE
*"  EXPORTING
*"     VALUE(DIR_NAME) TYPE  EPSF-EPSDIRNAM
*"     VALUE(FILE_COUNTER) TYPE  EPSF-EPSFILSIZ
*"     VALUE(ERROR_COUNTER) TYPE  EPSF-EPSFILSIZ
*"     VALUE(E_DIR_LIST) TYPE  ZCL_DIR_CONT=>TTY_EPS2FILI
*"  EXCEPTIONS
*"      INVALID_EPS_SUBDIR
*"      SAPGPARAM_FAILED
*"      BUILD_DIRECTORY_FAILED
*"      NO_AUTHORIZATION
*"      READ_DIRECTORY_FAILED
*"      TOO_MANY_READ_ERRORS
*"      EMPTY_DIRECTORY_LIST
*"----------------------------------------------------------------------

  DATA: BEGIN OF file,
          dirname(75) TYPE c, " name of directory. (possibly truncated.)
          name        TYPE eps2filnam, " name of entry. (possibly truncated.)
          type(10)    TYPE c, " type of entry.
          len(8)      TYPE p, " length in bytes.
          owner(8)    TYPE c, " owner of the entry.
          mtime(6)    TYPE p, " last modification date, seconds since 1970
          mode(9)     TYPE c, " like "rwx-r-x--x": protection mode.
          errno(3)    TYPE c,
          errmsg(40)  TYPE c,
        END OF file.

  DATA: lv_eps_subdir LIKE epsf-epssubdir,
        lv_dir_name   LIKE epsf-epsdirnam,
        lv_dir_long   TYPE eps2filnam.

  DATA: dir_list TYPE TABLE OF zcl_dir_cont=>ty_eps2fili WITH HEADER LINE.

* authority check
  IF iv_dir_name(4) = gc_trans_dir.   " check TMS authority
    PERFORM check_trans_read_authority.
    IF sy-subrc <> 0.
      RAISE no_authorization.
    ENDIF.
  ELSE.
    PERFORM check_ftp_authority.
    IF sy-subrc <> 0.
      RAISE no_authorization.
    ENDIF.
  ENDIF.

* expand EPS subdirectory names
  lv_dir_long = iv_dir_name.
  IF iv_dir_name = space.                   " assume files in EPS/in
    lv_dir_long = gc_eps_in.
  ENDIF.
  IF lv_dir_long = gc_eps_in  OR            " get full directory name
     lv_dir_long = gc_eps_out OR
     lv_dir_long = gc_eps_log OR
     lv_dir_long(4) = gc_trans_dir.
    lv_eps_subdir = lv_dir_long.
    lv_dir_name = lv_dir_long.
    CALL FUNCTION 'EPS_GET_DIRECTORY_PATH'
      EXPORTING
        eps_subdir             = lv_eps_subdir
        dir_name               = lv_dir_name
      IMPORTING
        dir_name               = dir_name
      EXCEPTIONS
        invalid_eps_subdir     = 01
        sapgparam_failed       = 02
        build_directory_failed = 03.
    CASE sy-subrc.
      WHEN 01.
        RAISE invalid_eps_subdir.
      WHEN 02.
        RAISE sapgparam_failed.
      WHEN 03.
        RAISE build_directory_failed.
    ENDCASE.
    lv_dir_long = dir_name.
  ENDIF.

* get directory listing
  CALL 'C_DIR_READ_FINISH'                  " just to be sure
        ID 'ERRNO'  FIELD file-errno
        ID 'ERRMSG' FIELD file-errmsg.

  CALL 'C_DIR_READ_START'
        ID 'DIR'    FIELD lv_dir_long
        ID 'FILE'   FIELD file_mask
        ID 'ERRNO'  FIELD file-errno
        ID 'ERRMSG' FIELD file-errmsg.
  IF sy-subrc <> 0.
    RAISE read_directory_failed.
  ENDIF.

  REFRESH dir_list.
  CLEAR file_counter.
  CLEAR error_counter.
  DO.
    CLEAR file.
    CLEAR dir_list.
    CALL 'C_DIR_READ_NEXT'
          ID 'TYPE'   FIELD file-type
          ID 'NAME'   FIELD file-name
          ID 'LEN'    FIELD file-len
          ID 'OWNER'  FIELD file-owner
          ID 'MTIME'  FIELD file-mtime
          ID 'MODE'   FIELD file-mode
          ID 'ERRNO'  FIELD file-errno
          ID 'ERRMSG' FIELD file-errmsg.
    dir_list-size = file-len.
    dir_list-name = file-name.
    IF NOT file-name IS INITIAL.
*      data: fittingtime(8) type p.
*      fittingtime = file-mtime * 1000.
*      dir_list-mtim = cl_abap_tstmp=>normalize( fittingtime ).
      DATA: tstmp TYPE timestamp.
      CONVERT DATE '19700101' TIME '000000'
              INTO TIME STAMP tstmp TIME ZONE 'UTC   '.
      TRY.
          CALL METHOD cl_abap_tstmp=>add
            EXPORTING
              tstmp   = tstmp
              secs    = file-mtime
            RECEIVING
              r_tstmp = tstmp.
        CATCH cx_parameter_invalid_range.
          CLEAR tstmp.
        CATCH cx_root.
          CLEAR tstmp.
      ENDTRY.
      WRITE tstmp TIME ZONE sy-zonlo TO dir_list-mtim.
    ELSE.
      CLEAR dir_list-mtim.
    ENDIF.
    dir_list-owner = file-owner.
    IF sy-subrc = 0.
      IF file-type(1) = 'f' OR              " regular file
         file-type(1) = 'F' OR
         to_lower( file-type(1) ) = 'd'.
        ADD 1 TO file_counter.
        dir_list-rc   = 0.
        dir_list-is_dir = boolc( to_lower( file-type(1) ) = 'd' ).
        APPEND dir_list.
      ENDIF.
    ELSEIF sy-subrc = 1.
      EXIT.
    ELSE.
      IF error_counter > gc_1000.
        CALL 'C_DIR_READ_FINISH'
              ID 'ERRNO'  FIELD file-errno
              ID 'ERRMSG' FIELD file-errmsg.
        RAISE too_many_read_errors.
      ENDIF.
      ADD 1 TO error_counter.
      dir_list-rc  = 18.
      APPEND dir_list.
    ENDIF.
  ENDDO.

  CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD file-errno
        ID 'ERRMSG' FIELD file-errmsg.

  IF file_counter > 0.
    SORT dir_list BY name ASCENDING.
  ELSE.
    RAISE empty_directory_list.
  ENDIF.

  e_dir_list = dir_list[].

ENDFUNCTION.
