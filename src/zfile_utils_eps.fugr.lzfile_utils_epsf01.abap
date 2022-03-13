*----------------------------------------------------------------------*
***INCLUDE LZFILE_UTILS_EPSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_TRANS_READ_AUTHORITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_trans_read_authority .
  CALL FUNCTION 'TR_AUTHORITY_CHECK_DISPLAY'
    EXCEPTIONS
      e_no_authority = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_FTP_AUTHORITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_ftp_authority .
  DATA: lv_mgr_user            TYPE as4user.

  CALL FUNCTION 'TMS_CI_GET_USER_INFO'
    IMPORTING
      ev_mgr_user = lv_mgr_user.
  CALL FUNCTION 'TR_AUTHORITY_CHECK_ADMIN'
    EXPORTING
      iv_user          = lv_mgr_user
      iv_adminfunction = 'EPS1'
    EXCEPTIONS
      OTHERS           = 1.
ENDFORM.
