*&---------------------------------------------------------------------*
*& Report ZTEST_FILE_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_file_demo.

**********************************************************************
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (10) l_fil FOR FIELD p_fil.
PARAMETERS: p_fil    TYPE string LOWER CASE.
SELECTION-SCREEN: PUSHBUTTON 63(20) btn1 USER-COMMAND cmd_expand1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (10) l_lp FOR FIELD p_lp MODIF ID a1.
PARAMETERS p_lp TYPE pathintern MATCHCODE OBJECT z_sh_path DEFAULT 'ZTEST_BASE_PATH' MODIF ID a1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (10) l_fil2 FOR FIELD p_fil2.
PARAMETERS: p_fil2    TYPE string LOWER CASE.
SELECTION-SCREEN: PUSHBUTTON 63(20) btn2 USER-COMMAND cmd_expand2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (10) l_ep FOR FIELD p_ep MODIF ID a2.
PARAMETERS p_ep TYPE string DEFAULT '/usr/sap/' MODIF ID a2 LOWER CASE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.
**********************************************************************


DATA: num_vis TYPE TABLE OF c,
      ok_save TYPE sy-ucomm,
      err     TYPE REF TO cx_static_check.

INITIALIZATION.
  l_fil = 'Path'.
  l_lp = 'Log.Path'.
  l_fil2 = 'Path'.
  l_ep = 'Explicit Path'.
  btn1 = |{ icon_expand }Show Log-Path|.
  btn2 = |{ icon_expand }Show Expl-Path|.
  SELECT SINGLE FROM path
         FIELDS 'exists' AS exists
          WHERE pathintern = @p_lp
      INTO @DATA(exists).
  IF sy-subrc <> 0.
    CLEAR p_lp.
  ENDIF.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN INTO DATA(wa_screen).
    IF wa_screen-group1(1) = |A| AND NOT line_exists( num_vis[ table_line = wa_screen-group1+1(1) ] ).
      wa_screen-invisible = 1.
      wa_screen-active = 0.
    ELSEIF ok_save(10) = 'CMD_EXPAND'.
      DATA(num) = ok_save+10(1).
      IF wa_screen-group1 = |A{ num }|.
        wa_screen-invisible = 0.
        wa_screen-active = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN FROM wa_screen.
  ENDLOOP.


AT SELECTION-SCREEN.
* wenn Button-Kommando 'CMD_SHOW'
  IF sy-ucomm(10) = 'CMD_EXPAND'.
    IF NOT line_exists( num_vis[ table_line = sy-ucomm+10(1) ] ).
      APPEND sy-ucomm+10(1) TO num_vis.
    ENDIF.
    ok_save = sy-ucomm.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fil.
  TRY.
      DATA(as_dir) = zcl_as_directory=>create_from_log_path(
                       i_logpath = p_lp
                     ).
      CALL FUNCTION 'Z_SELECT_PATH_SIMPLE'
        EXPORTING
          io_base_path = as_dir            " Directory auf dem AS
        IMPORTING
          ev_fullpath  = p_fil
        EXCEPTIONS
          cancelled    = 1                " Auswahl abgebrochen
          OTHERS       = 2.
      IF sy-subrc = 1.
        MESSAGE 'cancelled' TYPE 'I'.
      ENDIF.
    CATCH zcx_file_err INTO err. " zcx_file_err
      MESSAGE |Error: { err->get_text( ) }| TYPE 'I'.
  ENDTRY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fil2.
  TRY.
      DATA(as_dir_expl) = zcl_as_directory=>create_from_phys_path(
                     i_physpath = p_ep
                   ).
      CALL FUNCTION 'Z_SELECT_PATH'
        EXPORTING
          io_base_path = as_dir_expl            " Directory auf dem AS
        IMPORTING
          ev_fullpath  = p_fil2
        EXCEPTIONS
          cancelled    = 1                " Auswahl abgebrochen
          OTHERS       = 2.
      IF sy-subrc = 1.
        MESSAGE 'cancelled' TYPE 'I'.
      ENDIF.
    CATCH zcx_file_err INTO err. " zcx_file_err
      MESSAGE |Error: { err->get_text( ) }| TYPE 'I'.
  ENDTRY.

START-OF-SELECTION.
  WRITE:/ |File1: { p_fil }|.
  WRITE:/ |File2: { p_fil2 }|.
*  DATA: line  TYPE string,
*        lines TYPE TABLE OF string.
*  OPEN DATASET p_fil FOR INPUT IN TEXT MODE ENCODING DEFAULT IGNORING CONVERSION ERRORS.
*  DO.
*    IF sy-subrc NE 0.
*      EXIT.
*    ENDIF.
*    READ DATASET p_fil INTO line.
*    APPEND line TO lines.
*  ENDDO.
*  LOOP AT lines ASSIGNING FIELD-SYMBOL(<line>).
*    WRITE:/ <line>.
*  ENDLOOP.
