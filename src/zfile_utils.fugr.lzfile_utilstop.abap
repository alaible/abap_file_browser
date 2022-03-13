FUNCTION-POOL zfile_utils.                  "MESSAGE-ID ..

* INCLUDE LZFILE_UTILSD...                   " Local class definition
DATA: gv_filesize TYPE i.

TYPES: t_range_size  LIKE RANGE OF gv_filesize,
       t_range_chdat TYPE RANGE OF dats.

INCLUDE lzfile_utilsx01. "local exception classes
*INCLUDE lzfile_utilsd01.
*INCLUDE lzfile_utilsd02.
INCLUDE lzfile_utilsd05. "lcl_match_cursor
INCLUDE lzfile_utilsd07. "lcl_dd_handler
INCLUDE lzfile_utilsd08. "lcl_splitter_base
INCLUDE lzfile_utilsd09. "lcl_screen_base
INCLUDE lzfile_utilsd04. "lcl_screen_objects_col
INCLUDE lzfile_utilsd10. "lcl_screen_objects_col(backup)
INCLUDE lzfile_utilsd06. "lcl_screen_objects_col_simple
INCLUDE lzfile_utilsd03. "lcl_screen_state

*** OK-Code
DATA: ok_code TYPE sy-ucomm.
DATA: go_screen_state TYPE REF TO lcl_screen_state.

DATA: file_search TYPE c LENGTH 100,
      filter_res  TYPE c.

DATA: gv_owner   TYPE c LENGTH 50,
      gv_changed TYPE dats.

DATA: go_err TYPE REF TO cx_static_check.

**********************************************************************
*** Dynpro 9200
DATA: go_screen_objects_col TYPE REF TO lcl_screen_objects_col.
DATA: file_txtedit  TYPE string,
      file_txtedit2 TYPE char255.

DATA: go_dd_handler  TYPE REF TO lcl_dd_handler,
      go_dd_filt_opt TYPE REF TO lcl_dd_filter.

*** DD: Encoding
DATA: dd_encoding TYPE lcl_dd_handler=>t_char_255,
      dd_linefeed TYPE lcl_dd_handler=>t_char_255,
      dd_codepage TYPE lcl_dd_handler=>t_char_255.
*DATA: p_fdd TYPE lcl_dd_handler=>t_char_255.
DATA: btn_leg TYPE c, btn_enc TYPE c.
**********************************************************************

**********************************************************************
*** Dynpro 9300
DATA: go_screen_objects_simple TYPE REF TO lcl_screen_objects_col_simple.
**********************************************************************

SELECTION-SCREEN BEGIN OF SCREEN 8100 AS SUBSCREEN.
*PARAMETERS: p_sear2 TYPE string.
SELECTION-SCREEN BEGIN OF BLOCK bl_sea WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) l_sear3 FOR FIELD p_sear3. " Label
PARAMETERS: p_sear3 TYPE c LENGTH 200 VISIBLE LENGTH 41 LOWER CASE.                     " Editfeld ab Position 10
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (13) l_sosz FOR FIELD so_size. " Label
SELECT-OPTIONS so_size FOR gv_filesize.
SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT (13) l_soow FOR FIELD so_own. " Label
*SELECT-OPTIONS so_own FOR gv_owner.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (13) l_chd FOR FIELD so_chd. " Label
SELECT-OPTIONS so_chd FOR gv_changed.
SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT (16) l_fil FOR FIELD p_fil. " Label
*PARAMETERS p_fil TYPE c AS CHECKBOX.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) l_fdd FOR FIELD p_fdd. " Label
PARAMETERS p_fdd TYPE lcl_dd_handler=>t_char_255 AS LISTBOX VISIBLE LENGTH 20 OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) l_cur. " Label
SELECTION-SCREEN PUSHBUTTON 18(50) btn_up USER-COMMAND crs_up VISIBLE LENGTH 5.
SELECTION-SCREEN PUSHBUTTON 24(50) btn_down USER-COMMAND crs_down VISIBLE LENGTH 5.
PARAMETERS: p_cur TYPE lcl_match_cursor=>t_cursor_info VISIBLE LENGTH 10 MODIF ID a0.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (16) l_clear. "FOR FIELD btn_clr.
SELECTION-SCREEN: PUSHBUTTON 18(50) btn_clr USER-COMMAND btn_clear VISIBLE LENGTH 5.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl_sea.
*SELECTION-SCREEN BEGIN OF BLOCK bl_file WITH FRAME TITLE TEXT-002.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: p_fil_t TYPE string MODIF ID a0.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK bl_file.
SELECTION-SCREEN END OF SCREEN 8100.

AT SELECTION-SCREEN OUTPUT.
  IF go_dd_filt_opt->m_init_done EQ abap_false.
    TRY.
        go_dd_filt_opt->set_values( ).
        p_fdd = lcl_dd_filter=>default_filteropt.
      CATCH lcx_vrm_value.
    ENDTRY.
  ENDIF.
  LOOP AT SCREEN INTO DATA(wa_screen).
    IF wa_screen-group1 = 'A0'.
      wa_screen-input = 0.
      MODIFY SCREEN FROM wa_screen.
    ENDIF.
  ENDLOOP.
