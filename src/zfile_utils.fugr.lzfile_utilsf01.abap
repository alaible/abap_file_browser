*----------------------------------------------------------------------*
***INCLUDE LZFILE_UTILSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CREATE_ICONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_icons .
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_previous_value     " Ikonenname ( Name aus INCLUDE <ICON> )
    IMPORTING
      result = btn_down.               " Ikone ( geben Sie hier das Dynprofeld an)
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_next_value         " Ikonenname ( Name aus INCLUDE <ICON> )
    IMPORTING
      result = btn_up.                 " Ikone ( geben Sie hier das Dynprofeld an)
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_delete_template    " Ikonenname ( Name aus INCLUDE <ICON> )
    IMPORTING
      result = btn_clr.                " Ikone ( geben Sie hier das Dynprofeld an)
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.
