*&---------------------------------------------------------------------*
*& Include          LZFILE_UTILSD03
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_screen_state
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_screen_state DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      set_grid_rendered IMPORTING iv_grid_rendered TYPE abap_bool,
      set_file_selection_allowed IMPORTING iv_filsel_allowed TYPE abap_bool.
    DATA: mv_grid_inital_rendered   TYPE abap_bool READ-ONLY,
          mv_file_selection_allowed TYPE abap_bool READ-ONLY.
ENDCLASS.

CLASS lcl_screen_state IMPLEMENTATION.
  METHOD set_file_selection_allowed.
    mv_file_selection_allowed = iv_filsel_allowed.
  ENDMETHOD.
  METHOD set_grid_rendered.
    mv_grid_inital_rendered = iv_grid_rendered.
  ENDMETHOD.
ENDCLASS.
