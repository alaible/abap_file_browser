CLASS zcl_fs_path DEFINITION
  PUBLIC
  INHERITING FROM cl_fs_path
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_components TYPE TABLE OF string WITH EMPTY KEY.
    CLASS-METHODS:
      create_ext IMPORTING name           TYPE  clike
                           force_absolute TYPE  abap_bool DEFAULT abap_false
                           path_kind      TYPE  path_kind_t DEFAULT space
                 RETURNING VALUE(result)  TYPE REF TO cl_fs_path
                 RAISING   cx_smart_path_syntax.
    METHODS:
      get_components RETURNING VALUE(rt_comp) TYPE tt_components.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FS_PATH IMPLEMENTATION.


  METHOD create_ext.
    DATA: inst TYPE REF TO zcl_fs_path.
    result = cl_fs_path=>create(
               name           = name
               force_absolute = force_absolute
               path_kind      = path_kind
             ).
    TRY.
        inst ?= result.
      CATCH cx_sy_move_cast_error.
    ENDTRY.
*             CATCH cx_smart_path_syntax.
  ENDMETHOD.


  METHOD get_components.
    rt_comp = me->components.
  ENDMETHOD.
ENDCLASS.
