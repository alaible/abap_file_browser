CLASS zcl_file_scr_utils DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: t_char_50 TYPE c LENGTH 50.
    TYPES: BEGIN OF t_err_var,
             var1 TYPE t_char_50,
             var2 TYPE t_char_50,
             var3 TYPE t_char_50,
             var4 TYPE t_char_50,
           END OF t_err_var.
    TYPES: tt_string TYPE TABLE OF string WITH EMPTY KEY.
    CLASS-METHODS: string_to_err_var IMPORTING iv_str TYPE string RETURNING VALUE(rs_err) TYPE t_err_var.
    CLASS-METHODS: split_string IMPORTING i_string TYPE string i_max_len TYPE i RETURNING VALUE(splitted) TYPE tt_string.
ENDCLASS.



CLASS ZCL_FILE_SCR_UTILS IMPLEMENTATION.


  METHOD split_string.
*    DATA(copy) = i_string.
    DATA: current_offset TYPE i, len TYPE i.
    WHILE strlen( i_string+current_offset ) > i_max_len.
      current_offset = ( sy-index - 1 ) * i_max_len.
      len = COND #( WHEN strlen( i_string+current_offset ) > i_max_len THEN i_max_len ELSE strlen( i_string+current_offset ) ).
      splitted = VALUE #( BASE splitted ( i_string+current_offset(len) ) ).
    ENDWHILE.
    IF lines( splitted ) = 0.
      len = strlen( i_string+current_offset ).
      APPEND i_string+current_offset(len) TO splitted.
    ENDIF.
  ENDMETHOD.


  METHOD string_to_err_var.
    DATA: lv_offset TYPE i.
    lv_offset = strlen( iv_str ).
**********************************************************************
    CASE strlen( iv_str ) DIV 50.
      WHEN 0.
        rs_err-var1 = iv_str(lv_offset).
      WHEN 1.
        lv_offset = strlen( iv_str ) - 50.
        rs_err-var1 = iv_str(50).
        rs_err-var2 = iv_str+50(lv_offset).
      WHEN 2.
        lv_offset = strlen( iv_str ) - 100.
        rs_err-var1 = iv_str(50).
        rs_err-var2 = iv_str+50(50).
        rs_err-var3 = iv_str+100(lv_offset).
      WHEN OTHERS.
        lv_offset = strlen( iv_str ) - 150.
        rs_err-var1 = iv_str(50).
        rs_err-var2 = iv_str+50(50).
        rs_err-var3 = iv_str+100(50).
        rs_err-var4 = iv_str+150(lv_offset).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
