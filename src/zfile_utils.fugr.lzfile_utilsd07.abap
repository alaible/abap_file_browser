*&---------------------------------------------------------------------*
*& Include          LZFILE_UTILSD07
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_dd_handler
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_dd_filter DEFINITION DEFERRED.
CLASS lcl_dd_handler DEFINITION FRIENDS lcl_dd_filter.
  PUBLIC SECTION.
**********************************************************************
*** Public Types
    TYPES: t_char_255 TYPE c LENGTH 255.
    TYPES: BEGIN OF t_init,
             ref_dd_f TYPE REF TO t_char_255,
             name     TYPE vrm_id,
           END OF t_init.
*** benötigt für vrm_setvalues
    TYPES: BEGIN OF t_keypair,
             key  TYPE string,
             text TYPE string,
           END OF t_keypair.
**********************************************************************
    CLASS-METHODS:
      class_constructor.
    CLASS-DATA:
      gt_encoding   TYPE vrm_values,
      gt_linefeed   TYPE vrm_values,
      gt_codepage   TYPE vrm_values,
      gt_codepage_t TYPE vrm_values.
    CONSTANTS: BEGIN OF c_encoding,
                 default     TYPE t_char_255 VALUE 'DEFAULT',
                 utf_8       TYPE t_char_255 VALUE 'UTF-8',
                 non_unicode TYPE t_char_255 VALUE 'NON-UNICODE',
*                 utf_16  TYPE t_char_255 VALUE 'UTF-16',
               END OF c_encoding.
    CONSTANTS: BEGIN OF c_linefeed,
                 native  TYPE t_char_255 VALUE 'NATIVE',
                 smart   TYPE t_char_255 VALUE 'SMART',
                 windows TYPE t_char_255 VALUE 'WINDOWS',
                 unix    TYPE t_char_255 VALUE 'UNIX',
               END OF c_linefeed.
    CONSTANTS: BEGIN OF c_codepage,
                 cp_1100 TYPE t_char_255 VALUE '1100',      "iso-8859-1
                 cp_1101 TYPE t_char_255 VALUE '1101', "ascii
                 cp_1105 TYPE t_char_255 VALUE '1105', "us-ascii (7 bits)
                 cp_1160 TYPE t_char_255 VALUE '1160', "windows-1252
                 cp_1401 TYPE t_char_255 VALUE '1401',      "iso-8859-2
                 cp_4102 TYPE t_char_255 VALUE '4102', "utf-16be
                 cp_4103 TYPE t_char_255 VALUE '4103', "utf-16le
                 cp_4110 TYPE t_char_255 VALUE '4110', "utf-8
               END OF c_codepage.
    CONSTANTS: BEGIN OF c_cp_text,
                 cp_1100 TYPE t_char_255 VALUE '1100 (iso-8859-1)',
                 cp_1101 TYPE t_char_255 VALUE '1101 (ascii)',
                 cp_1105 TYPE t_char_255 VALUE '1105 (us-ascii (7 bits))',
                 cp_1160 TYPE t_char_255 VALUE '1160 (windows-1252)',
                 cp_1401 TYPE t_char_255 VALUE '1401 (iso-8859-2)',
                 cp_4102 TYPE t_char_255 VALUE '4102 (utf-16be)',
                 cp_4103 TYPE t_char_255 VALUE '4103 (utf-16le)',
                 cp_4110 TYPE t_char_255 VALUE '4110 (utf-8)',
               END OF c_cp_text.
    METHODS:
      set_dd_encoding IMPORTING is_init TYPE t_init,
      set_dd_linefeed IMPORTING is_init TYPE t_init,
      set_dd_codepage IMPORTING is_init TYPE t_init,
      set_values RAISING lcx_vrm_value,
      get_encoding RETURNING VALUE(rv_enc) TYPE t_char_255 RAISING lcx_no_value_set,
      get_linefeed RETURNING VALUE(rv_lf) TYPE t_char_255 RAISING lcx_no_value_set,
      get_codepage RETURNING VALUE(rv_cp) TYPE t_char_255 RAISING lcx_no_value_set.
  PROTECTED SECTION.
    CLASS-METHODS: vrm_setvalues IMPORTING iv_id TYPE vrm_id it_values TYPE vrm_values RAISING lcx_vrm_value.
    DATA: ms_encoding TYPE t_init,
          ms_linefeed TYPE t_init,
          ms_codepage TYPE t_init.
ENDCLASS.

CLASS lcl_dd_filter DEFINITION.
**********************************************************************
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.
*    types: begin of t_vrm_value.
*      INCLUDE TYPE vrm_value.
*      types:
    CONSTANTS: BEGIN OF c_filt_opt,
                 nofilter         TYPE lcl_dd_handler=>t_char_255 VALUE 'No Filter',
                 filter_all       TYPE lcl_dd_handler=>t_char_255 VALUE 'Filter all',
                 filter_file_only TYPE lcl_dd_handler=>t_char_255 VALUE 'Keep Subdirectories',
               END OF c_filt_opt.
    METHODS:
      get_fil_opt RETURNING VALUE(rv_filopt) TYPE lcl_dd_handler=>t_char_255 RAISING lcx_no_value_set,
      set_dd_filt_opt IMPORTING is_init TYPE lcl_dd_handler=>t_init,
      set_values RAISING lcx_vrm_value.
    DATA:
      m_init_done TYPE abap_bool READ-ONLY.
    CLASS-DATA:
      default_filteropt TYPE vrm_value-key READ-ONLY,
      gt_filtopt        TYPE vrm_values.
  PROTECTED SECTION.
*    CLASS-METHODS: vrm_setvalues IMPORTING iv_id TYPE vrm_id it_values TYPE vrm_values RAISING lcx_vrm_value.
    DATA: ms_filt_opt TYPE lcl_dd_handler=>t_init.
ENDCLASS.

CLASS lcl_dd_filter IMPLEMENTATION.
  METHOD class_constructor.
    gt_filtopt = VALUE #(
                    ( key = '1' text = c_filt_opt-filter_file_only )
                    ( key = '2' text = c_filt_opt-filter_all )
                    ( key = '3' text = c_filt_opt-nofilter ) ).
    default_filteropt = gt_filtopt[ 1 ]-key.
  ENDMETHOD.
  METHOD get_fil_opt.
    FIELD-SYMBOLS: <dd_filopt> TYPE lcl_dd_handler=>t_char_255.
    ASSIGN ms_filt_opt-ref_dd_f->* TO <dd_filopt>.
    IF line_exists( gt_filtopt[ key = <dd_filopt> ] ).
      rv_filopt = gt_filtopt[ key = <dd_filopt> ]-text.
    ELSE.
      RAISE EXCEPTION TYPE lcx_no_value_set.
    ENDIF.
  ENDMETHOD.
  METHOD set_values.
    FIELD-SYMBOLS: <dd_filt_opt> TYPE lcl_dd_handler=>t_char_255.
    ASSIGN ms_filt_opt-ref_dd_f->* TO <dd_filt_opt>.
    CHECK sy-subrc = 0.
    lcl_dd_handler=>vrm_setvalues(
      EXPORTING
        iv_id     = ms_filt_opt-name
        it_values = gt_filtopt
    ).
*    <dd_filt_opt> = c_filt_opt-filter_file_only.
    m_init_done = abap_true.
  ENDMETHOD.
  METHOD set_dd_filt_opt.
    ms_filt_opt = is_init.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_dd_handler IMPLEMENTATION.
  METHOD class_constructor.
    gt_encoding = VALUE #(
                    ( key = '1' text = c_encoding-default )
                    ( key = '2' text = c_encoding-utf_8 )
                    ( key = '3' text = c_encoding-non_unicode ) ).
    gt_linefeed = VALUE #(
                    ( key = '1' text = c_linefeed-native )
                    ( key = '2' text = c_linefeed-smart )
                    ( key = '3' text = c_linefeed-unix )
                    ( key = '4' text = c_linefeed-windows ) ).
    gt_codepage = VALUE #(
                    ( key = '1' text = c_codepage-cp_1100 )
                    ( key = '2' text = c_codepage-cp_1101 )
                    ( key = '3' text = c_codepage-cp_1105 )
                    ( key = '4' text = c_codepage-cp_1160 )
                    ( key = '5' text = c_codepage-cp_1401 )
                    ( key = '6' text = c_codepage-cp_4102 )
                    ( key = '7' text = c_codepage-cp_4103 )
                    ( key = '8' text = c_codepage-cp_4110 ) ).
    gt_codepage_t = VALUE #(
                    ( key = '1' text = c_cp_text-cp_1100 )
                    ( key = '2' text = c_cp_text-cp_1101 )
                    ( key = '3' text = c_cp_text-cp_1105 )
                    ( key = '4' text = c_cp_text-cp_1160 )
                    ( key = '5' text = c_cp_text-cp_1401 )
                    ( key = '6' text = c_cp_text-cp_4102 )
                    ( key = '7' text = c_cp_text-cp_4103 )
                    ( key = '8' text = c_cp_text-cp_4110 ) ).
  ENDMETHOD.
  METHOD set_dd_encoding.
    ms_encoding = is_init.
  ENDMETHOD.
  METHOD set_dd_codepage.
    ms_codepage = is_init.
  ENDMETHOD.
  METHOD set_dd_linefeed.
    ms_linefeed = is_init.
  ENDMETHOD.
  METHOD get_encoding.
    FIELD-SYMBOLS: <dd_enc> TYPE t_char_255.
    ASSIGN ms_encoding-ref_dd_f->* TO <dd_enc>.
    IF line_exists( gt_encoding[ key = <dd_enc> ] ).
      rv_enc = gt_encoding[ key = <dd_enc> ]-text.
    ELSE.
      RAISE EXCEPTION TYPE lcx_no_value_set.
    ENDIF.
  ENDMETHOD.
  METHOD get_linefeed.
    FIELD-SYMBOLS: <dd_lf> TYPE t_char_255.
    ASSIGN ms_linefeed-ref_dd_f->* TO <dd_lf>.
    IF line_exists( gt_linefeed[ key = <dd_lf> ] ).
      rv_lf = gt_linefeed[ key = <dd_lf> ]-text.
    ELSE.
      RAISE EXCEPTION TYPE lcx_no_value_set.
    ENDIF.
  ENDMETHOD.
  METHOD get_codepage.
    FIELD-SYMBOLS: <dd_cp> TYPE t_char_255.
    ASSIGN ms_codepage-ref_dd_f->* TO <dd_cp>.
    IF line_exists( gt_codepage[ key = <dd_cp> ] ).
      rv_cp = gt_codepage[ key = <dd_cp> ]-text.
    ELSE.
      RAISE EXCEPTION TYPE lcx_no_value_set.
    ENDIF.
  ENDMETHOD.
  METHOD set_values.
    FIELD-SYMBOLS: <dd_encoding> TYPE t_char_255,
                   <dd_linefeed> TYPE t_char_255,
                   <dd_codepage> TYPE t_char_255.
    ASSIGN ms_encoding-ref_dd_f->* TO <dd_encoding>.
    CHECK sy-subrc = 0.
    ASSIGN ms_codepage-ref_dd_f->* TO <dd_codepage>.
    CHECK sy-subrc = 0.
    ASSIGN ms_linefeed-ref_dd_f->* TO <dd_linefeed>.
    CHECK sy-subrc = 0.
    me->vrm_setvalues(
      EXPORTING
        iv_id     = ms_encoding-name
        it_values = gt_encoding
    ).
    me->vrm_setvalues(
      EXPORTING
        iv_id     = ms_linefeed-name
        it_values = gt_linefeed
    ).
    me->vrm_setvalues(
      EXPORTING
        iv_id     = ms_codepage-name
        it_values = gt_codepage_t
    ).
*    CATCH lcx_vrm_value.
  ENDMETHOD.
  METHOD vrm_setvalues.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = iv_id " Name der Wertemenge
        values          = it_values    " Wertetabelle für ID
      EXCEPTIONS
        id_illegal_name = 1                " Der Name (ID) enthält ungültige Zeichen
        OTHERS          = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_vrm_value
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
