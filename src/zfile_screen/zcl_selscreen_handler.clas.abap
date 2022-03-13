CLASS zcl_selscreen_handler DEFINITION PUBLIC CREATE PUBLIC .
    PUBLIC SECTION.
    TYPES: ty_c_255 TYPE c LENGTH 255.
    TYPES: tt_log TYPE TABLE OF string WITH EMPTY KEY.
    CLASS-METHODS: get_comp_flat_str IMPORTING it_comp TYPE zcl_dir_cont=>tt_comp RETURNING VALUE(rv_flat) TYPE string.
    METHODS:
      set_ref_base_path IMPORTING ir_ref TYPE REF TO pathintern RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_base_path_h IMPORTING ir_ref TYPE REF TO pathintern RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_param_1 IMPORTING ir_ref TYPE REF TO ty_c_255 RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_param_1_h IMPORTING ir_ref TYPE REF TO ty_c_255 RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_param_2 IMPORTING ir_ref TYPE REF TO ty_c_255 RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_param_3 IMPORTING ir_ref TYPE REF TO ty_c_255 RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_file_name IMPORTING ir_ref TYPE REF TO ty_c_255 RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_file_name_h IMPORTING ir_ref TYPE REF TO ty_c_255 RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_path_raw IMPORTING ir_ref TYPE REF TO ty_c_255 RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_path_exp IMPORTING ir_ref TYPE REF TO ty_c_255 RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler,
      set_ref_selection_par IMPORTING ir_ref TYPE REF TO ty_c_255 RETURNING VALUE(ro_inst) TYPE REF TO zcl_selscreen_handler.
    METHODS:
      init_base_path RAISING zcx_file_err,
      handle_at_sel_screen RAISING zcx_file_err,
      handle_at_sel_screen_output RAISING zcx_file_err,
      handle_value_request RAISING zcx_file_err,
      handle_start_of_selection RETURNING VALUE(rt_log) TYPE tt_log RAISING zcx_file_err.
    DATA: mv_path_raw TYPE ty_c_255 READ-ONLY,
          mv_path_exp TYPE ty_c_255 READ-ONLY.
  PROTECTED SECTION.
*** References
    DATA: mr_base_path        TYPE REF TO pathintern,
          mr_base_path_hidden TYPE REF TO pathintern,
          mr_param_1          TYPE REF TO ty_c_255,
          mr_param_1_hidden   TYPE REF TO ty_c_255,
          mr_param_2          TYPE REF TO ty_c_255,
          mr_param_3          TYPE REF TO ty_c_255,
          mr_file_name        TYPE REF TO ty_c_255,
          mr_file_name_hidden TYPE REF TO ty_c_255,
          mr_path_raw         TYPE REF TO ty_c_255,
          mr_path_exp         TYPE REF TO ty_c_255,
          mr_selection_par    TYPE REF TO ty_c_255.
    DATA: mo_base_path TYPE REF TO zcl_log_path,
          mo_comp      TYPE TABLE OF string.
private section.
ENDCLASS.



CLASS ZCL_SELSCREEN_HANDLER IMPLEMENTATION.


  METHOD get_comp_flat_str.
    LOOP AT it_comp ASSIGNING FIELD-SYMBOL(<comp>).
      rv_flat = |{ rv_flat }{ <comp> }::|.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_at_sel_screen.
    FIELD-SYMBOLS: <base_path>        TYPE pathintern,
                   <base_path_hidden> TYPE pathintern,
                   <file_name>        TYPE ty_c_255,
                   <file_name_hidden> TYPE ty_c_255,
                   <param_1>          TYPE ty_c_255,
                   <param_1_hidden>   TYPE ty_c_255,
                   <path_raw>         TYPE ty_c_255,
                   <path_exp>         TYPE ty_c_255..

    ASSIGN mr_base_path->* TO <base_path>.
    ASSIGN mr_base_path_hidden->* TO <base_path_hidden>.
    ASSIGN mr_file_name->* TO <file_name>.
    ASSIGN mr_file_name_hidden->* TO <file_name_hidden>.
    ASSIGN mr_param_1->* TO <param_1>.
    ASSIGN mr_param_1_hidden->* TO <param_1_hidden>.
    ASSIGN mr_path_exp->* TO <path_exp>.
    ASSIGN mr_path_raw->* TO <path_raw>.

    IF <base_path_hidden> NE <base_path>.
      CLEAR <file_name>.
      CLEAR <path_exp>.
      CLEAR: <param_1>, <param_1_hidden>.
    ENDIF.
    IF <base_path> IS NOT INITIAL.
      mo_base_path = NEW #( <base_path> ).
      <path_raw> = CONV #( mo_base_path->mv_path_raw ).
    ENDIF.
  ENDMETHOD.


  METHOD handle_at_sel_screen_output.
    DATA: lt_comp TYPE TABLE OF string.
    FIELD-SYMBOLS: <base_path>        TYPE pathintern,
                   <base_path_hidden> TYPE pathintern,
                   <file_name>        TYPE ty_c_255,
                   <file_name_hidden> TYPE ty_c_255,
                   <param_1>          TYPE ty_c_255,
                   <param_1_hidden>   TYPE ty_c_255,
                   <path_raw>         TYPE ty_c_255,
                   <path_exp>         TYPE ty_c_255..

    ASSIGN mr_base_path->* TO <base_path>.
    ASSIGN mr_base_path_hidden->* TO <base_path_hidden>.
    ASSIGN mr_file_name->* TO <file_name>.
    ASSIGN mr_file_name_hidden->* TO <file_name_hidden>.
    ASSIGN mr_param_1->* TO <param_1>.
    ASSIGN mr_param_1_hidden->* TO <param_1_hidden>.
    ASSIGN mr_path_exp->* TO <path_exp>.
    ASSIGN mr_path_raw->* TO <path_raw>.

    CHECK mo_base_path IS BOUND.

*** hier wird sich der aktuelle wert in p_bhid gemerkt, damit später eine prüfung erfolgen kann
*** ob dieser Wert geändert wurde
    <base_path_hidden> = <base_path>.
*** Konstruktion des Dynmischen Pfadteils
    IF <param_1_hidden> IS NOT INITIAL OR <file_name> IS NOT INITIAL.
      SPLIT <param_1_hidden> AT '::' INTO TABLE lt_comp.
      mo_base_path->set_p1_from_comp( lt_comp ).
      <param_1> = mo_base_path->mv_p1_sub_path.
      IF <file_name> IS INITIAL.
        mo_base_path->read_path_expanded( ).
      ELSE.
        IF <file_name_hidden> IS NOT INITIAL.
          <file_name> = <file_name_hidden>.
          mo_base_path->set_file_name( CONV #( <file_name_hidden> ) ).
          CLEAR <file_name_hidden>.
        ELSE.
          mo_base_path->set_file_name( CONV #( <file_name> ) ).
        ENDIF.
        mo_base_path->read_path_expanded_w_fname( ).
      ENDIF.
    ENDIF.
    IF mo_base_path IS BOUND.
      <path_exp> = mo_base_path->mv_path_expanded.
      <path_raw> = mo_base_path->mv_path_raw.
    ENDIF.
    IF <base_path> IS INITIAL.
      CLEAR: <path_exp>, <path_raw>.
    ENDIF.
  ENDMETHOD.


  METHOD handle_start_of_selection.
    DATA: "lo_sel_par TYPE REF TO zcl_sel_par,
          lt_comp    TYPE TABLE OF string.
    FIELD-SYMBOLS: <base_path>        TYPE pathintern,
                   <base_path_hidden> TYPE pathintern,
                   <file_name>        TYPE ty_c_255,
                   <file_name_hidden> TYPE ty_c_255,
                   <param_1>          TYPE ty_c_255,
                   <param_1_hidden>   TYPE ty_c_255,
                   <path_raw>         TYPE ty_c_255,
                   <path_exp>         TYPE ty_c_255,
                   <sel_parameter>    TYPE ty_c_255.

    ASSIGN mr_base_path->* TO <base_path>.
    ASSIGN mr_base_path_hidden->* TO <base_path_hidden>.
    ASSIGN mr_file_name->* TO <file_name>.
    ASSIGN mr_file_name_hidden->* TO <file_name_hidden>.
    ASSIGN mr_param_1->* TO <param_1>.
    ASSIGN mr_param_1_hidden->* TO <param_1_hidden>.
    ASSIGN mr_path_exp->* TO <path_exp>.
    ASSIGN mr_path_raw->* TO <path_raw>.
    ASSIGN mr_selection_par->* TO <sel_parameter>.

    CHECK <base_path> IS NOT INITIAL.

    TRY.
*        lo_sel_par = zcl_sel_var_factory=>create_selpar_with_e_lock( iv_name = CONV #( <sel_parameter> ) ).
        IF <param_1_hidden> IS NOT INITIAL OR <file_name> IS NOT INITIAL.
          SPLIT <param_1_hidden> AT '::' INTO TABLE lt_comp.
          mo_base_path->set_p1_from_comp( lt_comp ).
          <param_1> = mo_base_path->mv_p1_sub_path.
          IF <file_name> IS INITIAL.
            mo_base_path->read_path_expanded( ).
          ELSE.
            IF <file_name_hidden> IS NOT INITIAL.
              <file_name> = <file_name_hidden>.
              mo_base_path->set_file_name( CONV #( <file_name_hidden> ) ).
              CLEAR <file_name_hidden>.
            ELSE.
              mo_base_path->set_file_name( CONV #( <file_name> ) ).
            ENDIF.
            mo_base_path->read_path_expanded_w_fname( ).
          ENDIF.
        ENDIF.
*        LOOP AT lo_sel_par->mt_tvarvc_old ASSIGNING FIELD-SYMBOL(<old_val>).
*          APPEND |Selpar { <sel_parameter> }(alter Wert): { <old_val>-low }| TO rt_log.
*        ENDLOOP.
*        lo_sel_par->set_value( iv_value = CONV #( mo_base_path->mv_path_expanded ) ).
*        APPEND |Selpar { <sel_parameter> }(neuer Wert): { mo_base_path->mv_path_expanded }| TO rt_log.
*        lo_sel_par->save_db( ).
*        COMMIT WORK.
*        lo_sel_par->dequeue_tvarv( ).
      CATCH cx_root. " generic data access error
*      CATCH zcx_enqueue_error. " zcx_enqueue_error
    ENDTRY.
  ENDMETHOD.


  METHOD handle_value_request.
    DATA: lt_comp        TYPE TABLE OF string,
          lo_dir_service TYPE REF TO zif_dir_service.
    DATA: lo_as_dir        TYPE REF TO zcl_as_directory,
          lv_fullpath      TYPE string,
          lv_file_name     TYPE string,
          lv_file_selected TYPE abap_bool.
    FIELD-SYMBOLS: <base_path>        TYPE pathintern,
                   <base_path_hidden> TYPE pathintern,
                   <file_name>        TYPE ty_c_255,
                   <file_name_hidden> TYPE ty_c_255,
                   <param_1>          TYPE ty_c_255,
                   <param_1_hidden>   TYPE ty_c_255,
                   <path_raw>         TYPE ty_c_255,
                   <path_exp>         TYPE ty_c_255..

    ASSIGN mr_base_path->* TO <base_path>.
    ASSIGN mr_base_path_hidden->* TO <base_path_hidden>.
    ASSIGN mr_file_name->* TO <file_name>.
    ASSIGN mr_file_name_hidden->* TO <file_name_hidden>.
    ASSIGN mr_param_1->* TO <param_1>.
    ASSIGN mr_param_1_hidden->* TO <param_1_hidden>.
    ASSIGN mr_path_exp->* TO <path_exp>.
    ASSIGN mr_path_raw->* TO <path_raw>.


    CLEAR <file_name>.
    TRY.
*      BREAK-POINT.
        mo_base_path = NEW #( <base_path> ).
        lo_dir_service = NEW zcl_phys_dir_service( ).
        lo_as_dir = NEW #( io_dir_service = lo_dir_service iv_base_path = mo_base_path->mv_phys_path it_comp = VALUE #( ) iv_is_root = abap_true ).
        lo_as_dir->read_dir_content( abap_true ).

        CALL FUNCTION 'Z_SELECT_PATH'
          EXPORTING
            io_base_path        = lo_as_dir " Directory auf dem AS
            iv_file_sel_allowed = abap_true
          IMPORTING
            et_comp             = lt_comp
            ev_filname          = lv_file_name
            ev_fullpath         = lv_fullpath
            ev_file_selected    = lv_file_selected
          EXCEPTIONS
            cancelled           = 1                " Auswahl abgebrochen
            OTHERS              = 2.
        CASE sy-subrc.
          WHEN 0.
            mo_base_path->set_p1_from_comp( lt_comp ).
            <param_1> = mo_base_path->mv_p1_sub_path.
            <param_1_hidden> = zcl_selscreen_handler=>get_comp_flat_str( lt_comp ).
            IF lv_file_selected = abap_true.
              <file_name> = lv_file_name.
              <file_name_hidden> = lv_file_name.
              mo_base_path->set_file_name( lv_file_name ).
              mo_base_path->read_path_expanded_w_fname( ).
            ELSE.
              CLEAR: <file_name>, <file_name_hidden>.
              mo_base_path->read_path_expanded( ).
            ENDIF.
*          cl_gui_cfw=>set_new_ok_code( EXPORTING new_code = 'SEL_SUBPATH' ).
          WHEN 1.
            MESSAGE 'cancel by user' TYPE 'I'.
        ENDCASE.
      CATCH cx_root INTO DATA(lo_err).
        RAISE EXCEPTION TYPE zcx_file_err
          EXPORTING
            previous = lo_err.
    ENDTRY.
  ENDMETHOD.


  METHOD init_base_path.
    FIELD-SYMBOLS: <path> TYPE pathintern.
    ASSIGN mr_base_path->* TO <path>.
    IF <path> IS NOT INITIAL.
      mo_base_path = NEW #( <path> ).
    ENDIF.
  ENDMETHOD.


  METHOD set_ref_base_path.
    mr_base_path =  ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_base_path_h.
    mr_base_path_hidden = ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_file_name.
    mr_file_name = ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_file_name_h.
    mr_file_name_hidden = ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_param_1.
    mr_param_1 = ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_param_1_h.
    mr_param_1_hidden = ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_param_2.
    mr_param_2 = ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_param_3.
    mr_param_3 = ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_path_exp.
    mr_path_exp = ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_path_raw.
    mr_path_raw = ir_ref.
    ro_inst = me.
  ENDMETHOD.


  METHOD set_ref_selection_par.
    mr_selection_par = ir_ref.
    ro_inst = me.
  ENDMETHOD.
ENDCLASS.
