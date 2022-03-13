*&---------------------------------------------------------------------*
*& Include          LZFILE_UTILSD06
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_screen_objects_col_simple
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_screen_objects_col_simple DEFINITION INHERITING FROM lcl_screen_objects_col.
  PUBLIC SECTION.
    METHODS: build_alv_tree REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      init_container REDEFINITION,
      init_splitter_left REDEFINITION.
ENDCLASS.

CLASS lcl_screen_objects_col_simple IMPLEMENTATION.
  METHOD build_alv_tree.
    FIELD-SYMBOLS: <node_table> TYPE zcl_dir_cont=>tt_file_alv.
    IF mv_alv_constructed EQ abap_false.
      me->init_container( ).
      me->init_splitter_left( ).
*** Kein Drag und Drop!!
*      me->init_dd_objects( ).
*** Kein Text-Edit!
*      me->init_text_edit( ).
**********************************************************************
      me->init_alv_tree_col( ).
      me->init_toolbar( ).
      mr_node_table = NEW #( ).
      mv_alv_constructed = abap_true.
    ENDIF.
    me->add_root_node_to_tree( ).
  ENDMETHOD.
  METHOD init_container.
    CREATE OBJECT mo_docking_bottom
      EXPORTING
        side                        = cl_gui_docking_container=>dock_at_bottom     " Side to which this control is docked to
*       extension                   = 250              " Extension of this  control
        ratio                       = 67               " Prozent des Dynpros: gewinnt gegen EXTENSION
      EXCEPTIONS
        cntl_error                  = 1                " Invalid parent control
        cntl_system_error           = 2                " System Error
        create_error                = 3                " Create Error
        lifetime_error              = 4                " Lifetime Error
        lifetime_dynpro_dynpro_link = 5                " LIFETIME_DYNPRO_DYNPRO_LINK
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_scrn_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD init_splitter_left.
    CREATE OBJECT mo_splitter_left
      EXPORTING
        parent            = mo_docking_bottom   " Parent Container
        rows              = 2                   " Anzahl zu zeigender Zeilen
        columns           = 1                   " Anzahl zu zeigender Spalten
      EXCEPTIONS
        cntl_error        = 1                   " siehe Oberklasse
        cntl_system_error = 2                   " siehe Oberklasse
        OTHERS            = 3.
    IF sy-subrc <> 0.
      zcx_scrn_err=>raise_from_msg( |Error while creating left splitter (sy-subrc:{ sy-subrc })| ).
    ENDIF.
**********************************************************************
    mo_splitter_left->set_row_mode(
      EXPORTING
        mode              = cl_gui_splitter_container=>mode_relative " Zeilenmodus
      EXCEPTIONS
        cntl_error        = 1                " siehe CL_GUI_CONTROL
        cntl_system_error = 2                " siehe CL_GUI_CONTROL
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      zcx_scrn_err=>raise_from_msg( |Error set_row_mode (sy-subrc:{ sy-subrc })| ).
    ENDIF.
**********************************************************************
    mo_splitter_left->set_row_sash(
          EXPORTING
          id                = 1
          type              = cl_gui_splitter_container=>type_sashvisible
          value             = cl_gui_splitter_container=>false ).
**********************************************************************
    mo_splitter_left->set_row_height(
      EXPORTING
        id                = 1                 " Id der Zeile
        height            = 5                 " Höhe
      EXCEPTIONS
        cntl_error        = 1                 " siehe CL_GUI_CONTROL
        cntl_system_error = 2                 " siehe CL_GUI_CONTROL
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      zcx_scrn_err=>raise_from_msg( |Error set_column_width (sy-subrc:{ sy-subrc })| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
