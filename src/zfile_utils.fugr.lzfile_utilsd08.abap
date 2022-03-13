*&---------------------------------------------------------------------*
*& Include          LZFILE_UTILSD08
*&---------------------------------------------------------------------*
*& Class lcl_splitter_base
*&---------------------------------------------------------------------*
*& Abstrakte Basis-Klasse für Docking-Control(s)
*& -> Im linken Splitter ist ein weiterer Splitter eingebettet (für Toolbar)
*& -> Konkrete Breiten/Höhen der Splitter durch impl. Klassen
*&---------------------------------------------------------------------*
CLASS lcl_splitter_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES: ty_c30 TYPE c LENGTH 30.
  PROTECTED SECTION.
    DATA: mo_docking_left  TYPE REF TO cl_gui_docking_container,
          mo_docking_right TYPE REF TO cl_gui_docking_container.
    DATA: mo_docking_bottom TYPE REF TO cl_gui_docking_container.
    DATA: mo_cust_control     TYPE REF TO cl_gui_custom_container,
          mo_splitter_control TYPE REF TO cl_gui_splitter_container,
          mo_splitter_left    TYPE REF TO cl_gui_splitter_container.
    METHODS:
**********************************************************************
*** Abstrakte Methoden
      get_container_name ABSTRACT RETURNING VALUE(rv_name) TYPE ty_c30,
      get_left_splter_width ABSTRACT RETURNING VALUE(rv_left_width) TYPE i,
*** Initialiserungs-Methoden der Enjoy-Controls
      init_container RAISING zcx_scrn_err,
      init_splitter RAISING zcx_scrn_err,
      init_splitter_left RAISING zcx_scrn_err.
    METHODS:
      free_container RAISING zcx_scrn_err.
ENDCLASS.

CLASS lcl_splitter_base IMPLEMENTATION.
  METHOD init_container.
*    CREATE OBJECT mo_docking_right
*      EXPORTING
*        side                        = cl_gui_docking_container=>dock_at_right     " Side to which this control is docked to
**       extension                   = 530               " Extension of this  control
*        ratio                       = 62                 " Prozent des Dynpros: gewinnt gegen EXTENSION
*      EXCEPTIONS
*        cntl_error                  = 1                " Invalid parent control
*        cntl_system_error           = 2                " System Error
*        create_error                = 3                " Create Error
*        lifetime_error              = 4                " Lifetime Error
*        lifetime_dynpro_dynpro_link = 5                " LIFETIME_DYNPRO_DYNPRO_LINK
*        OTHERS                      = 6.
*    IF sy-subrc <> 0.
*      zcx_scrn_err=>raise_from_msg( |Error while creating right docker (sy-subrc:{ sy-subrc })| ).
*    ENDIF.
*    CREATE OBJECT mo_docking_left
*      EXPORTING
*        side                        = cl_gui_docking_container=>dock_at_bottom     " Side to which this control is docked to
**       extension                   = 190               " Extension of this  control
*        ratio                       = 54                 " Prozent des Dynpros: gewinnt gegen EXTENSION
*      EXCEPTIONS
*        cntl_error                  = 1                " Invalid parent control
*        cntl_system_error           = 2                " System Error
*        create_error                = 3                " Create Error
*        lifetime_error              = 4                " Lifetime Error
*        lifetime_dynpro_dynpro_link = 5                " LIFETIME_DYNPRO_DYNPRO_LINK
*        OTHERS                      = 6.
*    IF sy-subrc <> 0.
*      zcx_scrn_err=>raise_from_msg( |Error while creating left docker (sy-subrc:{ sy-subrc })| ).
*    ENDIF.
    CREATE OBJECT mo_docking_bottom
      EXPORTING
        side                        = cl_gui_docking_container=>dock_at_bottom     " Side to which this control is docked to
*       extension                   = 190               " Extension of this  control
        ratio                       = 75                 " Prozent des Dynpros: gewinnt gegen EXTENSION
      EXCEPTIONS
        cntl_error                  = 1                " Invalid parent control
        cntl_system_error           = 2                " System Error
        create_error                = 3                " Create Error
        lifetime_error              = 4                " Lifetime Error
        lifetime_dynpro_dynpro_link = 5                " LIFETIME_DYNPRO_DYNPRO_LINK
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      zcx_scrn_err=>raise_from_msg( |Error while creating left docker (sy-subrc:{ sy-subrc })| ).
    ENDIF.
  ENDMETHOD.
  METHOD init_splitter.
    IF mo_splitter_control IS BOUND.
      IF mo_splitter_control->is_alive( ).
        FREE mo_splitter_control.
      ENDIF.
    ENDIF.
    CREATE OBJECT mo_splitter_control
      EXPORTING
        parent            = mo_docking_bottom  " Parent Container
        rows              = 1                  " Anzahl zu zeigender Zeilen
        columns           = 2                  " Anzahl zu zeigender Spalten
      EXCEPTIONS
        cntl_error        = 1                  " siehe Oberklasse
        cntl_system_error = 2                  " siehe Oberklasse
        OTHERS            = 3.
    IF sy-subrc <> 0.
      zcx_scrn_err=>raise_from_msg( |Error while creating main splitter (sy-subrc:{ sy-subrc })| ).
    ENDIF.
**********************************************************************
*** rel. Splitter-Breite -> akt. Wert durch impl. Klasse
    mo_splitter_control->set_row_mode(
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
    mo_splitter_control->set_row_sash(
          EXPORTING
          id                = 1
          type              = cl_gui_splitter_container=>type_sashvisible
          value             = cl_gui_splitter_container=>false ).
**********************************************************************
    mo_splitter_control->set_column_width(
      EXPORTING
        id                = 1                " Id der Spalte
        width             = get_left_splter_width( ) " Breite
      EXCEPTIONS
        cntl_error        = 1                " siehe CL_GUI_CONTROL
        cntl_system_error = 2                " siehe CL_GUI_CONTROL
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      zcx_scrn_err=>raise_from_msg( iv_msg = |Error set_column_width (sy-subrc:{ sy-subrc })| ).
    ENDIF.
  ENDMETHOD.
  METHOD init_splitter_left.
    CREATE OBJECT mo_splitter_left
      EXPORTING
        parent            = mo_splitter_control->get_container(
                              row    = 1
                              column = 1
                            )     " Parent Container
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
  METHOD free_container.
    me->mo_splitter_control->get_container( row = 1  column = 1 )->free(
      EXCEPTIONS
        cntl_error        = 1                " CNTL_ERROR
        cntl_system_error = 2                " CNTL_SYSTEM_ERROR
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_scrn_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
