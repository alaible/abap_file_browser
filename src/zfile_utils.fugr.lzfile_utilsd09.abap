*&---------------------------------------------------------------------*
*& Include          LZFILE_UTILSD09
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class lcl_screen_base
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_screen_base DEFINITION ABSTRACT INHERITING FROM lcl_splitter_base.
  PUBLIC SECTION.
*** Hilfstyp zum initialisieren des Column-Trees
    TYPES: BEGIN OF t_tree_hierarchy,
             hierarchy_column_name TYPE tv_itmname,
             hierarchy_header      TYPE treemhhdr,
           END OF t_tree_hierarchy.
    TYPES: tt_nodekey TYPE TABLE OF tm_nodekey WITH EMPTY KEY.
    TYPES: t_c_text TYPE TABLE OF char1024 WITH DEFAULT KEY.
  PROTECTED SECTION.
    DATA: mo_alv_tree  TYPE REF TO cl_column_tree_model,
          mo_text_edit TYPE REF TO cl_gui_textedit,
          mo_toolbar   TYPE REF TO cl_gui_toolbar.
**********************************************************************
    DATA: mr_nodetab_int TYPE REF TO treemcnota,
          mr_itemtab_int TYPE REF TO treemcitac.
    METHODS:
**********************************************************************
      register_events ABSTRACT RAISING zcx_tree_error,
      register_events_toolb ABSTRACT,
      get_hierarchy_hdr ABSTRACT RETURNING VALUE(rs_hdr) TYPE t_tree_hierarchy,
      tree_add_colums ABSTRACT,
      add_toolb_buttons ABSTRACT,
**********************************************************************
      add_nodes RAISING zcx_tree_error,
      add_items RAISING zcx_tree_error,
      expand_nodes IMPORTING it_nodekey TYPE tt_nodekey RAISING zcx_tree_error,
      expand_node IMPORTING iv_nodekey TYPE tm_nodekey RAISING zcx_tree_error,
      node_set_style IMPORTING iv_node_key TYPE tm_nodekey iv_style TYPE i RAISING zcx_tree_error,
      item_set_style IMPORTING iv_node_key TYPE tm_nodekey iv_item_name TYPE tv_itmname iv_style TYPE i RAISING zcx_tree_error,
      set_registered_evts IMPORTING it_evt TYPE cntl_simple_events RAISING lcx_ctrl_error,
      set_selected_nkey IMPORTING iv_nodekey TYPE tm_nodekey RAISING zcx_tree_error,
      get_selected_nkey RETURNING VALUE(rv_nkey) TYPE tm_nodekey RAISING zcx_tree_error,
      collapse_node IMPORTING iv_nodekey TYPE tm_nodekey RAISING zcx_tree_error,
**********************************************************************
      set_read_only_mode IMPORTING iv_read_only TYPE abap_bool RAISING zcx_scrn_err,
      set_word_wrap RAISING zcx_scrn_err,
      set_text_as_table IMPORTING it_text TYPE t_c_text RAISING zcx_scrn_err,
      set_text_as_stream IMPORTING it_text TYPE t_c_text RAISING zcx_scrn_err,
      get_text_as_stream RETURNING VALUE(rt_text) TYPE t_c_text RAISING zcx_scrn_err,
**********************************************************************
      init_alv_tree_col RAISING zcx_tree_error,
      init_toolbar RAISING zcx_scrn_err,
      init_text_edit RAISING zcx_scrn_err.
ENDCLASS.

CLASS lcl_screen_base IMPLEMENTATION.
  METHOD init_alv_tree_col.
    CREATE OBJECT mo_alv_tree
      EXPORTING
        node_selection_mode   = cl_column_tree_model=>node_sel_mode_single " Knoten: Einfach- oder Mehrfachselektion
        hierarchy_column_name = get_hierarchy_hdr( )-hierarchy_column_name      " Name der Spalte im Hierarchie-Bereich
        hierarchy_header      = get_hierarchy_hdr( )-hierarchy_header
        item_selection        = abap_true         " Selektierbarkeit einzelner Items
      EXCEPTIONS
        OTHERS                = 1.
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'constructor' iv_sysubrc = sy-subrc ).
    ENDIF.
**********************************************************************
    me->tree_add_colums( ).

    mo_alv_tree->create_tree_control( parent = mo_splitter_left->get_container( row = 2 column = 1 ) ).
*** Konkrete Events in der implementierenden Klasse
    me->register_events( ).
  ENDMETHOD.

  METHOD init_text_edit.
    CREATE OBJECT mo_text_edit
      EXPORTING
        parent                 = mo_splitter_control->get_container(
                                   row    = 1
                                   column = 2
                                 )         " Parent-Container
      EXCEPTIONS
        error_cntl_create      = 1                        " Error while performing creation of TextEdit control!
        error_cntl_init        = 2                        " Error while initializing TextEdit control!
        error_cntl_link        = 3                        " Error while linking TextEdit control!
        error_dp_create        = 4                        " Error while creating DataProvider control!
        gui_type_not_supported = 5                        " This type of GUI is not supported!
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_scrn_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD init_toolbar.
    CREATE OBJECT mo_toolbar
      EXPORTING
        parent = mo_splitter_left->get_container( row = 1 column = 1 ) " Name des Containers
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_scrn_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
**********************************************************************
*** Toolbar-Drucktasten hinzufügen (abstrakt)
    me->add_toolb_buttons( ).

*** abstrakter Handler zum registrieren der Events
    me->register_events_toolb( ).
  ENDMETHOD.
**********************************************************************
*** column-tree functions
  METHOD add_nodes.
    mo_alv_tree->add_nodes(
      EXPORTING
        node_table          = mr_nodetab_int->*     " Knoten-Tabelle
      EXCEPTIONS
        error_in_node_table = 1                " Knotentabelle fehlerhaft
        OTHERS              = 2
    ).
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'add_nodes' iv_sysubrc = sy-subrc ).
    ENDIF.
  ENDMETHOD.
  METHOD add_items.
    mo_alv_tree->add_items(
     EXPORTING
       item_table          = mr_itemtab_int->*     " Items
     EXCEPTIONS
       node_not_found      = 1                " Knotenschlüssel existiert nicht
       error_in_item_table = 2                " ITEM_TABLE enthält einen fehlerhaften Eintrag
       OTHERS              = 3
   ).
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'add_items' iv_sysubrc = sy-subrc ).
    ENDIF.
  ENDMETHOD.
  METHOD expand_nodes.
    mo_alv_tree->expand_nodes(
      EXPORTING
        node_key_table          = CONV #( it_nodekey )          " Tabelle mit Knotenschlüsseln
      EXCEPTIONS
        error_in_node_key_table = 1                " unbekannter Knotenschlüssel in NODE_KEY_TABLE
        OTHERS                  = 2
    ).
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'expand_nodes' iv_sysubrc = sy-subrc ).
    ENDIF.
  ENDMETHOD.
  METHOD expand_node.
    mo_alv_tree->expand_node(
      EXPORTING
        node_key            = iv_nodekey        " Schlüssel des Knotens
*        expand_predecessors =                  " 'X': Vorgänger des Knotens expandieren
*        expand_subtree      =                  " 'X': alle Nachfahren expandieren
*        level_count         =                  " Anzahl zu expandierende Nachfolgeebenen
      EXCEPTIONS
        node_not_found      = 1                 " Knoten existiert nicht
        OTHERS              = 2
    ).
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'expand_node' iv_sysubrc = sy-subrc ).
    ENDIF.
  ENDMETHOD.
  METHOD node_set_style.
    mo_alv_tree->node_set_style(
      EXPORTING
        node_key       = iv_node_key     " Schlüssel des Knotens
        style          = iv_style         " siehe Methodendokumentation
      EXCEPTIONS
        node_not_found = 1                " Knoten existiert nicht
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'node_set_style' iv_sysubrc = sy-subrc ).
    ENDIF.
  ENDMETHOD.
  METHOD item_set_style.
    mo_alv_tree->item_set_style(
      EXPORTING
        node_key       = iv_node_key      " Schlüssel des Knotens
        item_name      = iv_item_name     " Name des Items
        style          = iv_style         " siehe Methodendokumentation
      EXCEPTIONS
        node_not_found = 1                " Knoten mit Schlüssel NODE_KEY nicht vorhanden
        item_not_found = 2                " Item mit Namen ITEM_NAME nicht vorhanden
        OTHERS         = 3
    ).
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'item_set_style' iv_sysubrc = sy-subrc ).
    ENDIF.
  ENDMETHOD.
  METHOD set_registered_evts.
    mo_alv_tree->set_registered_events(
      EXPORTING
        events                    = it_evt           " Eventtabelle
      EXCEPTIONS
        illegal_event_combination = 1                " ILLEGAL_EVENT_COMBINATION
        unknown_event             = 2                " "
        OTHERS                    = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_ctrl_error
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD set_selected_nkey.
    mo_alv_tree->set_selected_node(
      EXPORTING
        node_key                   = iv_nodekey       " Schlüssel des Knotens
      EXCEPTIONS
        single_node_selection_only = 1                " nur bei Knoten-Einfachselektion erlaubt
        node_not_found             = 2                " Knoten mit Schlüssel NODE_KEY nicht gefunden
        OTHERS                     = 3
    ).
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'set_selected_node' iv_sysubrc = sy-subrc ).
    ENDIF.
  ENDMETHOD.
  METHOD get_selected_nkey.
    mo_alv_tree->get_selected_node(
      IMPORTING
        node_key                   = rv_nkey " Schlüssel des selektierten Knotens
      EXCEPTIONS
        control_not_existing       = 1                " Tree Control existiert nicht
        control_dead               = 2                " Tree Control wurde bereits zerstört
        cntl_system_error          = 3                " "
        failed                     = 4                " Allgemeiner Fehler
        single_node_selection_only = 5                " nur bei Knoten-Einfachselektion erlaubt
        OTHERS                     = 6    ).
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'get_selected_node' iv_sysubrc = sy-subrc ).
    ENDIF.
  ENDMETHOD.
  METHOD collapse_node.
    mo_alv_tree->collapse_node(
        EXPORTING
          node_key         =  iv_nodekey     " Schlüssel des Knotens
*    collapse_subtree =                       " 'X': Nachfolger des Knotens schließen
        EXCEPTIONS
          node_not_found   = 1                " Knoten existiert nicht
          OTHERS           = 2
      ).
    IF sy-subrc <> 0.
      zcx_tree_error=>raise_from_sysubrc( iv_method = 'collapse_node' iv_sysubrc = sy-subrc ).
    ENDIF.
  ENDMETHOD.
**********************************************************************
*** Text-Edit Functions
  METHOD set_read_only_mode.
    me->mo_text_edit->set_readonly_mode(
      EXPORTING
        readonly_mode          = COND #( WHEN iv_read_only EQ abap_true THEN 1 ELSE 0 )             " readonly mode; eq 0: OFF ; ne 0: ON
      EXCEPTIONS
        error_cntl_call_method = 1                " Error while setting readonly mode!
        invalid_parameter      = 2                " INVALID_PARAMETER
        OTHERS                 = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_scrn_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD set_word_wrap.
    me->mo_text_edit->set_wordwrap_behavior(
      EXPORTING
        wordwrap_mode              = 0               " 0: OFF; 1: wrap a window border; 2: wrap at fixed position
*        wordwrap_position          = 256               " position of wordwrap, only makes sense with wordwrap_mode=2
        wordwrap_to_linebreak_mode = 1 "bool_initial     " eq 1: change wordwrap to linebreak; 0: preserve wordwraps
      EXCEPTIONS
        error_cntl_call_method     = 1                " Error while setting word wrap properties of control!
        OTHERS                     = 2
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_scrn_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD set_text_as_table.
    me->mo_text_edit->set_text_as_r3table(
      EXPORTING
        table           = it_text          " table with text
      EXCEPTIONS
        error_dp        = 1                " Error while sending R/3 table to TextEdit control!
        error_dp_create = 2                " ERROR_DP_CREATE
        OTHERS          = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_scrn_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD set_text_as_stream.
    me->mo_text_edit->set_text_as_stream(
      EXPORTING
        text            = it_text          " text as stream with carrige retruns and linefeeds
      exceptions
        error_dp        = 1                " ERROR_DP
        error_dp_create = 2                " ERROR_DP_CREATE
        others          = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_scrn_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD get_text_as_stream.
    mo_text_edit->get_text_as_stream(
*          EXPORTING
*            only_when_modified     = false           " get text only when modified
          IMPORTING
            text                  = rt_text          " text as R/3 table
*            is_modified            =                 " modify status of text
          EXCEPTIONS
            error_dp               = 1                " Error while retrieving text table via DataProvider control!
            error_cntl_call_method = 2                " Error while retrieving a property from TextEdit control
*            error_dp_create        = 3                " Error while creating DataProvider Control
*            potential_data_loss    = 4                " Potential data loss: use get_text_as_stream instead
            OTHERS                 = 5
        ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_scrn_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
