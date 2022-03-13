CLASS zcl_log_file DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_file TYPE fileintern iv_opsys TYPE opsys OPTIONAL RAISING zcx_file_err.
    METHODS:
      read_path_expanded RAISING zcx_file_err,
      read_path_expanded_p RAISING zcx_file_err.
    METHODS: read_path_expanded_w_fname RAISING zcx_file_err.
    METHODS: set_p1_from_comp IMPORTING it_comp TYPE zcl_dir_cont=>tt_comp,
      set_p1 IMPORTING iv_p1 TYPE string,
      set_p2 IMPORTING iv_p2 TYPE string,
      set_p3 IMPORTING iv_p3 TYPE string.
*    METHODS: set_file_name IMPORTING iv_file_name TYPE string.
    DATA: mv_phys_path     TYPE string READ-ONLY,
          mv_phys_file     TYPE string READ-ONLY,
          mv_path_expanded TYPE string READ-ONLY,
          mv_p1_sub_path   TYPE string READ-ONLY,
          mv_path_raw      TYPE string READ-ONLY,
          mv_log_path      TYPE pathintern READ-ONLY.
    DATA: mv_file_name TYPE string READ-ONLY,
          mv_file_raw  TYPE string READ-ONLY.
    DATA: mv_param_2 TYPE string READ-ONLY,
          mv_param_1 TYPE string READ-ONLY,
          mv_param_3 TYPE string READ-ONLY.
  PROTECTED SECTION.
    METHODS:
      read_phys_file_and_path RAISING zcx_file_err,
      read_file RAISING zcx_file_err,
      read_phys_path RAISING zcx_file_err,
      read_file_raw,
      read_path_raw.
    DATA: mv_log_file TYPE fileintern,
          mo_fs_path  TYPE REF TO cl_fs_path.
    DATA: mv_opsys   LIKE sy-opsys,
          mv_filesys TYPE opsystem-filesys.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_LOG_FILE IMPLEMENTATION.


  METHOD constructor.
*    mv_log_path = iv_path.
    mv_log_file = iv_file.
    mv_opsys = COND #( WHEN iv_opsys IS NOT INITIAL THEN iv_opsys ELSE sy-opsys ).
    me->read_phys_file_and_path( ).
    me->read_phys_path( ).
    me->read_file( ).
    me->read_path_raw( ).
    me->read_file_raw( ).
    mo_fs_path = cl_fs_path=>create(
                   name           = me->mv_phys_path
*                   force_absolute = abap_false
*                   path_kind      = ' '
                 ).
*                 CATCH cx_smart_path_syntax.
  ENDMETHOD.


  METHOD read_file.
    DATA: lv_file_name TYPE string.
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
*       client           = SY-MANDT         " Mandant zum Lesen der Dateinamentabelle
        logical_filename = mv_log_file       " Logischer Dateiname
*       operating_system = SY-OPSYS         " Betriebssystem
*       parameter_1      = space            " Parameter für Variable <PARAM_1>
*       parameter_2      = space            " Parameter für Variable <PARAM_2>
*       parameter_3      = space            " Parameter für Variable <PARAM_3>
*       use_presentation_server = space            " Betriebssystem des SAPtemu benutzen
*       with_file_extension     = space            " Dateiformat an Dateinamen hängen
*       use_buffer       = space            " Kennzeichen, ob gepuffert gearbeitet werden soll
*       eleminate_blanks = 'X'              " Leerzeichen eliminieren = 'X'
*       including_dir    = space            " Markierung für Eingabe in ein Dynpro-Feld
      IMPORTING
*       emergency_flag   =                  " Kennzeichen, ob Bedarfsfall gegeben war
*       file_format      =                  " Format der Datei
        file_name        = lv_file_name     " Physischer Dateiname
      EXCEPTIONS
        file_not_found   = 1                " Logischer Dateiname unbekannt
        OTHERS           = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_file_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD read_file_raw.
    SELECT SINGLE FROM filenameci
                FIELDS fileextern
                 WHERE fileintern = @mv_log_file
                  INTO @mv_file_raw.
  ENDMETHOD.


  METHOD read_path_expanded.
    DATA: lv_full TYPE string.
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
*       client           = SY-MANDT         " Mandant zum Lesen der Dateinamentabelle
        logical_filename = mv_log_file      " Logischer Dateiname
*       operating_system = SY-OPSYS         " Betriebssystem
        parameter_1      = mv_p1_sub_path   " Parameter für Variable <PARAM_1>
        parameter_2      = COND #( WHEN mv_param_2 IS NOT INITIAL THEN mv_param_2 ELSE space )            " Parameter für Variable <PARAM_2>
        parameter_3      = COND #( WHEN mv_param_3 IS NOT INITIAL THEN mv_param_3 ELSE space )            " Parameter für Variable <PARAM_3>
*       use_presentation_server = space     " Betriebssystem des SAPtemu benutzen
*       with_file_extension     = space     " Dateiformat an Dateinamen hängen
*       use_buffer       = space            " Kennzeichen, ob gepuffert gearbeitet werden soll
*       eleminate_blanks = 'X'              " Leerzeichen eliminieren = 'X'
*       including_dir    = space            " Markierung für Eingabe in ein Dynpro-Feld
      IMPORTING
*       emergency_flag   =                  " Kennzeichen, ob Bedarfsfall gegeben war
*       file_format      =                  " Format der Datei
        file_name        = mv_path_expanded " Physischer Dateiname
      EXCEPTIONS
        file_not_found   = 1                " Logischer Dateiname unbekannt
        OTHERS           = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_file_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*    CATCH zcx_file_err. " zcx_file_err
  ENDMETHOD.


  METHOD read_path_expanded_p.
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
*       client           = SY-MANDT         " Mandant zum Lesen der Dateinamentabelle
        logical_filename = mv_log_file      " Logischer Dateiname
*       operating_system = SY-OPSYS         " Betriebssystem
        parameter_1      = COND #( WHEN mv_param_1 IS NOT INITIAL THEN mv_param_1 ELSE space )   " Parameter für Variable <PARAM_1>
        parameter_2      = COND #( WHEN mv_param_2 IS NOT INITIAL THEN mv_param_2 ELSE space )            " Parameter für Variable <PARAM_2>
        parameter_3      = COND #( WHEN mv_param_3 IS NOT INITIAL THEN mv_param_3 ELSE space )            " Parameter für Variable <PARAM_3>
*       use_presentation_server = space     " Betriebssystem des SAPtemu benutzen
*       with_file_extension     = space     " Dateiformat an Dateinamen hängen
*       use_buffer       = space            " Kennzeichen, ob gepuffert gearbeitet werden soll
*       eleminate_blanks = 'X'              " Leerzeichen eliminieren = 'X'
*       including_dir    = space            " Markierung für Eingabe in ein Dynpro-Feld
      IMPORTING
*       emergency_flag   =                  " Kennzeichen, ob Bedarfsfall gegeben war
*       file_format      =                  " Format der Datei
        file_name        = mv_path_expanded " Physischer Dateiname
      EXCEPTIONS
        file_not_found   = 1                " Logischer Dateiname unbekannt
        OTHERS           = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_file_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD read_path_expanded_w_fname.
    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
*       client                     = SY-MANDT         " Mandant zum Lesen der Dateinamentabelle
        logical_path               = mv_log_path      " Logischer Pfad
        operating_system           = mv_opsys         " Betriebssystem
        parameter_1                = mv_p1_sub_path   " Parameter für Variable <PARAM_1>
*       parameter_2                = space            " Parameter für Variable <PARAM_2>
*       parameter_3                = space            " Parameter für Variable <PARAM_3>
*       use_buffer                 = space            " Kennzeichen, ob gepuffert gearbeitet werden soll
        file_name                  = mv_file_name     " Dateiname
*       use_presentation_server    = space            " Betriebssystem des SAPtemu benutzen
*       eleminate_blanks           = 'X'              " Leerzeichen eliminieren = 'X'
      IMPORTING
        file_name_with_path        = mv_path_expanded     " Dateiname mit Pfad
      EXCEPTIONS
        path_not_found             = 1                " Logischer Pfad unbekannt
        missing_parameter          = 2                " Fehlende Parameterübergabe
        operating_system_not_found = 3                " Betriebssystem unbekannt
        file_system_not_found      = 4                " Dateisystem unbekannt
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_file_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD read_path_raw.
    SELECT SINGLE FROM opsystem
                FIELDS filesys
                 WHERE opsys = @mv_opsys
                  INTO @mv_filesys.
    SELECT SINGLE FROM path
           FIELDS pathextern
            WHERE pathintern = @mv_log_path
             AND  filesys = @mv_filesys
             INTO @mv_path_raw.
  ENDMETHOD.


  METHOD read_phys_file_and_path.
    CALL FUNCTION 'FILE_GET_NAME_AND_LOGICAL_PATH'
      EXPORTING
*       client                     = SY-MANDT         " Mandant zum Lesen der Dateinamentabelle
        logical_filename           = mv_log_file      " Logischer Dateiname
*       operating_system           = SY-OPSYS         " Betriebssystem
*       parameter_1                = space            " Parameter für Variable <PARAM_1>
*       parameter_2                = space            " Parameter für Variable <PARAM_2>
*       parameter_3                = space            " Parameter für Variable <PARAM_3>
*       use_presentation_server    = space            " Betriebssystem des SAPtemu benutzen
*       with_file_extension        = space            " Dateiformat an Dateinamen hängen
*       use_buffer                 = space            " Kennzeichen, ob gepuffert gearbeitet werden soll
*       eleminate_blanks           = 'X'              " Leerzeichen eliminieren = 'X'
      IMPORTING
*       file_format                =                  " Format der Datei
        file_name                  = mv_phys_file     " Physischer Dateiname
        logical_path               = mv_log_path      " Logischer Pfad
      EXCEPTIONS
        file_not_found             = 1                " Logischer Dateiname unbekannt
        operating_system_not_found = 2                " Betriebssystem unbekannt
        file_system_not_found      = 3                " Dateisystem unbekannt
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_file_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD read_phys_path.
    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
*       client                     = SY-MANDT         " Mandant zum Lesen der Dateinamentabelle
        logical_path               = mv_log_path      " Logischer Pfad
*       operating_system           = SY-OPSYS         " Betriebssystem
*       parameter_1                = 'subpath'        " Parameter für Variable <PARAM_1>
*       parameter_2                = space            " Parameter für Variable <PARAM_2>
*       parameter_3                = space            " Parameter für Variable <PARAM_3>
*       use_buffer                 = space            " Kennzeichen, ob gepuffert gearbeitet werden soll
        file_name                  = '__to_replace__.txt'       " Dateiname
*       use_presentation_server    = space            " Betriebssystem des SAPtemu benutzen
*       eleminate_blanks           = 'X'              " Leerzeichen eliminieren = 'X'
      IMPORTING
        file_name_with_path        = mv_phys_path     " Dateiname mit Pfad
      EXCEPTIONS
        path_not_found             = 1                " Logischer Pfad unbekannt
        missing_parameter          = 2                " Fehlende Parameterübergabe
        operating_system_not_found = 3                " Betriebssystem unbekannt
        file_system_not_found      = 4                " Dateisystem unbekannt
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_file_err
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    REPLACE ALL OCCURRENCES OF '__to_replace__.txt$' IN mv_phys_path WITH space.
  ENDMETHOD.


  METHOD set_p1.
    mv_param_1 = iv_p1.
  ENDMETHOD.


  METHOD set_p1_from_comp.
    CLEAR mv_p1_sub_path.
    LOOP AT it_comp ASSIGNING FIELD-SYMBOL(<comp>).
      CASE sy-tabix.
        WHEN 1.
          mv_p1_sub_path = |{ <comp> }|.
        WHEN OTHERS.
          mv_p1_sub_path = |{ mv_p1_sub_path }{ mo_fs_path->parse_separator_actual }{ <comp> }|.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_p2.
    mv_param_2 = iv_p2.
  ENDMETHOD.


  METHOD set_p3.
    mv_param_3 = iv_p3.
  ENDMETHOD.
ENDCLASS.
