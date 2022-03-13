class ZCX_TREE_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_TREE_ERROR,
      msgid type symsgid value 'ZFILE_ERR',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TREE_ERROR .
  constants:
    begin of TREE_ERROR,
      msgid type symsgid value 'ZFILE_ERR',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MV_CLASS',
      attr2 type scx_attrname value 'MV_EXCEPTION',
      attr3 type scx_attrname value 'MV_EXCEPTION',
      attr4 type scx_attrname value '',
    end of TREE_ERROR .
  data MV_CLASS type SYMSGV .
  data MV_METHOD type SYMSGV .
  data MV_EXCEPTION type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_CLASS type SYMSGV optional
      !MV_METHOD type SYMSGV optional
      !MV_EXCEPTION type SYMSGV optional .
  class-methods RAISE_FROM_SYSUBRC
    importing
      !IV_CLASSNAME type STRING optional
      !IV_METHOD type STRING
      !IV_SYSUBRC like SY-SUBRC
    raising
      ZCX_TREE_ERROR .
protected section.
private section.

  class-methods RAISE_FROM_SYSUBRC_INT
    importing
      !IV_CLASSNAME type STRING
      !IV_METHOD type STRING
      !IV_SYSUBRC like SY-SUBRC
    raising
      ZCX_TREE_ERROR .
ENDCLASS.



CLASS ZCX_TREE_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_CLASS = MV_CLASS .
me->MV_METHOD = MV_METHOD .
me->MV_EXCEPTION = MV_EXCEPTION .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_TREE_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD raise_from_sysubrc.
    zcx_tree_error=>raise_from_sysubrc_int(
      EXPORTING
        iv_classname = to_upper( iv_classname )
        iv_method    = to_upper( iv_method )
        iv_sysubrc   = iv_sysubrc
    ).
*    CATCH zcx_tree_error.
  ENDMETHOD.


  METHOD raise_from_sysubrc_int.
    DATA: lv_exception TYPE string.
**********************************************************************
    SELECT SINGLE FROM seosubco AS exc
            INNER JOIN seosubcodf AS df
                    ON exc~clsname = df~clsname
                   AND exc~cmpname = df~cmpname
                   AND exc~sconame = df~sconame
                FIELDS exc~sconame AS exception
                 WHERE exc~scotype = '1'
                   AND exc~clsname = @iv_classname
                   AND exc~cmpname = @iv_method
                   AND df~editorder = @iv_sysubrc
                  INTO @lv_exception.
    IF sy-subrc <> 0.
*** In case there is noch result -> return exception with default text
      RAISE EXCEPTION TYPE zcx_tree_error.
    ELSE.
      RAISE EXCEPTION TYPE zcx_tree_error
        EXPORTING
          textid       = zcx_tree_error=>tree_error
          mv_class     = CONV #( iv_classname )
          mv_method    = CONV #( iv_method )
          mv_exception = CONV #( lv_exception ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
