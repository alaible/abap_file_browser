class ZCX_SCRN_ERR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_SCRN_ERR,
      msgid type symsgid value 'ZFILE_ERR',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SCRN_ERR .
  constants:
    begin of GEN_ERROR,
      msgid type symsgid value 'ZFILE_ERR',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value 'MV_MSGV4',
    end of GEN_ERROR .
  data MV_MSGV1 type SYMSGV .
  data MV_MSGV2 type SYMSGV .
  data MV_MSGV3 type SYMSGV .
  data MV_MSGV4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional .
  class-methods RAISE_FROM_MSG
    importing
      !IV_MSG type STRING
    raising
      ZCX_SCRN_ERR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_SCRN_ERR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_MSGV1 = MV_MSGV1 .
me->MV_MSGV2 = MV_MSGV2 .
me->MV_MSGV3 = MV_MSGV3 .
me->MV_MSGV4 = MV_MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_SCRN_ERR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD raise_from_msg.
    DATA(err_msg) = zcl_file_scr_utils=>string_to_err_var( iv_msg ).
    RAISE EXCEPTION TYPE zcx_scrn_err
      EXPORTING
        textid   = zcx_scrn_err=>gen_error
        mv_msgv1 = err_msg-var1
        mv_msgv2 = err_msg-var2
        mv_msgv3 = err_msg-var3
        mv_msgv4 = err_msg-var4.
  ENDMETHOD.
ENDCLASS.
