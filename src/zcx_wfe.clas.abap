class ZCX_WFE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ERROR_SAVE_HEADER_DATA,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_SAVE_HEADER_DATA .
  constants:
    begin of ERROR_SAVE_VALUES_DATA,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_SAVE_VALUES_DATA .
  constants:
    begin of ERROR_SAVE_CHANGELOG,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_SAVE_CHANGELOG .
  constants:
    begin of WF_ID_NOT_EXIST,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of WF_ID_NOT_EXIST .
  constants:
    begin of ERROR_SAVE_STEPS_DATA,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_SAVE_STEPS_DATA .
  constants:
    begin of ERROR_SAVE_STEPS_USER_DATA,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_SAVE_STEPS_USER_DATA .
  constants:
    begin of WORKFLOW_NAME_NOT_EXIST,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of WORKFLOW_NAME_NOT_EXIST .
  constants:
    begin of WORKFLOW_NOT_STATUS_CONF,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of WORKFLOW_NOT_STATUS_CONF .
  constants:
    begin of WORKFLOW_NOT_OWNER,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of WORKFLOW_NOT_OWNER .
  constants:
    begin of OWNER_CONF_INCOMPLETE,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '015',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OWNER_CONF_INCOMPLETE .
  constants:
    begin of DRAFT_STATUS_NOT_CONFIGURED,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '016',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DRAFT_STATUS_NOT_CONFIGURED .
  constants:
    begin of STATUS_NOT_CONFIGURED,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '017',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of STATUS_NOT_CONFIGURED .
  constants:
    begin of WORKFLOW_NOT_ACTIVE,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '021',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of WORKFLOW_NOT_ACTIVE .
  constants:
    begin of NOT_ACTIVE_STEPS,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '022',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOT_ACTIVE_STEPS .
  constants:
    begin of NOT_NEXT_STEP,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '023',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOT_NEXT_STEP .
  constants:
    begin of STATUS_BADI_NOT_VALID,
      msgid type symsgid value 'ZWFE',
      msgno type symsgno value '027',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of STATUS_BADI_NOT_VALID .
  data MV_MSGV1 type STRING .
  data MV_MSGV2 type STRING .
  data MV_MSGV3 type STRING .
  data MV_MSGV4 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MSGV1 type STRING optional
      !MV_MSGV2 type STRING optional
      !MV_MSGV3 type STRING optional
      !MV_MSGV4 type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_WFE IMPLEMENTATION.


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
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
