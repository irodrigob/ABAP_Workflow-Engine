interface ZIF_WFE_BADI_WORKFLOW_MODEL
  public .


  interfaces IF_BADI_INTERFACE .

  types:
    BEGIN OF ts_available_status,
      status  TYPE zwfe_e_status,
      complete type zwfe_t003-complete,
      init type zwfe_t003-init,
      reject type zwfe_t003-reject,
    END OF ts_available_status .
  types:
    tt_available_status TYPE STANDARD TABLE OF ts_available_status WITH EMPTY KEY .

  "! <p class="shorttext synchronized">Change step of approver</p>
  methods CHANGE_STEP_APPROVERS
    importing
      !IV_STATUS type ZWFE_E_STATUS
      !IT_VALUES type ZWFE_I_VALUES_WF
    changing
      !CT_APPROVERS type ZIF_WFE_WORKFLOW=>TT_APPROVERS .
  methods POST_SAVE_WORKFLOW
    importing
      !IO_WF_MODEL type ref to ZIF_WFE_WORKFLOW optional .
  "! <p class="shorttext synchronized">Determine next step</p>
  methods DETERMINE_NEXT_STEP
    importing
      !IV_ACTUAL_STATUS type ZWFE_E_STATUS
      !IT_VALUES type ZWFE_I_VALUES_WF
      !IT_STATUS_AVAILABLE_STATUS type TT_AVAILABLE_STATUS
      !IV_STEP_RESULT type ZWFE_E_STEP_RESULT
    changing
      !CV_WORKFLOW_COMPLETED type SAP_BOOL
      !CV_NEXT_STATUS type ZWFE_E_STATUS_NEXT .
endinterface.
