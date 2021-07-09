interface ZIF_WFE_WORKFLOW
  public .


  types:
    BEGIN OF ts_result_new,
      wf_id  TYPE zwfe_e_wf_id,
      status TYPE zwfe_e_status,
    END OF ts_result_new .
  types:
    BEGIN OF ts_approvers,
      approver TYPE zwfe_e_approver,
    END OF ts_approvers .
  types:
    tt_approvers TYPE STANDARD TABLE OF ts_approvers WITH EMPTY KEY .

  "! <p class="shorttext synchronized">New workflow</p>
  methods NEW
    importing
      !IT_VALUES type ZWFE_I_VALUES_WF
      !IV_TEST_MODE type SAP_BOOL default ABAP_FALSE
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
      !IV_PROCESS_USER type ZWFE_E_PROCESS_USER optional
    exporting
      !ES_RESULT type TS_RESULT_NEW
      !ET_APPROVERS type TT_APPROVERS
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
  "! <p class="shorttext synchronized">New draft workflow</p>
  methods NEW_DRAFT
    importing
      !IT_VALUES type ZWFE_I_VALUES_WF
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
      !IV_PROCESS_USER type ZWFE_E_PROCESS_USER optional
    exporting
      !ES_RESULT type TS_RESULT_NEW
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
  "! <p class="shorttext synchronized">Determine approvers of a status</p>
  methods DETERMINE_STATUS_APPROVERS
    importing
      !IV_STATUS type ZWFE_E_STATUS
      !IT_VALUES type ZWFE_I_VALUES_WF
    exporting
      !ET_APPROVERS type TT_APPROVERS
    raising
      ZCX_WFE .
  "! <p class="shorttext synchronized">Get approvers of the active step</p>
  methods GET_APPROVERS_STEP_ACTIVE
    exporting
      !ET_APPROVERS type TT_APPROVERS .
  "! <p class="shorttext synchronized">Get initial status</p>
  methods GET_INITIAL_STATUS
    returning
      value(RV_STATUS) type ZWFE_E_STATUS .
  "! <p class="shorttext synchronized">Get active status</p>
  methods GET_ACTIVE_STATUS
    returning
      value(RV_STATUS) type ZWFE_E_STATUS .
  "! <p class="shorttext synchronized">Approve or reject the step</p>
  methods CONTINUE_NEXT_STEP
    importing
      !IV_STEP_RESULT type ZWFE_E_STEP_RESULT
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
      !IV_PROCESS_USER type ZWFE_E_PROCESS_USER optional
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN
      !EV_NEXT_STATUS type ZWFE_E_STATUS
      !ET_APPROVERS type TT_APPROVERS
      !EV_WF_COMPLETED type SAP_BOOL .
  "! <p class="shorttext synchronized">Get data of model</p>
  methods GET_DATA
    exporting
      !ES_HEADER type ZWFE_S_HEADER_ALL_FIELDS
      !ET_VALUES type ZWFE_I_VALUES_ALL_FIELDS
      !ET_STEPS type ZWFE_I_STEPS_ALL_FIELDS
      !ET_STEPS_APPROVERS type ZWFE_I_STEPS_APPROV_ALL_FIELDS .
  "! <p class="shorttext synchronized">Is status or active status draft?</p>
  methods IS_DRAFT
    importing
      !IV_STATUS type ZWFE_E_STATUS optional
    returning
      value(RV_IS) type SAP_BOOL .
  "! <p class="shorttext synchronized">Is workflow completed?</p>
  methods IS_COMPLETED
    returning
      value(RV_IS) type SAP_BOOL .
  "! <p class="shorttext synchronized">Is workflow rejected?</p>
  methods IS_REJECTED
    returning
      value(RV_IS) type SAP_BOOL .
  "! <p class="shorttext synchronized">Update values</p>
  methods UPDATE_VALUES
    importing
      !IT_VALUES type ZWFE_I_VALUES_WF
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
      !IV_PROCESS_USER type ZWFE_E_PROCESS_USER optional
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
endinterface.
