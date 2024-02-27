INTERFACE zif_wfe_workflow
  PUBLIC .


  TYPES:
    BEGIN OF ts_result_new,
      wf_id  TYPE zwfe_e_wf_id,
      status TYPE zwfe_e_status,
    END OF ts_result_new .
  TYPES:
    BEGIN OF ts_approvers,
      approver TYPE zwfe_e_approver,
    END OF ts_approvers .
  TYPES:
    tt_approvers TYPE STANDARD TABLE OF ts_approvers WITH EMPTY KEY .

  "! <p class="shorttext synchronized">New workflow</p>
  "! @parameter it_values | <p class="shorttext synchronized">Workflow values</p>
  "! @parameter iv_commit | <p class="shorttext synchronized">Do commit</p>
  "! @parameter iv_process_user | <p class="shorttext synchronized">user process</p>
  "! @parameter iv_test_mode | <p class="shorttext synchronized">Test mode</p>
  "! @parameter es_result | <p class="shorttext synchronized">Process result</p>
  "! @parameter et_approvers | <p class="shorttext synchronized">ID Workflow</p>
  "! @parameter et_return | <p class="shorttext synchronized">Return process</p>
  METHODS new
    IMPORTING
      !it_values       TYPE zwfe_i_values_wf
      !iv_test_mode    TYPE sap_bool DEFAULT abap_false
      !iv_commit       TYPE sap_bool DEFAULT abap_true
      !iv_process_user TYPE zwfe_e_process_user OPTIONAL
    EXPORTING
      !es_result       TYPE ts_result_new
      !et_approvers    TYPE tt_approvers
      !et_return       TYPE zif_wfe_data=>tt_return .
  "! <p class="shorttext synchronized">New draft workflow</p>
  "! @parameter it_values | <p class="shorttext synchronized">Workflow values</p>
  "! @parameter iv_commit | <p class="shorttext synchronized">Do commit</p>
  "! @parameter iv_process_user | <p class="shorttext synchronized">User process</p>
  "! @parameter es_result | <p class="shorttext synchronized">Resultado del proceso</p>
  "! @parameter et_approvers | <p class="shorttext synchronized">Workflow ID</p>
  "! @parameter et_return | <p class="shorttext synchronized">Return process</p>
  METHODS new_draft
    IMPORTING
      !it_values       TYPE zwfe_i_values_wf
      !iv_commit       TYPE sap_bool DEFAULT abap_true
      !iv_process_user TYPE zwfe_e_process_user OPTIONAL
    EXPORTING
      !es_result       TYPE ts_result_new
      !et_return       TYPE zif_wfe_data=>tt_return .
  "! <p class="shorttext synchronized">Determine approvers of a status</p>
  "! @parameter it_values | <p class="shorttext synchronized">Workflow values</p>
  "! @parameter iv_status | <p class="shorttext synchronized">Status</p>
  "! @parameter et_approvers | <p class="shorttext synchronized">ID Workflow</p>
  METHODS determine_status_approvers
    IMPORTING
      !iv_status    TYPE zwfe_e_status
      !it_values    TYPE zwfe_i_values_wf
    EXPORTING
      !et_approvers TYPE tt_approvers
    RAISING
      zcx_wfe .
  "! <p class="shorttext synchronized">Get approvers of the active step</p>
  "! @parameter et_approvers | <p class="shorttext synchronized">Approvers</p>
  METHODS get_approvers_step_active
    EXPORTING
      !et_approvers TYPE tt_approvers .
  "! <p class="shorttext synchronized">Get initial status</p>
  "! @parameter rv_status | <p class="shorttext synchronized">Status</p>
  METHODS get_initial_status
    RETURNING
      VALUE(rv_status) TYPE zwfe_e_status .
  "! <p class="shorttext synchronized">Get active status</p>
  "! @parameter rv_status | <p class="shorttext synchronized">Status</p>
  METHODS get_active_status
    RETURNING
      VALUE(rv_status) TYPE zwfe_e_status .
  "! <p class="shorttext synchronized">Approve or reject the step</p>
  "! @parameter iv_step_result | <p class="shorttext synchronized">Result of step</p>
  "! @parameter iv_commit | <p class="shorttext synchronized">Do commit</p>
  "! @parameter iv_process_user | <p class="shorttext synchronized">User process</p>
  "! @parameter ev_next_status | <p class="shorttext synchronized">Next status</p>
  "! @parameter et_approvers | <p class="shorttext synchronized">ID workflow</p>
  "! @parameter et_return | <p class="shorttext synchronized">Process return</p>
  "! @parameter ev_wf_completed | <p class="shorttext synchronized">Workflow completed</p>
  METHODS continue_next_step
    IMPORTING
      !iv_step_result  TYPE zwfe_e_step_result
      !iv_commit       TYPE sap_bool DEFAULT abap_true
      !iv_process_user TYPE zwfe_e_process_user OPTIONAL
    EXPORTING
      !et_return       TYPE zif_wfe_data=>tt_return
      !ev_next_status  TYPE zwfe_e_status
      !et_approvers    TYPE tt_approvers
      !ev_wf_completed TYPE sap_bool .
  "! <p class="shorttext synchronized">Get data of model</p>
  "! @parameter es_header | <p class="shorttext synchronized">Header data</p>
  "! @parameter et_values | <p class="shorttext synchronized">Values</p>
  "! @parameter et_steps | <p class="shorttext synchronized">Steps</p>
  "! @parameter et_steps_approvers | <p class="shorttext synchronized">Approvers steps</p>
  METHODS get_data
    EXPORTING
      !es_header          TYPE zwfe_s_header_all_fields
      !et_values          TYPE zwfe_i_values_all_fields
      !et_steps           TYPE zwfe_i_steps_all_fields
      !et_steps_approvers TYPE zwfe_i_steps_approv_all_fields .
  "! <p class="shorttext synchronized">Is status or active status draft?</p>
  "! @parameter rv_status | <p class="shorttext synchronized">Status</p>
  METHODS is_draft
    IMPORTING
      !iv_status   TYPE zwfe_e_status OPTIONAL
    RETURNING
      VALUE(rv_is) TYPE sap_bool .
  "! <p class="shorttext synchronized">Is workflow completed?</p>
  "! @parameter rv_is | <p class="shorttext synchronized">Is completed</p>
  METHODS is_completed
    RETURNING
      VALUE(rv_is) TYPE sap_bool .
  "! <p class="shorttext synchronized">Is workflow rejected?</p>
  "! @parameter rv_is | <p class="shorttext synchronized">Is rejected</p>
  METHODS is_rejected
    RETURNING
      VALUE(rv_is) TYPE sap_bool .
  "! <p class="shorttext synchronized">Update values</p>
  "! @parameter it_values | <p class="shorttext synchronized">Values</p>
  "! @parameter iv_commit | <p class="shorttext synchronized">Do commit</p>
  "! @parameter iv_process_user | <p class="shorttext synchronized">User process</p>
  "! @parameter et_return | <p class="shorttext synchronized">Process return</p>
  METHODS update_values
    IMPORTING
      !it_values       TYPE zwfe_i_values_wf
      !iv_commit       TYPE sap_bool DEFAULT abap_true
      !iv_process_user TYPE zwfe_e_process_user OPTIONAL
    EXPORTING
      !et_return       TYPE zif_wfe_data=>tt_return .
ENDINTERFACE.
