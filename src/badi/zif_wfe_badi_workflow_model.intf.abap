INTERFACE zif_wfe_badi_workflow_model
  PUBLIC .


  INTERFACES if_badi_interface .

  TYPES:
    BEGIN OF ts_available_status,
      status   TYPE zwfe_e_status,
      complete TYPE zwfe_t003-complete,
      init     TYPE zwfe_t003-init,
      reject   TYPE zwfe_t003-reject,
    END OF ts_available_status .
  TYPES:
    tt_available_status TYPE STANDARD TABLE OF ts_available_status WITH EMPTY KEY .

  "! <p class="shorttext synchronized">Change step of approver</p>
  "! @parameter iv_status | <p class="shorttext synchronized">Status</p>
  "! @parameter it_values | <p class="shorttext synchronized">Values</p>
  "! @parameter iv_step_owner | <p class="shorttext synchronized">Owner of step</p>
  "! @parameter ct_approvers | <p class="shorttext synchronized">Approvers</p>
  METHODS change_step_approvers
    IMPORTING
      !iv_status     TYPE zwfe_e_status
      !it_values     TYPE zwfe_i_values_wf
      !iv_step_owner TYPE zwfe_e_owner
    CHANGING
      !ct_approvers  TYPE zif_wfe_workflow=>tt_approvers .
  "! <p class="shorttext synchronized">Post executing save data</p>
  "! @parameter io_wf_model | <p class="shorttext synchronized">Instance of workflow model</p>
  METHODS post_save_workflow
    IMPORTING
      !io_wf_model TYPE REF TO zif_wfe_workflow OPTIONAL .
  "! <p class="shorttext synchronized">Determine next step</p>
  "! @parameter iv_actual_status | <p class="shorttext synchronized">Actual status</p>
  "! @parameter iv_step_result | <p class="shorttext synchronized">Step result</p>
  "! @parameter it_next_status_available | <p class="shorttext synchronized">Next status availables</p>
  "! @parameter cv_next_status | <p class="shorttext synchronized">Next status</p>
  "! @parameter cv_workflow_completed | <p class="shorttext synchronized">Workflow completed</p>
  METHODS determine_next_status
    IMPORTING
      !iv_actual_status         TYPE zwfe_e_status
      !it_values                TYPE zwfe_i_values_wf
      !it_next_status_available TYPE tt_available_status
      !iv_step_result           TYPE zwfe_e_step_result
    CHANGING
      !cv_workflow_completed    TYPE sap_bool
      !cv_next_status           TYPE zwfe_e_status_next .
ENDINTERFACE.
