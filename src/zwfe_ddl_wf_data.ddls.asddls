@AbapCatalog.sqlViewName: 'ZWFECWFDATA'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'WFE - CDS for workflow data'
@VDM.viewType: #COMPOSITE
define view ZWFE_DDL_WF_DATA
with parameters p_langu:spras  
as select from ZWFE_CDS_HEADER(p_langu : :p_langu) as header
left outer join zwfeivalues as values
    on header.wf_id = values.wf_id 
left outer join ZWFE_DDL_STEPS(p_langu : :p_langu) as steps
    on header.wf_id = steps.wf_id   
left outer join ZWFE_DDL_STEPS_APPROVERS(p_langu : :p_langu) as steps_approvers
    on steps.wf_id = steps_approvers.wf_id   
    and steps.status = steps_approvers.status
    {
    
    // Header data
    header.wf_id as header_wf_id,
    header.status as header_status,
    header.status_desc as header_status_desc,
    header.wf_status as header_wf_status,
    header.status_desc as header_wf_status_desc,
    header.workflow as header_workflow,
    header.workflow_desc as header_workflow_desc,
    header.workflow_desc_short as header_workflow_desc_short,
    header.erdat as header_erdat,
    header.ernam as header_ernam,
    header.ernam_process as header_ernam_process,
    header.erzet as header_erzet,
    header.aedat as header_aedat,    
    header.aenam as header_aenam,
    header.aenam_process as header_aenam_process,
    header.aetim as header_aetim,
    
    // Values
    values.wf_id as values_wf_id,
    values.counter as values_counter,
    values.field as values_field,
    values.value as values_value,
    
    // Steps
    steps.wf_id as steps_wf_id,
    steps.status as steps_status,
    steps.status_desc as steps_status_desc,
    steps.step_status as steps_step_status,
    steps.step_status_desc as steps_step_status_desc,
    steps.approved_by as steps_approved_by,
    steps.is_backup as steps_is_backup,
    steps.erdat as steps_erdat,
    steps.ernam as steps_ernam,
    steps.ernam_process as steps_ernam_process,
    steps.erzet as steps_erzet,
    steps.aedat as steps_aedat,    
    steps.aenam_process as steps_aenam_process,
    steps.aetim as steps_aetim,
    
    // Steps approvers
    steps_approvers.wf_id as stepsapprovers_wf_id,
    steps_approvers.status as stepsapprovers_status,
    steps_approvers.status_desc as stepsapprovers_status_desc,    
    steps_approvers.approver as stepsapprovers_approver
    
}
