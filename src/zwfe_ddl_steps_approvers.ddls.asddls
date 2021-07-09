@AbapCatalog.sqlViewName: 'ZWFEISTEPSAPRV'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'WFE - CDS for steps approvers'
@VDM.viewType: #BASIC
define view ZWFE_DDL_STEPS_APPROVERS
with parameters p_langu:spras  
as select from zwfe_t012 as step_approver
 left outer join zwfe_t002t as status
    on step_approver.status = status.status
    and status.spras = :p_langu
 {
    key step_approver.wf_id as wf_id,
    key step_approver.status as status,
    key step_approver.approver as approver,
    status.description as status_desc
    
}
