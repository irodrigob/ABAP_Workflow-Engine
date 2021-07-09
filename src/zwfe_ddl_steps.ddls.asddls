@AbapCatalog.sqlViewName: 'ZWFEISTEPS'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'WFE - CDS for steps'
@VDM.viewType: #BASIC
define view ZWFE_DDL_STEPS
with parameters p_langu:spras 
 as select from zwfe_t011 as step
 left outer join zwfe_t002t as status
    on step.status = status.status
    and status.spras = :p_langu
left outer join dd07v as step_status
on step_status.domname = 'ZWFE_D_STEP_STATUS'
and step_status.ddlanguage = :p_langu
and step.step_status = step_status.domvalue_l 
 {
   key step.wf_id as wf_id,
   key step.status as status, 
   status.description as status_desc,
   step.step_status as step_status,
   step_status.ddtext as step_status_desc,
   step.approved_by as approved_by,
   step.is_backup as is_backup,
   step.erdat as erdat,
   step.ernam as ernam,
   step.ernam_process as ernam_process,
   step.erzet as erzet,
   step.aedat as aedat,    
   step.aenam as aenam,
   step.aenam_process as aenam_process,
   step.aetim as aetim    
    
}
