@AbapCatalog.sqlViewName: 'ZWFEIHEADER'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'WFE - CDS for header data'
@VDM.viewType: #BASIC
define view ZWFE_CDS_HEADER
with parameters p_langu:spras 
as select from zwfe_t007 as header
left outer join zwfe_t001t as workflow
    on header.workflow = workflow.workflow 
    and workflow.spras = :p_langu
left outer join zwfe_t002t as status
    on header.status = status.status
    and status.spras = :p_langu
left outer join dd07v as wf_status
on wf_status.domname = 'ZWFE_D_WF_STATUS'
and wf_status.ddlanguage = :p_langu
and header.wf_status = wf_status.domvalue_l
            
    {
    key header.wf_id as wf_id,
    header.workflow as workflow,
    workflow.description as workflow_desc,
    workflow.description_short as workflow_desc_short,
    header.status as status,
    status.description as status_desc,
    header.wf_status as wf_status,
    wf_status.ddtext as wf_status_desc,
    header.erdat as erdat,
    header.ernam as ernam,
    header.ernam_process as ernam_process,
    header.erzet as erzet,
    header.aedat as aedat,    
    header.aenam as aenam,
    header.aenam_process as aenam_process,
    header.aetim as aetim    
    
}
