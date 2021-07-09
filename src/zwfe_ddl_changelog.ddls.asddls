@AbapCatalog.sqlViewName: 'ZWFEICHNGLOG'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'WFE - CDS for change log'
define view ZWFE_DDL_CHANGELOG
with parameters p_langu:spras  
as select from zwfe_t009 as header
inner join zwfe_t010 as values 
on values.wf_id = header.wf_id
and values.change_id = header.change_id 
left outer join dd07v as change_ind
on change_ind.domname = 'ZWFE_D_CHANGE_IND'
and change_ind.ddlanguage = :p_langu
and header.change_ind = change_ind.domvalue_l  
left outer join dd07v as sources
on sources.domname = 'ZWFE_D_CHANGELOG_SOURCE'
and sources.ddlanguage = :p_langu
and values.source = sources.domvalue_l 
left outer join dd07v as chng_ind_field
on chng_ind_field.domname = 'CDCHNGIND'
and chng_ind_field.ddlanguage = :p_langu
and values.chngind = chng_ind_field.domvalue_l 
{
        header.wf_id,
        header.change_id,
        header.change_ind,
        change_ind.ddtext as change_ind_desc,
        header.ernam,
        header.erzet,
        header.erdat,
        header.ernam_process,       
        values.source,
        sources.ddtext as source_desc,
        values.counter,
        values.field,
        values.chngind,
        chng_ind_field.ddtext as chngind_desc,
        values.value_new,
        values.value_old
}
