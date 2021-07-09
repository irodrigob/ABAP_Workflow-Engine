@AbapCatalog.sqlViewName: 'ZWFEIVALUES'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'WFE - CDS for values data'
@VDM.viewType: #BASIC
define view ZWFE_DDL_VALUES as select from zwfe_t008 {
    key wf_id,
    key counter,
    key field,
    value
}
