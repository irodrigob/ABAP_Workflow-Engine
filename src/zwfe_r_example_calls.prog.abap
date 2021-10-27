*&---------------------------------------------------------------------*
*& Report  ZWFE_R_EXAMPLE_CALLS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZWFE_R_EXAMPLE_CALLS.

*----------------------------------------------------
* NUEVO WORKFLOW
*----------------------------------------------------
*DATA(lo_wf) = NEW zcl_wfe_workflow_engine( ).
*
*lo_wf->new_workflow(
*  EXPORTING
*    iv_workflow = 'MY_DATA'
*    iv_process_user = 'ivan.rodrigo@enzymeadvisinggroup.com'
*    it_values   =  VALUE #( ( field = 'MATERIAL' value = '22434' )
*                            ( field = 'CUSTOMER' value = '000223E' )
*                            ( field = 'CSR' value = '01223030' )
*                            ( field = 'AM' value = '22200' )
*                            ( field = 'KUNNR' value = '2343434' )
*                            ( counter = 1 field = 'CHEM_COMP_CHEMICALID' value = '5' )
*                            ( counter = 1 field = 'CHEM_COMP_MIN' value = '1' )
*                            ( counter = 1 field = 'CHEM_COMP_MAX' value = '5' )
*                            ( counter = 1 field = 'CHEM_COMP_UNIT' value = '%' )
*                            ( counter = 2 field = 'CHEM_COMP_CHEMICALID' value = '9' )
*                            ( counter = 2 field = 'CHEM_COMP_MIN' value = '10' )
*                            ( counter = 2 field = 'CHEM_COMP_MAX' value = '15' )
*                            ( counter = 2 field = 'CHEM_COMP_UNIT' value = 'KG' )
*                             )
*    iv_draft    = abap_false
*  IMPORTING
*    ev_wf_id    = DATA(lv_wf_id)
*    et_return   = DATA(lt_return) ).
*
*WRITE:/ lv_wf_id.

*------------------------------------------------------
* QUERY
*------------------------------------------------------

*DATA(lo_query) = NEW zcl_wfe_model_data_query( ).
*
*lo_query->get_all_data(
*  EXPORTING
*    it_params_sl       = VALUE #( ( selname = zif_wfe_data=>cs_model_data-header-fields-workflow kind = 'S' sign = 'I' option = 'EQ' low = 'PROFILE' )
*                                  ( selname = zif_wfe_data=>cs_model_data-header-fields-wf_status kind = 'S' sign = 'I' option = 'EQ' low = zif_wfe_data=>cs_wf_process-wf_status-active )
*                                  ( selname = zif_wfe_data=>cs_model_data-values-fields-field kind = 'S'  sign = 'I' option = 'EQ' low = 'KUNNR'  )
*                                  ( selname = zif_wfe_data=>cs_model_data-values-fields-value kind = 'S'  sign = 'I' option = 'EQ' low = '2343434'  ) )
*  IMPORTING
*    et_all_data        = DATA(lt_all_data)
*    et_header          = DATA(lt_header)
*    et_steps           = DATA(lt_steps)
*    et_steps_approvers = DATA(lt_steps_approvers)
*    et_values          = DATA(lt_values) ).

*lo_query->get_all_data(
*  EXPORTING
*    it_params_sl       = VALUE #( ( selname = zif_wfe_data=>cs_model_data-header-fields-workflow kind = 'S' sign = 'I' option = 'EQ' low = 'PROFILE' )
*                                  ( selname = zif_wfe_data=>cs_model_data-header-fields-wf_status kind = 'S' sign = 'I' option = 'EQ' low = zif_wfe_data=>cs_wf_process-wf_status-active )
*                                  ( selname = zif_wfe_data=>cs_model_data-steps_approvers-fields-approver kind = 'S'  sign = 'I' option = 'EQ' low = 'US_TRADEBE_APPROVALS_CHEMIST'  )
*                                  ( selname = zif_wfe_data=>cs_model_data-values-fields-field kind = 'S'  sign = 'I' option = 'EQ' low = 'KUNNR'  )
*                                  ( selname = zif_wfe_data=>cs_model_data-values-fields-value kind = 'S'  sign = 'I' option = 'EQ' low = '2343434'  )
*                                   )
*  IMPORTING
*    et_all_data        = DATA(lt_all_data)
*    et_header          = DATA(lt_header)
*    et_steps           = DATA(lt_steps)
*    et_steps_approvers = DATA(lt_steps_approvers)
*    et_values          = DATA(lt_values) ).
*
*IF lt_header IS INITIAL.
*  WRITE:/ 'NO hay datos'.
*
*ELSE.
*  WRITE:/ lt_header[ 1 ]-wf_id.
*ENDIF.


*lo_query->get_all_wf_id_data(
*  EXPORTING
*    it_r_wf_id         = VALUE #( ( sign = 'I' option = 'EQ' low = '005056010BFD1EDC8097BEA0C0B720BD' ) )
*  IMPORTING
*    et_header          = DATA(lt_header)
*    et_steps           = DATA(lt_steps)
*    et_steps_approvers = DATA(lt_steps_approvers)
*    et_values          = DATA(lt_values) ).


*------------------------------*
* Cambiar valores
*------------------------------*
*lo_wf->update_values(
*  EXPORTING
*    iv_wf_id  = '005056010BFD1EDBB58801A43D0D20BD'
*    it_values = VALUE #( ( field = 'STATUS_MAT' value = '05' ) )
*    iv_process_user = 'usuario@cliente.com'
*  IMPORTING
*    et_return = DATA(lt_return) ).

*------------------------------
* APROBAR/RECHAZAR PASO
*-------------------------------------*

*lo_wf->continue_workflow_step(
*  EXPORTING
*    iv_wf_id       = '005056010BFD1EDBBAC345CB1176C0BD'
*    iv_step_result = zif_wfe_data=>cs_wf_process-step_result-approve
*    iv_process_user = 'ivan.rodrigo@enzymeadvisinggroup.com'
*  IMPORTING
*    et_return      = DATA(lt_return_continue) )

********************


*-------------------------------------*
* BUSQUEDA DEL LOG DE MODIFICACIONES
*-------------------------------------*
*lo_query->get_changelog_data(
*  EXPORTING
*    it_r_wf_id        = VALUE #( ( sign = 'I' option = 'EQ' low = '005056010BFD1EDBB58801A43D0D20BD' ) )
*  IMPORTING
*    et_changelog_data =  DATA(lt_changelog_data) ).
**
