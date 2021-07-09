class ZCL_WFE_WORKFLOW_ENGINE definition
  public
  create public .

public section.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
  methods CONSTRUCTOR
    importing
      !IV_LANGU type SYLANGU default SY-LANGU .
    "! <p class="shorttext synchronized">New workflow or draft</p>
  methods NEW_WORKFLOW
    importing
      !IV_WORKFLOW type ZWFE_E_WORKFLOW
      !IT_VALUES type ZWFE_I_VALUES_WF
      !IV_DRAFT type SAP_BOOL default ABAP_FALSE
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
      !IV_PROCESS_USER type ZWFE_E_PROCESS_USER optional
    exporting
      !EV_WF_ID type ZWFE_E_WF_ID
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
    "! <p class="shorttext synchronized">Continue workflow step</p>
  methods CONTINUE_WORKFLOW_STEP
    importing
      !IV_WF_ID type ZWFE_E_WF_ID
      !IV_STEP_RESULT type ZWFE_E_STEP_RESULT
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
      !IV_PROCESS_USER type ZWFE_E_PROCESS_USER optional
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN
      !EV_WF_COMPLETED type SAP_BOOL .
    "! <p class="shorttext synchronized">Update values</p>
  methods UPDATE_VALUES
    importing
      !IV_WF_ID type ZWFE_E_WF_ID
      !IT_VALUES type ZWFE_I_VALUES_WF
      !IV_COMMIT type SAP_BOOL default ABAP_TRUE
      !IV_PROCESS_USER type ZWFE_E_PROCESS_USER optional
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
  PROTECTED SECTION.
    DATA mo_handle_badi TYPE REF TO zwfe_badi_engine.
    DATA mv_langu TYPE sy-langu .
    DATA mo_workflow TYPE REF TO zif_wfe_workflow .

    "! <p class="shorttext synchronized">New workflow</p>
    METHODS create_workflow
      IMPORTING
        !iv_workflow     TYPE zwfe_e_workflow
        !it_values       TYPE zwfe_i_values_wf
        !iv_commit       TYPE sap_bool DEFAULT abap_true
        !iv_process_user TYPE zwfe_e_process_user OPTIONAL
      EXPORTING
        !ev_wf_id        TYPE zwfe_e_wf_id
        !et_return       TYPE zif_wfe_data=>tt_return .
    "! <p class="shorttext synchronized">Create a draft workflow</p>
    METHODS create_draft_workflow
      IMPORTING
        !iv_workflow     TYPE zwfe_e_workflow
        !it_values       TYPE zwfe_i_values_wf
        !iv_commit       TYPE sap_bool DEFAULT abap_true
        !iv_process_user TYPE zwfe_e_process_user OPTIONAL
      EXPORTING
        !ev_wf_id        TYPE zwfe_e_wf_id
        !et_return       TYPE zif_wfe_data=>tt_return .
    "! <p class="shorttext synchronized">Instance BADI</p>
    METHODS instance_badi
      IMPORTING
        !iv_workflow TYPE zwfe_e_workflow .


  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_WFE_WORKFLOW_ENGINE IMPLEMENTATION.


  METHOD constructor.
    mv_langu = iv_langu.

  ENDMETHOD.


  METHOD continue_workflow_step.

    CLEAR: et_return.
    ev_wf_completed = abap_false.

    " Se valida que el resultado del paso sea el esperado
    IF iv_step_result = zif_wfe_data=>cs_wf_process-step_result-approve OR iv_step_result = zif_wfe_data=>cs_wf_process-step_result-reject.
      TRY.
          " Instancia el objeto workflow con el valor
          DATA(lo_workflow) = zcl_wfe_workflow=>get_instance_by_wf_id( iv_langu = mv_langu
                                                                       iv_wf_id = iv_wf_id ).

          " Se lanza el proceso de aprobación
          lo_workflow->continue_next_step(
            EXPORTING
              iv_step_result = iv_step_result
              iv_process_user = iv_process_user
              iv_commit = iv_commit
            IMPORTING
              et_return      = DATA(lt_return)
              ev_next_status = DATA(lv_next_status)
              ev_wf_completed = ev_wf_completed
              et_approvers = DATA(lt_approvers) ).

          " Si no hay erores en la instancia inicial se continua el proceso
          IF NOT line_exists( lt_return[ type = zif_wfe_data=>cs_message-type-error ] ).


            " Se devuelve un mensaje que el paso ha sido aprobado o rechazado correctamente
            INSERT VALUE #( type = zif_wfe_data=>cs_message-type-sucess
                            message = zcl_wfe_utilities=>fill_return( iv_number = COND #( WHEN iv_step_result = zif_wfe_data=>cs_wf_process-step_result-approve
                                                                                          THEN '024'
                                                                                          ELSE '025' ) )-message )
                   INTO TABLE et_return.


          ELSE.
            " Si hay error se devuelve los mismos mensajes
            et_return = lt_return.
          ENDIF.

        CATCH zcx_wfe INTO DATA(lo_wfe).
          INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                          message = zcl_wfe_utilities=>fill_return( iv_id         = lo_wfe->if_t100_message~t100key-msgid
                          iv_number     = lo_wfe->if_t100_message~t100key-msgno
                          iv_message_v1 = lo_wfe->mv_msgv1
                          iv_message_v2 = lo_wfe->mv_msgv2
                          iv_message_v3 = lo_wfe->mv_msgv3
                          iv_message_v4 = lo_wfe->mv_msgv4
                          iv_langu      = mv_langu )-message ) INTO TABLE et_return.
      ENDTRY.
    ELSE.
      INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                      message = zcl_wfe_utilities=>fill_return( iv_number = '020' )-message ) INTO TABLE et_return.
    ENDIF.
  ENDMETHOD.


  METHOD create_draft_workflow.
    CLEAR: ev_wf_id, et_return.

    TRY.

        DATA(lo_workflow) = zcl_wfe_workflow=>get_instance_by_wf_name( iv_langu = mv_langu
                                                                       iv_workflow = iv_workflow ).


        lo_workflow->new_draft(
          EXPORTING
            it_values = it_values
            iv_commit = iv_commit
            iv_process_user = iv_process_user
          IMPORTING es_result = DATA(ls_result)
                    et_return = et_return ).

        " Si no hay erores en la instancia inicial se continua el proceso
        IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).

          " Se devuelve el ID del workflow creado
          ev_wf_id = ls_result-wf_id.


        ENDIF.
        " Cualquier excepcion es captura y se devuelve como mensaje de error
      CATCH zcx_wfe INTO DATA(lo_wfe).
        INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                        message = zcl_wfe_utilities=>fill_return( iv_id         = lo_wfe->if_t100_message~t100key-msgid
                        iv_number     = lo_wfe->if_t100_message~t100key-msgno
                        iv_message_v1 = lo_wfe->mv_msgv1
                        iv_message_v2 = lo_wfe->mv_msgv2
                        iv_message_v3 = lo_wfe->mv_msgv3
                        iv_message_v4 = lo_wfe->mv_msgv4
                        iv_langu      = mv_langu )-message ) INTO TABLE et_return.
    ENDTRY.
  ENDMETHOD.


  METHOD create_workflow.
    TRY.
        DATA(lo_workflow) = zcl_wfe_workflow=>get_instance_by_wf_name( iv_langu = mv_langu
                                                                       iv_workflow = iv_workflow ).

        " Se lanza el proceso de creación del workflow
        lo_workflow->new(
          EXPORTING
            it_values = it_values
            iv_process_user = iv_process_user
            iv_commit = iv_commit
          IMPORTING es_result = DATA(ls_result)
                    et_return = DATA(lt_return)
                    et_approvers = DATA(lt_approvers) ).

        " Si no hay erores en la instancia inicial se continua el proceso
        IF NOT line_exists( lt_return[ type = zif_wfe_data=>cs_message-type-error ] ).
          " Se devuelve el ID del workflow creado
          ev_wf_id = ls_result-wf_id.

          " Se devuelve un mensaje que el workflow ha sido lanzado correctamente
          INSERT VALUE #( type = zif_wfe_data=>cs_message-type-sucess
                        message = zcl_wfe_utilities=>fill_return( iv_number = '019' )-message ) INTO TABLE et_return.


        ELSE.
          " Si hay error se devuelve los mismos mensajes
          et_return = lt_return.
        ENDIF.
        " Cualquier excepcion es captura y se devuelve como mensaje de error
      CATCH zcx_wfe INTO DATA(lo_wfe).
        INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                        message = zcl_wfe_utilities=>fill_return( iv_id         = lo_wfe->if_t100_message~t100key-msgid
                        iv_number     = lo_wfe->if_t100_message~t100key-msgno
                        iv_message_v1 = lo_wfe->mv_msgv1
                        iv_message_v2 = lo_wfe->mv_msgv2
                        iv_message_v3 = lo_wfe->mv_msgv3
                        iv_message_v4 = lo_wfe->mv_msgv4
                        iv_langu      = mv_langu )-message ) INTO TABLE et_return.
    ENDTRY.
  ENDMETHOD.


  METHOD instance_badi.
    TRY.

        GET BADI mo_handle_badi FILTERS workflow = iv_workflow.

      CATCH cx_root.

    ENDTRY.
  ENDMETHOD.


  METHOD new_workflow.

    " Obtengo el gestor de la BADI en base al workflow indicado
    instance_badi( iv_workflow ).


    " La creación varia si se quiere crear un borrador o un workflow normal y corriente. Aunque
    " los parámetros inicialmente son lo
    IF iv_draft = abap_false.
      create_workflow(
        EXPORTING
          iv_workflow = iv_workflow
          it_values   = it_values
          iv_process_user = iv_process_user
          iv_commit = iv_commit
        IMPORTING
          ev_wf_id    = ev_wf_id
          et_return   = et_return ).
    ELSE.
      create_draft_workflow(
        EXPORTING
          iv_workflow = iv_workflow
          it_values   = it_values
          iv_process_user = iv_process_user
          iv_commit = iv_commit
        IMPORTING
          ev_wf_id    = ev_wf_id
          et_return   = et_return ).
    ENDIF.

  ENDMETHOD.


  METHOD update_values.
    CLEAR: et_return.
    TRY.
        " Instancia el objeto workflow con el valor
        DATA(lo_workflow) = zcl_wfe_workflow=>get_instance_by_wf_id( iv_langu = mv_langu
                                                                     iv_wf_id = iv_wf_id ).

        " Se lanza el proceso de actualización de valores
        lo_workflow->update_values(
          EXPORTING
            it_values = it_values
            iv_process_user = iv_process_user
            iv_commit = iv_commit
          IMPORTING
            et_return      = DATA(lt_return) ).

        " Si no hay erores en la instancia inicial se continua el proceso
        IF NOT line_exists( lt_return[ type = zif_wfe_data=>cs_message-type-error ] ).

          " Se devuelve un mensaje que el paso ha sido aprobado o rechazado correctamente
          INSERT VALUE #( type = zif_wfe_data=>cs_message-type-sucess
                          message = zcl_wfe_utilities=>fill_return( iv_number = '028' )-message )
                 INTO TABLE et_return.

        ELSE.
          " Si hay error se devuelve los mismos mensajes
          et_return = lt_return.
        ENDIF.

      CATCH zcx_wfe INTO DATA(lo_wfe).
        INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                        message = zcl_wfe_utilities=>fill_return( iv_id         = lo_wfe->if_t100_message~t100key-msgid
                        iv_number     = lo_wfe->if_t100_message~t100key-msgno
                        iv_message_v1 = lo_wfe->mv_msgv1
                        iv_message_v2 = lo_wfe->mv_msgv2
                        iv_message_v3 = lo_wfe->mv_msgv3
                        iv_message_v4 = lo_wfe->mv_msgv4
                        iv_langu      = mv_langu )-message ) INTO TABLE et_return.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
