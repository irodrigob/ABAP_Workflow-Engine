CLASS zcl_wfe_workflow DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_wfe_workflow .

    .


    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !iv_workflow TYPE zwfe_e_workflow
        !iv_langu    TYPE sylangu DEFAULT sy-langu
      RAISING
        zcx_wfe .
    "! <p class="shorttext synchronized">Get instance by workflow name</p>
    CLASS-METHODS get_instance_by_wf_name
      IMPORTING
        !iv_langu          TYPE sylangu DEFAULT sy-langu
        !iv_workflow       TYPE zwfe_e_workflow
      RETURNING
        VALUE(ro_workflow) TYPE REF TO zif_wfe_workflow
      RAISING
        zcx_wfe .
    "! <p class="shorttext synchronized">Get instance by workflow ID</p>
    CLASS-METHODS get_instance_by_wf_id
      IMPORTING
        !iv_langu          TYPE sylangu DEFAULT sy-langu
        !iv_wf_id          TYPE zwfe_e_wf_id
      RETURNING
        VALUE(ro_workflow) TYPE REF TO zif_wfe_workflow
      RAISING
        zcx_wfe .
protected section.

  types:
    BEGIN OF ts_result_new,
        wf_id  TYPE zwfe_e_wf_id,
        status TYPE zwfe_e_status,
      END OF ts_result_new .
  types:
    BEGIN OF ts_approvers,
        approver TYPE zwfe_e_approver,
      END OF ts_approvers .
  types:
    tt_approvers TYPE STANDARD TABLE OF ts_approvers .
  types:
    tv_actvt TYPE c LENGTH 2 .
  types:
    BEGIN OF ts_wf_info,
        workflow          TYPE zwfe_t001-workflow,
        description       TYPE zwfe_t001t-description,
        description_short TYPE zwfe_t001t-description_short,
      END OF ts_wf_info .
  types:
    BEGIN OF ts_owner,
        owner      TYPE zwfe_t005-owner,
        owner_desc TYPE zwfe_t005t-description,
      END OF ts_owner .
  types:
    tt_owner TYPE STANDARD TABLE OF ts_owner WITH EMPTY KEY .
  types:
    BEGIN OF ts_owner_approvers,
        owner    TYPE zwfe_t005-owner,
        approver TYPE zwfe_t006-approver,
      END OF ts_owner_approvers .
  types:
    tt_owner_approvers TYPE STANDARD TABLE OF ts_owner_approvers WITH EMPTY KEY .
  types:
    BEGIN OF ts_status_flow,
        status      TYPE zwfe_e_status,
        status_next TYPE zwfe_e_status_next,
      END OF ts_status_flow .
  types:
    tt_status_flow TYPE STANDARD TABLE OF ts_status_flow WITH EMPTY KEY .
  types:
    BEGIN OF ts_wf_status,
        status      TYPE zwfe_t003-status,
        status_desc TYPE zwfe_t002t-description,
        owner       TYPE zwfe_t003-owner,
        init        TYPE zwfe_t003-init,
        draft       TYPE zwfe_t003-draft,
        reject      TYPE zwfe_t003-reject,
        complete    TYPE zwfe_t003-complete,
      END OF ts_wf_status .
  types:
    tt_wf_status TYPE STANDARD TABLE OF ts_wf_status WITH EMPTY KEY .

  constants:
    BEGIN OF cs_actvt,
        new         TYPE tv_actvt VALUE 'N',
        draft       TYPE tv_actvt VALUE 'D',
        edit        TYPE tv_actvt VALUE 'E',
        approve     TYPE tv_actvt VALUE 'A',
        edit_values TYPE tv_actvt VALUE 'V',
      END OF cs_actvt .
  data MO_HANDLE_BADI type ref to ZWFE_BADI_WF_MODEL .
  data MV_LANGU type SYLANGU .
  data MV_WORKFLOW type ZWFE_E_WORKFLOW .
  data MS_WF_INFO type TS_WF_INFO .
  data MT_WF_STATUS type TT_WF_STATUS .
  data MT_STATUS_FLOW type TT_STATUS_FLOW .
  data MV_STATUS_INIT type ZWFE_T002-STATUS .
  data MV_STATUS_DRAFT type ZWFE_T002-STATUS .
  data MT_OWNER type TT_OWNER .
  data MT_OWNER_APPROVERS type TT_OWNER_APPROVERS .
  data MO_QUERY type ref to ZCL_WFE_MODEL_DATA_QUERY .
  data MO_CRUD type ref to ZCL_WFE_MODEL_DATA_CRUD .
  data MS_HEADER type ZWFE_S_HEADER_ALL_FIELDS .
  data MT_STEPS type ZWFE_I_STEPS_ALL_FIELDS .
  data MT_STEPS_APPROVERS type ZWFE_I_STEPS_APPROV_ALL_FIELDS .
  data MT_VALUES type ZWFE_I_VALUES_ALL_FIELDS .
  data MV_ACTVT type TV_ACTVT .
  data MV_PROCESS_USER type ZWFE_E_PROCESS_USER .
  data:
    mt_status_reject TYPE STANDARD TABLE OF zwfe_t002-status .
  data:
    mt_status_completed TYPE STANDARD TABLE OF zwfe_t002-status .

  methods LOAD_WORKFLOW_CONFIGURATION
    raising
      ZCX_WFE .
  methods CHECK_CONSISTENCY_STATUS_CONF
    raising
      ZCX_WFE .
  methods CHECK_CONSISTENCY_OWNER_CONF
    raising
      ZCX_WFE .
  methods READ_WF_ID_DATA
    importing
      !IV_WF_ID type ZWFE_E_WF_ID
    raising
      ZCX_WFE .
  methods FILL_HEADER_ON_CREATION
    raising
      ZCX_WFE .
  methods FILL_VALUES
    importing
      !IT_VALUES type ZWFE_I_VALUES_WF .
  methods NEW_STEP
    importing
      !IV_STATUS type ZWFE_E_STATUS
      !IV_STEP_STATUS type ZWFE_E_STEP_STATUS default ZIF_WFE_DATA=>CS_WF_PROCESS-STEP_STATUS-ACTIVE .
  methods FILL_STEP_APPROVERS
    importing
      !IV_STATUS type ZWFE_S_HEADER_ALL_FIELDS-STATUS
      !IT_APPROVERS type ZIF_WFE_WORKFLOW=>TT_APPROVERS
    raising
      ZCX_WFE .
  methods CHECK_CONSISTENCY
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
  methods VALIDATE_STEPS_APPROVERS
    changing
      !CT_RETURN type ZIF_WFE_DATA=>TT_RETURN .
  methods SAVE_DATA
    importing
      !IV_COMMIT type SAP_BOOL
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
  methods DETERMINE_NEXT_STATUS
    importing
      !IV_STATUS type ZWFE_E_STATUS
      !IV_STEP_RESULT type ZWFE_E_STEP_RESULT
    exporting
      !EV_NEXT_STATUS type ZWFE_E_STATUS
      !EV_WORKFLOW_COMPLETED type SAP_BOOL
    raising
      ZCX_WFE .
  methods CHECK_DATA_NEXT_STEP
    raising
      ZCX_WFE .
  methods APPROVE_STEP
    importing
      !IV_NEXT_STATUS type ZWFE_E_STATUS_NEXT
      !IV_WORKFLOW_COMPLETED type SAP_BOOL default ABAP_FALSE
    raising
      ZCX_WFE .
  methods REJECT_STEP
    importing
      !IV_NEXT_STATUS type ZWFE_E_STATUS_NEXT .
  methods INSTANCE_BADI .
  methods CALL_BADI_CHANGE_STEPS_APPROV
    importing
      !IV_STATUS type ZWFE_E_STATUS
      !IT_VALUES type ZWFE_I_VALUES_WF
    changing
      !CT_APPROVERS type ZIF_WFE_WORKFLOW=>TT_APPROVERS .
  methods CALL_BADI_POST_SAVE_WORKFLOW .
  methods CALL_BADI_DET_NEXT_STATUS
    importing
      !IV_ACTUAL_STATUS type ZWFE_E_STATUS
      !IV_STEP_RESULT type ZWFE_E_STEP_RESULT
    changing
      !CV_NEXT_STATUS type ZWFE_E_STATUS_NEXT
      !CV_WORKFLOW_COMPLETED type SAP_BOOL
    raising
      ZCX_WFE .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WFE_WORKFLOW IMPLEMENTATION.


  METHOD approve_step.

    " Si no hay siguiente paso lanzo un error porque
    IF iv_next_status IS NOT INITIAL.

      ms_header-status = iv_next_status.

      " Se cierra el paso actual.
      READ TABLE mt_steps ASSIGNING FIELD-SYMBOL(<ls_steps>) WITH KEY step_status = zif_wfe_data=>cs_wf_process-step_status-active.
      IF sy-subrc = 0.
        <ls_steps>-step_status = zif_wfe_data=>cs_wf_process-step_status-completed.
        " Quien aprueba el paso dependerá si esta informado por parámetro, en caso contrario, por el usuario de SAP. Esto se hace
        " en procesos donde el aprobador no es un usuario SAP, sino que es otro tipo de entidad.
        <ls_steps>-approved_by = mv_process_user.
      ENDIF.

      " Si al determinar el siguiente paso se determina que es el final
      ms_header-wf_status = COND #( WHEN iv_workflow_completed = abap_true THEN zif_wfe_data=>cs_wf_process-wf_status-completed ELSE ms_header-wf_status ).


      " Determino  los aprobadores del siguiente paso
      zif_wfe_workflow~determine_status_approvers( EXPORTING iv_status = ms_header-status
                                      it_values = CORRESPONDING #( mt_values )
                            IMPORTING et_approvers = DATA(lt_approvers) ).

      " Si no hay aprobadores no se añade nada a la tabla de aprobadores
      IF lt_approvers IS NOT INITIAL.
        " Relleno de los aprobadores del paso. Los aprobadores del paso se determinan en el motor. Ya que intervienen
        " exit que permiten modificar o determinar aprobadores según el estado.
        fill_step_approvers( EXPORTING iv_status = ms_header-status
                                       it_approvers = lt_approvers  ).
      ENDIF.

      " Se añade el nuevo paso con un status dependiendo del estado global
      new_step( iv_status = ms_header-status
                     iv_step_status = COND #( WHEN ms_header-wf_status = zif_wfe_data=>cs_wf_process-wf_status-completed
                                              THEN zif_wfe_data=>cs_wf_process-step_status-completed
                                              ELSE zif_wfe_data=>cs_wf_process-step_status-active ) ).

    ELSE.
      RAISE EXCEPTION TYPE zcx_wfe
        EXPORTING
          textid = zcx_wfe=>not_next_step.
    ENDIF.
  ENDMETHOD.


  METHOD call_badi_change_steps_approv.
    IF mo_handle_badi IS BOUND.
      CALL BADI mo_handle_badi->change_step_approvers
        EXPORTING
          iv_status    = iv_status
          it_values    = it_values
        CHANGING
          ct_approvers = ct_approvers.

    ENDIF.
  ENDMETHOD.


  METHOD call_badi_det_next_status.

    IF mo_handle_badi IS BOUND.

      DATA(lt_status) = VALUE zif_wfe_badi_workflow_model=>tt_available_status( ).
      " Se informan los status a los que se puede cambiar ir según la configuración del paso actual
      LOOP AT mt_status_flow ASSIGNING FIELD-SYMBOL(<ls_status_flow>) WHERE status = iv_actual_status.
        READ TABLE mt_wf_status ASSIGNING FIELD-SYMBOL(<ls_wf_status>) WITH KEY status = <ls_status_flow>-status_next.
        IF sy-subrc = 0.
          " En el caso de aprobar el siguiente estado no puede ser de rechazo.
          IF <ls_wf_status>-reject = abap_false AND  iv_step_result = zif_wfe_data=>cs_wf_process-step_result-approve.
            INSERT VALUE #( status = <ls_status_flow>-status_next
                            complete = <ls_wf_status>-complete
                            init = <ls_wf_status>-init ) INTO TABLE lt_status.
            " Y en caso de rechazar el siguiente paso no puede ser de aprobación
          ELSEIF <ls_wf_status>-reject = abap_true AND iv_step_result = zif_wfe_data=>cs_wf_process-step_result-reject.
            INSERT VALUE #( status = <ls_status_flow>-status
                            reject = <ls_wf_status>-reject ) INTO TABLE lt_status.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " Los valores los adapto al formato de BADI.
      DATA(lt_values) = CORRESPONDING zwfe_i_values_wf( mt_values ).

      CALL BADI mo_handle_badi->determine_next_status
        EXPORTING
          iv_actual_status         = iv_actual_status
          it_values                = lt_values
          it_next_status_available = lt_status
          iv_step_result           = iv_step_result
        CHANGING
          cv_workflow_completed    = cv_workflow_completed
          cv_next_status           = cv_next_status.

      " Si el status determinado no es uno de lo que puede escoger se lanzará una excepción.
      " El blanco no lo valido porque el método de determinar puede devolver blanco para saber si
      " no hay siguientes pasos y por lo tanto se tiene que cerrar el workflow
      IF cv_next_status IS NOT INITIAL.

        READ TABLE lt_status TRANSPORTING NO FIELDS WITH KEY status = cv_next_status.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_wfe
            EXPORTING
              textid   = zcx_wfe=>status_badi_not_valid
              mv_msgv1 = CONV #( cv_next_status ).
        ENDIF.

        " Hago una validación, y correción si es necesario, donde si el estados determinado es el que por custo esta marcado
        " como completado se marca el parámetro de workflow completed
        READ TABLE mt_status_completed TRANSPORTING NO FIELDS WITH KEY table_line = cv_next_status.
        IF sy-subrc = 0.
          cv_workflow_completed = abap_true.
        ELSE.
          cv_workflow_completed = abap_false.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD call_badi_post_save_workflow.
    IF mo_handle_badi IS BOUND.
      CALL BADI mo_handle_badi->post_save_workflow
        EXPORTING
          io_wf_model = me.

    ENDIF.
  ENDMETHOD.


  METHOD check_consistency.
    CLEAR: et_return.

    " Validación de aprobadores del paso.
    validate_steps_approvers( CHANGING ct_return = et_return ).

  ENDMETHOD.


  METHOD check_consistency_owner_conf.


  ENDMETHOD.


  METHOD check_consistency_status_conf.
    DATA lv_num_init TYPE i.
    DATA lv_num_reject TYPE i.
    DATA lv_num_draft TYPE i.
    DATA lv_num_complete TYPE i.

    " Cuento el numero de status que tienen flags marcados.
    LOOP AT mt_wf_status ASSIGNING FIELD-SYMBOL(<ls_status>).
      lv_num_init = COND #( WHEN <ls_status>-init = abap_true THEN lv_num_init + 1 ELSE lv_num_init ).
      lv_num_reject = COND #( WHEN <ls_status>-reject = abap_true THEN lv_num_reject + 1 ELSE lv_num_reject ).
      lv_num_draft = COND #( WHEN <ls_status>-draft = abap_true THEN lv_num_draft + 1 ELSE lv_num_draft ).
      lv_num_complete = COND #( WHEN <ls_status>-complete = abap_true THEN lv_num_complete + 1 ELSE lv_num_complete ).
    ENDLOOP.
    " Estado inicial solo puede haber 1. De borrador si hay más de uno también es error, puede haber 0 porque el workflow no tiene
    " la posibilidad de borrador. De rechazado dejo la posibilidad que haya varios tipos de rechazo.
    " De completo puede haber varios estados como el rechazo.
    IF lv_num_init NE 1 OR lv_num_reject = 0 OR lv_num_draft > 1 OR lv_num_complete = 0.
      RAISE EXCEPTION TYPE zcx_wfe
        EXPORTING
          textid = zcx_wfe=>workflow_not_status_conf.
    ENDIF.

    " Para simplificar las búsqueda posterior me guardo dichos estados en variables globales.
    mv_status_draft = COND #( WHEN lv_num_draft = 1 THEN mt_wf_status[ draft = abap_true ]-status ELSE space ).
    mv_status_init = mt_wf_status[ init = abap_true ]-status.
    " Se guarda en una tabla todos los posibles estados de completado
    mt_status_completed = VALUE #( FOR <wa> IN mt_wf_status WHERE ( complete = abap_true ) ( <wa>-status ) ).
    " Guardo todos los posibles rechazos que existan. Solo debería haber uno, por lo general. En el caso de rechazar
    " se tomará el primer registro, aún así, la exit/badi que hay determinará el rechazo final
    mt_status_reject = VALUE #( FOR <wa> IN mt_wf_status WHERE ( reject = abap_true ) ( <wa>-status ) ).


  ENDMETHOD.


  METHOD check_data_next_step.

    IF ms_header-wf_status NE zif_wfe_data=>cs_wf_process-wf_status-active.
      RAISE EXCEPTION TYPE zcx_wfe
        EXPORTING
          textid = zcx_wfe=>workflow_not_active.

    ELSE.
      " Por tal como funciona la aplicación, debería de haber un paso activo. En caso de no haberlo se produce error.
      READ TABLE mt_steps TRANSPORTING NO FIELDS WITH KEY step_status = zif_wfe_data=>cs_wf_process-step_status-active.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_wfe
          EXPORTING
            textid = zcx_wfe=>not_active_steps.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    mv_langu = iv_langu.
    mv_workflow = iv_workflow.

    " Obtengo el gestor de la BADI en base al workflow indicado
    instance_badi( ).

    " Se instancia la clase que gestiona las búsquedas del modelo
    mo_query = NEW zcl_wfe_model_data_query( iv_langu = mv_langu ).

    " Se instancia la clase que gestionas las actualizaciones del modelo
    mo_crud = NEW zcl_wfe_model_data_crud( ).

    " Primer paso leer la configuración del workflow para obtenerlo todo.
    load_workflow_configuration( ).
  ENDMETHOD.


  METHOD determine_next_status.
    DATA lt_r_status TYPE RANGE OF zwfe_e_status.

    CLEAR: ev_next_status.

    " Si el resultado del paso es aprobar voy al paso siguiente configurado que no sea de tipo recazdo.
    IF iv_step_result = zif_wfe_data=>cs_wf_process-step_result-approve.
      lt_r_status = VALUE #( FOR <wa> IN mt_status_reject ( sign = 'I' option = 'NE' low = <wa> ) ).
    ELSE.
      " Si se rechaza se busca el siguiente paso que sea de rechazo.
      lt_r_status = VALUE #( FOR <wa> IN mt_status_reject ( sign = 'I' option = 'EQ' low = <wa> ) ).
    ENDIF.

    " El siguiente status es buscar el status pasado en la tabla de flujo y buscar aquel que su paso siguiente no sea el rechazado.
    " Si no encuentra registro no se generá error porque será quien llame a este método para que lo necesita.
    LOOP AT mt_status_flow ASSIGNING FIELD-SYMBOL(<ls_status_flow>)
                           WHERE status = iv_status
                                 AND status_next IN lt_r_status.
      ev_next_status = <ls_status_flow>-status_next.
      EXIT.
    ENDLOOP.

    " Si el status siguiente es el mismo que el completado se marca el parámetro de completado
    READ TABLE mt_status_completed TRANSPORTING NO FIELDS WITH KEY table_line = ev_next_status.
    IF sy-subrc = 0.
      ev_workflow_completed = abap_true.
    ELSE.
      ev_workflow_completed = abap_false.
    ENDIF.

    " Se llama a la BADI para poder cambiar el paso siguiente
    call_badi_det_next_status(
      EXPORTING
        iv_actual_status           = ms_header-status
        iv_step_result             = iv_step_result
      CHANGING
        cv_next_status             = ev_next_status
        cv_workflow_completed      = ev_workflow_completed ).

  ENDMETHOD.


  METHOD fill_header_on_creation.
    " ID del worlflow
    ms_header-wf_id = /bobf/cl_frw_factory=>get_new_key( ).
    ms_header-workflow = mv_workflow.


    " El workflow se puede crear con modo borrador o normal, eso se indica con la actividad global de la clase.
    " Cada uno se inicia con un status distinto. En modo borrador el workflow tiene que tener el status de borrador configurado.
    IF mv_actvt = cs_actvt-new.
      " El status, que pongo me gusta el nombre, será el inicial.
      ms_header-status = mv_status_init.
      " El estado del workflow será activo.
      ms_header-wf_status = zif_wfe_data=>cs_wf_process-wf_status-active.
    ELSE.
      IF mv_status_draft IS NOT INITIAL.
        " Status será borrador
        ms_header-status = mv_status_draft.
        " El estado será borrador
        ms_header-wf_status = zif_wfe_data=>cs_wf_process-wf_status-draft.
      ELSE.
        RAISE EXCEPTION TYPE zcx_wfe
          EXPORTING
            textid = zcx_wfe=>draft_status_not_configured.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD fill_step_approvers.

    " Borro los aprobadores que pueda tener el paso para añadir los que me vienen como parámetro de salida
    DELETE mt_steps_approvers WHERE status = iv_status.

    mt_steps_approvers = VALUE #( BASE mt_steps_approvers FOR <wa> IN it_approvers WHERE ( approver IS NOT INITIAL ) ( wf_id = ms_header-wf_id
                                                             status = iv_status
                                                             approver = <wa>-approver ) ).
  ENDMETHOD.


  METHOD fill_values.

    " El relleno de valores actualiza los valores que vengan por parámetro, en caso contrario, se añaden.

    LOOP AT it_values ASSIGNING FIELD-SYMBOL(<ls_values_param>).
      READ TABLE mt_values ASSIGNING FIELD-SYMBOL(<ls_values>) WITH KEY counter = <ls_values_param>-counter
                                                                        field = <ls_values_param>-field.
      IF sy-subrc = 0.
        " Nota: Hago corresponding por si en un futuro añado más tipos de campos valor
        <ls_values> = CORRESPONDING #( BASE ( <ls_values> ) <ls_values_param> ).
      ELSE.
        INSERT CORRESPONDING #( <ls_values_param> ) INTO TABLE mt_values ASSIGNING <ls_values>.
        <ls_values>-wf_id = ms_header-wf_id.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance_by_wf_id.

    CLEAR ro_workflow.

    " Buscamos si existe el ID del workflow
    SELECT SINGLE wf_id, workflow INTO @DATA(ls_header)
           FROM zwfe_t007
           WHERE wf_id = @iv_wf_id.
    IF sy-subrc = 0.


      DATA(lo_workflow) = NEW zcl_wfe_workflow( iv_langu = iv_langu
                                                iv_workflow = ls_header-workflow ).

      " Se llama al método que obtiene los datos
      lo_workflow->read_wf_id_data( EXPORTING iv_wf_id = iv_wf_id ).

      " Hago un casting para devolverlo como el parámetro de salida.
      ro_workflow = CAST #( lo_workflow ).

    ELSE.
      RAISE EXCEPTION TYPE zcx_wfe
        EXPORTING
          textid = zcx_wfe=>wf_id_not_exist.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance_by_wf_name.

    CLEAR ro_workflow.

    CREATE OBJECT ro_workflow TYPE zcl_wfe_workflow
      EXPORTING
        iv_langu    = iv_langu
        iv_workflow = iv_workflow.

  ENDMETHOD.


  METHOD instance_badi.
    TRY.

        GET BADI mo_handle_badi FILTERS workflow = mv_workflow.

      CATCH cx_root.

    ENDTRY.
  ENDMETHOD.


  METHOD load_workflow_configuration.
    DATA lt_r_owner TYPE RANGE OF zwfe_t003-owner.

    CLEAR: ms_wf_info, mt_wf_status.

    " Información general del workflow
    SELECT SINGLE a~workflow
           b~description b~description_short
           INTO ms_wf_info
           FROM zwfe_t001 AS a LEFT OUTER JOIN zwfe_t001t AS b ON
                b~workflow = a~workflow
                AND b~spras = mv_langu
           WHERE a~workflow = mv_workflow.
    IF sy-subrc = 0.

      " Status del workflow
      SELECT a~status b~description AS status_desc a~owner a~init a~draft a~reject a~complete
            INTO TABLE mt_wf_status
            FROM zwfe_t003 AS a LEFT OUTER JOIN zwfe_t002t AS b ON
                 b~status = a~status
                 AND b~spras = mv_langu
            WHERE a~workflow = mv_workflow.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_wfe
          EXPORTING
            textid = zcx_wfe=>workflow_not_status_conf.
      ELSE.
        " Se verifica de la consistencia en la configuración de estados
        check_consistency_status_conf( ).
      ENDIF.

      " Owner
      " No tengo claro que pueda haber owner_profile en blanco. Pero de momento lo dejamos así.
      lt_r_owner = VALUE #( FOR <wa> IN mt_wf_status WHERE ( owner IS NOT INITIAL ) ( sign = 'I' option = 'EQ' low = <wa>-owner ) ).
      IF lt_r_owner IS NOT INITIAL.
        SELECT a~owner b~description AS owner_desc
               INTO TABLE mt_owner
               FROM zwfe_t005 AS a LEFT OUTER JOIN zwfe_t005t AS b ON
                    b~owner = a~owner
                    AND b~spras = mv_langu
               WHERE a~owner IN lt_r_owner.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_wfe
            EXPORTING
              textid = zcx_wfe=>workflow_not_owner.
        ENDIF.

        " Aprovadores fijos del owner
        SELECT owner approver INTO TABLE mt_owner_approvers
               FROM zwfe_t006
               WHERE owner IN lt_r_owner.

        " Se verifica la consistencia del owner profile
        check_consistency_owner_conf( ).
      ENDIF.

      " Pasos del flujo
      SELECT status status_next INTO TABLE mt_status_flow
             FROM zwfe_t004
             WHERE workflow = mv_workflow.

    ELSE.
      RAISE EXCEPTION TYPE zcx_wfe
        EXPORTING
          textid   = zcx_wfe=>workflow_name_not_exist
          mv_msgv1 = CONV #( mv_workflow ).
    ENDIF.

  ENDMETHOD.


  METHOD new_step.
    " El nuevo paso se añade con el status pasado por parámetro y con el estado de paso como activo.
    " Porque un paso puede estar activo o completado. No hay valor intermedio, al menos de momento.
    INSERT VALUE #( wf_id = ms_header-wf_id
                    status = iv_status
                    step_status = iv_step_status ) INTO TABLE mt_steps.

  ENDMETHOD.


  METHOD read_wf_id_data.

    " Los datos del worfklow se recuperan de la clase que gestiona las búsquedas
    mo_query->get_all_wf_id_data( EXPORTING it_r_wf_id = VALUE #( ( sign = 'I' option = 'EQ' low = iv_wf_id ) )
                                  IMPORTING et_header = DATA(lt_header)
                                            et_steps = mt_steps
                                            et_steps_approvers = mt_steps_approvers
                                            et_values = mt_values ).

    READ TABLE lt_header INTO ms_header INDEX 1.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_wfe
        EXPORTING
          textid = zcx_wfe=>wf_id_not_exist.
    ENDIF.

  ENDMETHOD.


  METHOD reject_step.


    " Se pone el status en la cabecera como rechazado y el estado de rechazo
    ms_header-status = iv_next_status.
    ms_header-wf_status = zif_wfe_data=>cs_wf_process-wf_status-rejected.

    " Se cierra el paso actual.
    READ TABLE mt_steps ASSIGNING FIELD-SYMBOL(<ls_steps>) WITH KEY step_status = zif_wfe_data=>cs_wf_process-step_status-active.
    IF sy-subrc = 0.
      <ls_steps>-approved_by = mv_process_user.
      <ls_steps>-step_status = zif_wfe_data=>cs_wf_process-step_status-completed.
    ENDIF.

    TRY.
        " Determino  los aprobadores del paso rechazado por si luego se quiere enviar cualquier notificacion o lo que sea.
        zif_wfe_workflow~determine_status_approvers( EXPORTING iv_status = ms_header-status
                                        it_values = CORRESPONDING #( mt_values )
                              IMPORTING et_approvers = DATA(lt_approvers) ).

        " Si no hay aprobadores no se añade nada a la tabla de aprobadores
        IF lt_approvers IS NOT INITIAL.
          " Relleno de los aprobadores del paso. Los aprobadores del paso se determinan en el motor. Ya que intervienen
          " exit que permiten modificar o determinar aprobadores según el estado.
          fill_step_approvers( EXPORTING iv_status = ms_header-status
                                         it_approvers = lt_approvers  ).
        ENDIF.
      CATCH zcx_wfe.

    ENDTRY.
    " Se añade el nuevo paso de rechazo
    new_step( iv_status = iv_next_status
                   iv_step_status = zif_wfe_data=>cs_wf_process-step_status-completed ).


  ENDMETHOD.


  METHOD save_data.

    " Las opciones del CRUD son iguales para cualquier tipo de edición.
    " El process_user es el usuario real que realiza el proceso. Este usuario de proceso puede ser desde
    " un usuario SAP, correo electrónico, o cualquier tipo de identificación de usuario que se use en el motor.
    DATA(ls_options) = VALUE zcl_wfe_model_data_crud=>ts_crud_options( langu = mv_langu
                                                         commit = iv_commit
                                                         process_user = mv_process_user ).
    " La manera de grabar dependerá del tipo de actividad de la clase. Ya que afectará a como se llama al proceso de actualización
    CASE mv_actvt.
      WHEN cs_actvt-new OR cs_actvt-edit OR cs_actvt-draft.

        mo_crud->dispatch_data(
          EXPORTING
              is_options                = ls_options
              iv_updkz                  = SWITCH #( mv_actvt WHEN cs_actvt-new OR cs_actvt-draft THEN zif_wfe_data=>cs_model_data-update_ind-new_wf
                                                             WHEN cs_actvt-edit THEN zif_wfe_data=>cs_model_data-update_ind-edit_wf )
              is_header                 = CORRESPONDING #( ms_header )
              it_values                 = CORRESPONDING #( mt_values )
              it_steps                  = CORRESPONDING #( mt_steps )
              it_steps_approvers        = CORRESPONDING #( mt_steps_approvers )
          IMPORTING
              et_return                 = et_return
              es_header_result          = DATA(ls_header_result)
              et_values_result          = DATA(lt_values_result)
              et_steps_result           = DATA(lt_steps_result)
              et_steps_approvers_result = DATA(lt_steps_approvers_result) ).
        " Si no existen errores en la grabación entonces vuelco los datos devueltos a las variables globales.
        " Los datos devueltos son los mismos salvo algun contador que se informa correctamente para evitar claves duplicadas.
        IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
          ms_header = CORRESPONDING #( ls_header_result ).
          mt_values = CORRESPONDING #( lt_values_result ).
          mt_steps = CORRESPONDING #( lt_steps_result ).
          mt_steps_approvers = CORRESPONDING #( lt_steps_approvers_result ).
        ENDIF.

      WHEN cs_actvt-approve.
        mo_crud->dispatch_data(
          EXPORTING
              is_options                = ls_options
              iv_updkz                  = zif_wfe_data=>cs_model_data-update_ind-continue_step
              is_header                 = CORRESPONDING #( ms_header )
              it_steps                  = CORRESPONDING #( mt_steps )
              it_steps_approvers        = CORRESPONDING #( mt_steps_approvers )
          IMPORTING
              et_return                 = et_return
              es_header_result          = ls_header_result
              et_steps_result           = lt_steps_result
              et_steps_approvers_result = lt_steps_approvers_result ).
        " Si no existen errores en la grabación entonces vuelco los datos devueltos a las variables globales.
        " Los datos devueltos son los mismos salvo algun contador que se informa correctamente para evitar claves duplicadas.
        IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
          ms_header = CORRESPONDING #( ls_header_result ).
          mt_values = CORRESPONDING #( lt_values_result ).
          mt_steps = CORRESPONDING #( lt_steps_result ).
          mt_steps_approvers = CORRESPONDING #( lt_steps_approvers_result ).
        ENDIF.
      WHEN cs_actvt-edit_values.
        mo_crud->dispatch_data(
          EXPORTING
              is_options                = ls_options
              iv_updkz                  = zif_wfe_data=>cs_model_data-update_ind-edit_wf
              is_header                 = CORRESPONDING #( ms_header )
              it_values                 = CORRESPONDING #( mt_values )
          IMPORTING
              et_return                 = et_return
              es_header_result          = ls_header_result
              et_values_result          = lt_values_result ).
    ENDCASE.

    " Si no existen errores en la grabación entonces vuelco los datos devueltos a las variables globales.
    " Los datos devueltos son los mismos salvo algun contador que se informa correctamente para evitar claves duplicadas.
    IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
      ms_header = CORRESPONDING #( ls_header_result ).
      mt_values = CORRESPONDING #( lt_values_result ).

      " Se llama a la BADI del proceso antes de grabar.
      call_badi_post_save_workflow( ).
    ENDIF.
  ENDMETHOD.


  METHOD validate_steps_approvers.

    " La validación es sencilla. Si el status del paso activo tiene asociado un owner, entonces tiene que haber aprobadores.
    READ TABLE mt_steps ASSIGNING FIELD-SYMBOL(<ls_steps>) WITH KEY step_status = zif_wfe_data=>cs_wf_process-step_status-active.
    IF sy-subrc = 0.
      READ TABLE mt_wf_status ASSIGNING FIELD-SYMBOL(<ls_wf_status>) WITH KEY status = <ls_steps>-status.
      IF sy-subrc = 0.
        IF <ls_wf_status>-owner IS NOT INITIAL.
          READ TABLE mt_steps_approvers TRANSPORTING NO FIELDS WITH KEY status = <ls_steps>-status.
          IF sy-subrc NE 0.
            INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                            message = zcl_wfe_utilities=>fill_return( iv_number = '018'
                                                                      iv_message_v1 = <ls_steps>-status )-message ) INTO TABLE ct_return.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wfe_workflow~continue_next_step.

    CLEAR: et_return, ev_next_status, et_approvers.
    ev_wf_completed = abap_false.

    " Usuario del proceso que no tiene porque ser el mismo que el de SAP. Es más, si no viene informado se pone
    " el de SAP.
    mv_process_user = COND #( WHEN iv_process_user IS INITIAL THEN sy-uname ELSE iv_process_user ).

    " La actividad en el proceso de aprobación es edición, porque solo se cambian valores de tablas.
    mv_actvt = cs_actvt-approve.

    " Se determina la acción en base al resultado del paso
    IF iv_step_result = zif_wfe_data=>cs_wf_process-step_result-approve OR iv_step_result = zif_wfe_data=>cs_wf_process-step_result-reject.

      TRY.
          " Valido que se puede aprobar o rechazar el paso del workflow
          check_data_next_step( ).

          " Se determina el siguiente paso segun el resultado del paso.
          " Uno puede pensar que no tiene sentido ponerlo en un rechazo, pero como dejo abierto a que pueda haber distintos tipos rechazos. Tengo
          " que permitir que se determine a que rechazado va a ir, o incluso a que paso irá. Por que el método que determina el siguiente estado
          " tiene una BADI donde el cliente puedo cambiar lo que desee. Además, puede llegar a determinar que el workflow esta completado
          determine_next_status( EXPORTING iv_status = ms_header-status
                                           iv_step_result = iv_step_result
                                 IMPORTING ev_next_status = ev_next_status
                                           ev_workflow_completed = ev_wf_completed ).

          IF ev_next_status IS NOT INITIAL.

            CASE iv_step_result.
              WHEN zif_wfe_data=>cs_wf_process-step_result-approve.
                approve_step( EXPORTING iv_next_status = ev_next_status
                                        iv_workflow_completed = ev_wf_completed ).

              WHEN zif_wfe_data=>cs_wf_process-step_result-reject.
                reject_step( EXPORTING iv_next_status = ev_next_status ).

            ENDCASE.

            " Se lanza el proceso de verificación
            check_consistency( IMPORTING et_return = et_return ).

            " Si no hay errores y no se esta en modo test entonces se graban los datos
            IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
              " Se graban los datos
              save_data( EXPORTING iv_commit = iv_commit
                       IMPORTING et_return = et_return ).

              " Si no hay mensajes de error se devuelve el ID del workflow generado y se devuelve un mensaje indicando que se ha
              " lanzado
              IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).

                " Devuelvo los aprobadores del paso activo, es decir, del nuevo paso determinado
                zif_wfe_workflow~get_approvers_step_active( IMPORTING et_approvers = et_approvers ).

              ENDIF.

            ENDIF.
          ELSE.
            " Mensaje de error que no se ha podido determinar el siguiente aprobador
            INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                            message = zcl_wfe_utilities=>fill_return( iv_number = '026'
                                                                      iv_langu = mv_langu )-message ) INTO TABLE et_return.

          ENDIF.
        CATCH zcx_wfe INTO DATA(lo_wfe).
          INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                         message = zcl_wfe_utilities=>fill_return( iv_id         = lo_wfe->if_t100_message~t100key-msgid
                                                                   iv_number     = lo_wfe->if_t100_message~t100key-msgno
                                                                   iv_message_v1 = lo_wfe->mv_msgv1
                                                                   iv_message_v2 = lo_wfe->mv_msgv2
                                                                   iv_message_v3 = lo_wfe->mv_msgv3
                                                                   iv_message_v4 = lo_wfe->mv_msgv4
                                                                   iv_langu      = mv_langu )-message )
                         INTO TABLE et_return.
      ENDTRY.

    ELSE.
      INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                            message = zcl_wfe_utilities=>fill_return( iv_number = '020' )-message ) INTO TABLE et_return.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wfe_workflow~determine_status_approvers.

    CLEAR et_approvers.

    " Primero se mira el status del workflow para saber el owner. Si no existe el status entonces
    " se genera una excepción
    READ TABLE mt_wf_status ASSIGNING FIELD-SYMBOL(<ls_wf_status>) WITH KEY status = iv_status.
    IF sy-subrc = 0.

      " Puede ser que el paso no tenga owner. En ese caso, no se mirará nada.
      IF <ls_wf_status>-owner IS NOT INITIAL.
        et_approvers = VALUE #( FOR <wa> IN mt_owner_approvers WHERE ( owner = <ls_wf_status>-owner )
                                                               ( approver = <wa>-approver ) ).
      ENDIF.

      " Se llama al método de la BADI que puede cambiar los aprobadores del paso
      call_badi_change_steps_approv( EXPORTING iv_status    = iv_status
                                               it_values    = it_values
                                     CHANGING ct_approvers = et_approvers ).

    ELSE.
      RAISE EXCEPTION TYPE zcx_wfe
        EXPORTING
          textid   = zcx_wfe=>status_not_configured
          mv_msgv1 = CONV #( iv_status ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_wfe_workflow~get_active_status.
    rv_status = ms_header-status.
  ENDMETHOD.


  METHOD zif_wfe_workflow~get_approvers_step_active.
    CLEAR: et_approvers.

    et_approvers = VALUE #( FOR <wa> IN mt_steps_approvers WHERE ( status = ms_header-status ) ( approver = <wa>-approver ) ).

  ENDMETHOD.


  METHOD zif_wfe_workflow~get_data.
    es_header = ms_header.
    et_values = mt_values.
    et_steps = mt_steps.
    et_steps_approvers = mt_steps_approvers.
  ENDMETHOD.


  METHOD zif_wfe_workflow~get_initial_status.
    rv_status = mv_status_init.
  ENDMETHOD.


  METHOD zif_wfe_workflow~is_completed.
    rv_is = COND #( WHEN ms_header-wf_status = zif_wfe_data=>cs_wf_process-wf_status-completed THEN abap_true ELSE abap_false ).
  ENDMETHOD.


  METHOD zif_wfe_workflow~is_draft.

    IF iv_status IS NOT INITIAL.
      rv_is = COND #( WHEN iv_status = mv_status_draft THEN abap_true ELSE abap_false ).
    ELSE.
      rv_is = COND #( WHEN ms_header-status = mv_status_draft THEN abap_true ELSE abap_false ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_wfe_workflow~is_rejected.
    rv_is = COND #( WHEN ms_header-wf_status = zif_wfe_data=>cs_wf_process-wf_status-rejected THEN abap_true ELSE abap_false ).
  ENDMETHOD.


  METHOD zif_wfe_workflow~new.

    CLEAR: ms_header, mt_steps, mt_steps_approvers, mt_steps_approvers, mt_values.

    " Usuario del proceso que no tiene porque ser el mismo que el de SAP. Es más, si no viene informado se pone
    " el de SAP.
    mv_process_user = COND #( WHEN iv_process_user IS INITIAL THEN sy-uname ELSE iv_process_user ).

    " Indico la actividad global. Esto servirá para determinar como realizar determinados pasos
    " dentro de la clase
    mv_actvt = cs_actvt-new.

    TRY.
        " Primer paso son los datos de cabecera
        fill_header_on_creation( ).

        " Ahora los valores
        fill_values( it_values ).

        " Añado un nuevo paso con el status de cabecera que será inicial.
        new_step( iv_status = ms_header-status ).

        " Se determina los aprobadores del paso
        zif_wfe_workflow~determine_status_approvers( EXPORTING iv_status = ms_header-status
                                        it_values = it_values
                              IMPORTING et_approvers = et_approvers ).

        " Relleno de los aprobadores del paso. Los aprobadores del paso se determinan en el motor. Ya que intervienen
        " exit que permiten modificar o determinar aprobadores según el estado.
        fill_step_approvers( EXPORTING iv_status = ms_header-status
                                       it_approvers = et_approvers  ).

        " Se lanza el proceso de verificación
        check_consistency( IMPORTING et_return = et_return ).

        " Si no hay errores y no se esta en modo test entonces se graban los datos
        IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ) AND iv_test_mode = abap_false.
          save_data( EXPORTING iv_commit = iv_commit
                     IMPORTING et_return = et_return ).

          " Si no hay mensajes de error se devuelve el ID del workflow generado y se devuelve un mensaje indicando que se ha
          " lanzado
          IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
            es_result-wf_id = ms_header-wf_id.
            es_result-status = ms_header-status.

          ENDIF.

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


  METHOD zif_wfe_workflow~new_draft.
    CLEAR: ms_header, mt_steps, mt_steps_approvers, mt_steps_approvers, mt_values.

    " El proceso de borrador es muy parecido a la creación normal, la diferencia es que en el borrador
    " no se informan pasos ni aprobadores del paso. El motivo es claro, es un borrador, no se va enviar a nadie.

    " Usuario del proceso que no tiene porque ser el mismo que el de SAP. Es más, si no viene informado se pone
    " el de SAP.
    mv_process_user = COND #( WHEN iv_process_user IS INITIAL THEN sy-uname ELSE iv_process_user ).

    " Indico la actividad global. Esto servirá para determinar como realizar determinados pasos
    " dentro de la clase
    mv_actvt = cs_actvt-draft.

    TRY.
        " Primer paso son los datos de cabecera
        fill_header_on_creation( ).

        " Ahora los valores
        fill_values( it_values ).

        " Se lanza el proceso de verificación
        check_consistency( IMPORTING et_return = et_return ).

        " Si no hay errores y no se esta en modo test entonces se graban los datos
        IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
          save_data( EXPORTING iv_commit = iv_commit
                     IMPORTING et_return = et_return ).

          " Si no hay mensajes de error se devuelve el ID del workflow generado
          IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
            es_result-wf_id = ms_header-wf_id.
            es_result-status = ms_header-status.
          ENDIF.

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


  METHOD zif_wfe_workflow~update_values.

    " Usuario del proceso que no tiene porque ser el mismo que el de SAP. Es más, si no viene informado se pone
    " el de SAP.
    mv_process_user = COND #( WHEN iv_process_user IS INITIAL THEN sy-uname ELSE iv_process_user ).

    " La actividad en el proceso de aprobación es edición, porque solo se cambian valores de tablas.
    mv_actvt = cs_actvt-edit_values.

    " Actualizamos los valores
    fill_values( EXPORTING it_values = it_values ).

    " Grabamos el workflow
    save_data(
      EXPORTING
        iv_commit = iv_commit
      IMPORTING
        et_return = et_return ).

  ENDMETHOD.
ENDCLASS.
