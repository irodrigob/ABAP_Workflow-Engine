class ZCL_WFE_MODEL_DATA_QUERY definition
  public
  create public .

public section.

  types:
    tt_r_wf_id TYPE RANGE OF zwfe_e_wf_id .
  types:
    tt_r_wf_status TYPE RANGE OF zwfe_e_wf_status .
  types:
    tt_r_status TYPE RANGE OF zwfe_e_status .
  types:
    tt_r_approver TYPE RANGE OF zwfe_e_approver .
  types:
    tt_r_workflow TYPE RANGE OF zwfe_e_workflow .
  types:
    tt_r_field TYPE RANGE OF fieldname .
  types:
    tt_r_value TYPE RANGE OF zwfe_e_value .
  types:
    tt_r_step_status TYPE RANGE OF zwfe_e_step_status .
  types:
    tt_r_is_backup TYPE RANGE OF zwfe_e_is_backup .
  types:
    tt_r_user_process TYPE RANGE OF zwfe_e_process_user_create .
  types:
    tt_r_user_sap TYPE RANGE OF syuname .
  types:
    tt_r_date TYPE RANGE OF sydatum .
  types:
    tt_r_time TYPE RANGE OF syuzeit .

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
  methods CONSTRUCTOR
    importing
      !IV_LANGU type SY-LANGU default SY-LANGU .
    "! <p class="shorttext synchronized">Get all data of workflow ID</p>
  methods GET_ALL_WF_ID_DATA
    importing
      !IT_R_WF_ID type TT_R_WF_ID
    exporting
      !ET_HEADER type ZWFE_I_HEADER_ALL_FIELDS
      !ET_STEPS type ZWFE_I_STEPS_ALL_FIELDS
      !ET_STEPS_APPROVERS type ZWFE_I_STEPS_APPROV_ALL_FIELDS
      !ET_VALUES type ZWFE_I_VALUES_ALL_FIELDS .
    "! <p class="shorttext synchronized">Get all data </p>
  methods GET_ALL_DATA
    importing
      !IT_PARAMS_SL type ZIF_WFE_DATA=>TT_PARAMS_SL
    exporting
      !ET_HEADER type ZWFE_I_HEADER_ALL_FIELDS
      !ET_STEPS type ZWFE_I_STEPS_ALL_FIELDS
      !ET_STEPS_APPROVERS type ZWFE_I_STEPS_APPROV_ALL_FIELDS
      !ET_VALUES type ZWFE_I_VALUES_ALL_FIELDS
      !ET_ALL_DATA type ZWFE_I_WF_ALL_DATA .
    "! <p class="shorttext synchronized">Get header data</p>
  methods GET_HEADER_DATA
    importing
      !IT_PARAMS_SL type ZIF_WFE_DATA=>TT_PARAMS_SL
    exporting
      !ET_HEADER type ZWFE_I_HEADER_ALL_FIELDS .
    "! <p class="shorttext synchronized">Get values data</p>
  methods GET_VALUES_DATA
    importing
      !IT_PARAMS_SL type ZIF_WFE_DATA=>TT_PARAMS_SL
    exporting
      !ET_VALUES type ZWFE_I_VALUES_ALL_FIELDS .
    "! <p class="shorttext synchronized">Get steps data</p>
  methods GET_STEPS_DATA
    importing
      !IT_PARAMS_SL type ZIF_WFE_DATA=>TT_PARAMS_SL
    exporting
      !ET_STEPS type ZWFE_I_STEPS_ALL_FIELDS .
    "! <p class="shorttext synchronized">Get steps approvers data</p>
  methods GET_STEPS_APPROVERS_DATA
    importing
      !IT_PARAMS_SL type ZIF_WFE_DATA=>TT_PARAMS_SL
    exporting
      !ET_STEPS_APPROVERS type ZWFE_I_STEPS_APPROV_ALL_FIELDS .
    "! <p class="shorttext synchronized">Get the changelog data</p>
  methods GET_CHANGELOG_DATA
    importing
      !IT_R_WF_ID type TT_R_WF_ID
    exporting
      !ET_CHANGELOG_DATA type ZWFE_I_WF_CHANGELOG_DATA .
  PROTECTED SECTION.

    DATA mv_langu TYPE sy-langu .
    DATA mt_params_sl TYPE zif_wfe_data=>tt_params_sl .

    "! <p class="shorttext synchronized">Split all data itab in the separated tables</p>
    METHODS split_all_data_in_tables
      IMPORTING
        !it_data            TYPE zwfe_i_wf_all_data
      EXPORTING
        !et_header          TYPE zwfe_i_header_all_fields
        !et_values          TYPE zwfe_i_values_all_fields
        !et_steps           TYPE zwfe_i_steps_all_fields
        !et_steps_approvers TYPE zwfe_i_steps_approv_all_fields .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WFE_MODEL_DATA_QUERY IMPLEMENTATION.


  METHOD constructor.
    mv_langu = iv_langu.
  ENDMETHOD.


  METHOD get_all_data.
    DATA: lt_r_wf_id TYPE tt_r_wf_id.
    DATA: lt_r_header_status TYPE tt_r_wf_status.
    DATA: lt_r_wf_status TYPE tt_r_wf_status.
    DATA: lt_r_workflow TYPE tt_r_approver.
    DATA lt_r_field TYPE tt_r_field.
    DATA lt_r_value TYPE tt_r_value.
    DATA: lt_r_steps_status TYPE tt_r_wf_status.
    DATA lt_r_step_status TYPE tt_r_step_status.
    DATA lt_r_approver_by TYPE tt_r_approver.
    DATA lt_r_is_backup TYPE tt_r_is_backup.
    DATA: lt_r_steps_approver_status TYPE tt_r_wf_status.
    DATA lt_r_approver TYPE tt_r_approver.
    DATA lt_r_ernam_process TYPE tt_r_user_process.
    DATA lt_r_ernam TYPE tt_r_user_sap.
    DATA lt_r_erzet TYPE tt_r_time.
    DATA lt_r_erdat TYPE tt_r_date.
    DATA lt_r_aenam_process TYPE tt_r_user_process.
    DATA lt_r_aenam TYPE tt_r_user_sap.
    DATA lt_r_aetim TYPE tt_r_time.
    DATA lt_r_aedat TYPE tt_r_date.

    CLEAR: et_all_data, et_header, et_steps, et_steps_approvers, et_values.

    " Convierto los parametros de seleccion pasados a ranges para realizar las búsquedas
    " Campos de cabecera
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-wf_id
                                                        it_params_sl = it_params_sl
                                              IMPORTING et_r_ranges = lt_r_wf_id ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-status
                                                           it_params_sl = it_params_sl
                                                 IMPORTING et_r_ranges = lt_r_header_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-wf_status
                                                           it_params_sl = it_params_sl
                                                 IMPORTING et_r_ranges = lt_r_wf_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-workflow
                                                              it_params_sl = it_params_sl
                                                    IMPORTING et_r_ranges = lt_r_workflow ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-ernam_process
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_ernam_process ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-ernam
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_ernam ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-creation_time
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_erzet ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-creation_date
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_erdat ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-aenam_process
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aenam_process ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-aenam
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aenam ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-change_time
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aetim ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-change_date
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aedat ).

    " Valores
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-values-fields-field
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_field ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-values-fields-value
                                                           it_params_sl = it_params_sl
                                                 IMPORTING et_r_ranges = lt_r_value ).

    " Steps
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps-fields-status
                                                           it_params_sl = it_params_sl
                                                 IMPORTING et_r_ranges = lt_r_steps_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps-fields-step_status
                                                               it_params_sl = it_params_sl
                                                     IMPORTING et_r_ranges = lt_r_step_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps-fields-approved_by
                                                                   it_params_sl = it_params_sl
                                                         IMPORTING et_r_ranges = lt_r_approver_by ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps-fields-is_backup
                                                                   it_params_sl = it_params_sl
                                                         IMPORTING et_r_ranges = lt_r_is_backup ).

    " Steps approvers
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps_approvers-fields-status
                                                              it_params_sl = it_params_sl
                                                    IMPORTING et_r_ranges = lt_r_steps_approver_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps_approvers-fields-approver
                                                                      it_params_sl = it_params_sl
                                                            IMPORTING et_r_ranges = lt_r_approver ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-ernam_process
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_ernam_process ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-ernam
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_ernam ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-creation_time
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_erzet ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-creation_date
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_erdat ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-aenam_process
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aenam_process ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-aenam
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aenam ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-change_time
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aetim ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-change_date
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aedat ).

    " La búsqueda va a consistir en dos pasos. La primera se buscará por cualquier campo pasado por parámetro,
    " pero solo servirá para obtener los ID. El motivo, es que segun porque campos se filtren, como los de la tabla valores,
    " la búsqueda no nos darán la imagen completa del workflo. Por ello, hay que hacerlo en dos pasos
    SELECT DISTINCT header_wf_id FROM zwfecwfdata( p_langu = @mv_langu )
        WHERE header_wf_id IN @lt_r_wf_id
              AND header_status IN @lt_r_header_status
              AND header_wf_status IN @lt_r_wf_status
              AND header_workflow IN @lt_r_workflow
              AND header_ernam_process IN @lt_r_ernam_process
              AND header_ernam IN @lt_r_ernam
              AND header_erzet IN @lt_r_erzet
              AND header_erdat IN @lt_r_erdat
              AND header_aenam_process IN @lt_r_aenam_process
              AND header_aenam IN @lt_r_aenam
              AND header_aetim IN @lt_r_aetim
              AND header_aedat IN @lt_r_aedat
              AND values_field IN @lt_r_field
              AND values_value IN @lt_r_value
              AND steps_status IN @lt_r_steps_status
              AND steps_step_status IN @lt_r_step_status
              AND steps_approved_by IN @lt_r_approver_by
              AND steps_is_backup IN @lt_r_is_backup
              AND stepsapprovers_status IN @lt_r_steps_approver_status
              AND stepsapprovers_approver IN @lt_r_approver
        INTO TABLE @DATA(lt_wf_id).
    IF sy-subrc = 0.
      CLEAR: lt_r_wf_id.
      lt_r_wf_id = VALUE #( FOR <wa> IN lt_wf_id ( sign = 'I' option = 'EQ' low = <wa>-header_wf_id ) ).

      " Reemplazo la búsqueda directa al CDS por la del método que aunque puede que no sea tan, tan optima (aunque ese CDS) pero me garantiza que se
      " devuelvan todos los campos (incluidos los campos string)
      get_all_wf_id_data(
        EXPORTING
          it_r_wf_id         = VALUE #( FOR <wa> IN lt_wf_id ( sign = 'I' option = 'EQ' low = <wa>-header_wf_id ) )
        IMPORTING
          et_header          = et_header
          et_steps           = et_steps
          et_steps_approvers = et_steps_approvers
          et_values          = et_values ).

*      " Para prevenir futuras consultas muy gordas y evitar el dumps por ranges demasiado grande. Captura, la posible excepción
*      " y controlar para que se busque por for all entries. Que aunque, no es la más optima no petará por tamaño
*      TRY.
*          SELECT * FROM zwfecwfdata( p_langu = @mv_langu )
*           WHERE header_wf_id IN @lt_r_wf_id
*           INTO TABLE @et_all_data.
*        CATCH cx_root.
*          SELECT * FROM zwfecwfdata( p_langu = @mv_langu )
*          FOR ALL ENTRIES IN @lt_r_wf_id
*                 WHERE header_wf_id = @lt_r_wf_id-low
*                 INTO TABLE @et_all_data.
*      ENDTRY.
*      " Hago uno split de los datos para dividirlos en las distintas tablas del worklow
*      split_all_data_in_tables( EXPORTING it_data = et_all_data
*                                IMPORTING  et_header          = et_header
*                                          et_values          = et_values
*                                          et_steps           = et_steps
*                                          et_steps_approvers = et_steps_approvers ).
    ENDIF.
  ENDMETHOD.


  METHOD get_all_wf_id_data.

    CLEAR: et_header, et_steps, et_steps_approvers, et_values.

    " Paso el ranges al formato de selección de datos
    DATA(lt_params_sl_header) =  VALUE zif_wfe_data=>tt_params_sl( FOR <wa> IN it_r_wf_id ( selname = zif_wfe_data=>cs_model_data-header-fields-wf_id
                                                                               kind = 'S'
                                                                               sign = <wa>-sign
                                                                               option = <wa>-option
                                                                               low = <wa>-low
                                                                               high = <wa>-high ) ).

    " Búsqueda de datos de cabecera
    get_header_data( EXPORTING it_params_sl = lt_params_sl_header
                     IMPORTING et_header = et_header ).

    IF et_header IS NOT INITIAL. " Si hay datos se buscan el resto
      " Para reducir el tamaño de la búsqueda construyo los parámetros de selección
      " con los wf_id encontrados en la cabecera
      DATA(lt_params_sl) =  VALUE zif_wfe_data=>tt_params_sl( FOR <wa1> IN et_header ( selname = zif_wfe_data=>cs_model_data-header-fields-wf_id
                                                                          kind = 'S'
                                                                          sign = 'I'
                                                                          option = 'EQ'
                                                                          low = <wa1>-wf_id ) ).

      " Valores del workflow
      get_values_data( EXPORTING it_params_sl = lt_params_sl
                       IMPORTING et_values = et_values ).

      " Pasos del workflow
      get_steps_data( EXPORTING it_params_sl = lt_params_sl
                       IMPORTING et_steps = et_steps ).

      " aprobadores del pasos del workflow
      get_steps_approvers_data( EXPORTING it_params_sl = lt_params_sl
                                IMPORTING et_steps_approvers = et_steps_approvers ).

    ENDIF.


  ENDMETHOD.


  METHOD get_changelog_data.

    CLEAR: et_changelog_data.

    SELECT *
           FROM zwfeichnglog( p_langu = @mv_langu )
           WHERE wf_id IN @it_r_wf_id
           ORDER BY erdat DESCENDING, erzet DESCENDING
           INTO TABLE @et_changelog_data.

  ENDMETHOD.


  METHOD get_header_data.
    DATA: lt_r_wf_id TYPE tt_r_wf_id.
    DATA: lt_r_status TYPE tt_r_wf_status.
    DATA: lt_r_wf_status TYPE tt_r_wf_status.
    DATA: lt_r_workflow TYPE tt_r_approver.
    DATA lt_r_ernam_process TYPE tt_r_user_process.
    DATA lt_r_ernam TYPE tt_r_user_sap.
    DATA lt_r_erzet TYPE tt_r_time.
    DATA lt_r_erdat TYPE tt_r_date.
    DATA lt_r_aenam_process TYPE tt_r_user_process.
    DATA lt_r_aenam TYPE tt_r_user_sap.
    DATA lt_r_aetim TYPE tt_r_time.
    DATA lt_r_aedat TYPE tt_r_date.

    CLEAR: et_header.

    " Convierto los parametros de seleccion pasados a ranges para realizar las búsquedas
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-wf_id
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_wf_id ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-status
                                                           it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-wf_status
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_wf_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-workflow
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_workflow ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-ernam_process
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_ernam_process ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-ernam
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_ernam ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-creation_time
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_erzet ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-creation_date
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_erdat ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-aenam_process
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aenam_process ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-aenam
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aenam ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-change_time
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aetim ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-header-fields-change_date
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_aedat ).
    SELECT *
           FROM zwfeiheader( p_langu = @mv_langu )
           WHERE wf_id IN @lt_r_wf_id
                 AND status IN @lt_r_status
                 AND wf_status IN @lt_r_wf_status
                 AND workflow IN @lt_r_workflow
                 AND ernam_process IN @lt_r_ernam_process
                 AND ernam IN @lt_r_ernam
                 AND erzet IN @lt_r_erzet
                 AND erdat IN @lt_r_erdat
                 AND aenam_process IN @lt_r_aenam_process
                 AND aenam IN @lt_r_aenam
                 AND aetim IN @lt_r_aetim
                 AND aedat IN @lt_r_aedat
           INTO CORRESPONDING FIELDS OF TABLE @et_header                 .

  ENDMETHOD.


  METHOD get_steps_approvers_data.
    DATA: lt_r_wf_id TYPE tt_r_wf_id.
    DATA: lt_r_status TYPE tt_r_wf_status.
    DATA lt_r_approver TYPE tt_r_approver.

    CLEAR: et_steps_approvers.

    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps_approvers-fields-wf_id
                                         it_params_sl = it_params_sl
                               IMPORTING et_r_ranges = lt_r_wf_id ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps_approvers-fields-status
                                                           it_params_sl = it_params_sl
                                                 IMPORTING et_r_ranges = lt_r_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps_approvers-fields-approver
                                                                      it_params_sl = it_params_sl
                                                            IMPORTING et_r_ranges = lt_r_approver ).

    SELECT *
           FROM zwfeistepsaprv( p_langu = @mv_langu )
           WHERE wf_id IN @lt_r_wf_id
                 AND status IN @lt_r_status
                 AND approver IN @lt_r_approver
           INTO CORRESPONDING FIELDS OF TABLE @et_steps_approvers.

  ENDMETHOD.


  METHOD get_steps_data.
    DATA: lt_r_wf_id TYPE tt_r_wf_id.
    DATA: lt_r_status TYPE tt_r_wf_status.
    DATA lt_r_step_status TYPE tt_r_step_status.
    DATA lt_r_approver TYPE tt_r_approver.
    DATA lt_r_is_backup TYPE tt_r_is_backup.

    CLEAR: et_steps.

    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps-fields-wf_id
                                             it_params_sl = it_params_sl
                                   IMPORTING et_r_ranges = lt_r_wf_id ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps-fields-status
                                                           it_params_sl = it_params_sl
                                                 IMPORTING et_r_ranges = lt_r_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps-fields-step_status
                                                               it_params_sl = it_params_sl
                                                     IMPORTING et_r_ranges = lt_r_step_status ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps-fields-approved_by
                                                                   it_params_sl = it_params_sl
                                                         IMPORTING et_r_ranges = lt_r_approver ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-steps-fields-is_backup
                                                                   it_params_sl = it_params_sl
                                                         IMPORTING et_r_ranges = lt_r_is_backup ).

    SELECT *
           FROM zwfeisteps( p_langu = @mv_langu )
           WHERE wf_id IN @lt_r_wf_id
                 AND status IN @lt_r_status
                 AND step_status IN @lt_r_step_status
                 AND approved_by IN @lt_r_approver
                 AND is_backup IN @lt_r_is_backup
           INTO CORRESPONDING FIELDS OF TABLE @et_steps.
  ENDMETHOD.


  METHOD get_values_data.
    DATA: lt_r_wf_id TYPE tt_r_wf_id.
    DATA lt_r_field TYPE tt_r_field.
    DATA lt_r_value TYPE tt_r_value.

    CLEAR: et_values.

    " Convierto los parametros de seleccion pasados a ranges para realizar las búsquedas
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-values-fields-wf_id
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_wf_id ).

    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-values-fields-field
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_field ).
    zcl_wfe_utilities=>conv_params_2_ranges( EXPORTING iv_name  = zif_wfe_data=>cs_model_data-values-fields-value
                                                           it_params_sl = it_params_sl
                                                 IMPORTING et_r_ranges = lt_r_value ).

    SELECT *
           FROM zwfeivalues
           WHERE wf_id IN @lt_r_wf_id
                 AND field IN @lt_r_field
                 AND value IN @lt_r_value
           INTO CORRESPONDING FIELDS OF TABLE @et_values.

  ENDMETHOD.


  METHOD split_all_data_in_tables.

    CLEAR: et_header, et_steps, et_steps_approvers, et_values.

    IF it_data IS NOT INITIAL.

      " Obtengo los componentes de la tabla para saber donde tienen que ir. Porque el CDS del que
      " se alimenta los datos tiene un prefijo para saber de donde es cada dato.
      DATA(lt_components) = zcl_wfe_utilities=>get_struct_components_recus( it_data[ 1 ] ).


      LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        " Variables de control para saber cuando tengo que insertar un registro en blanco para guardar los valores
        DATA(lv_add_header) = abap_false.
        DATA(lv_add_values) = abap_false.
        DATA(lv_add_steps) = abap_false.
        DATA(lv_add_steps_approvers) = abap_false.

        LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_components>).
          ASSIGN COMPONENT <ls_components>-name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<field_data>).

          SPLIT <ls_components>-name AT '_' INTO: DATA(lv_prefix) DATA(lv_field).
          CASE lv_prefix.
            WHEN zif_wfe_data=>cs_model_data-header-prefix.
              IF lv_add_header = abap_false.
                lv_add_header = abap_true.
                APPEND INITIAL LINE TO et_header ASSIGNING FIELD-SYMBOL(<ls_header>).
              ENDIF.
              ASSIGN COMPONENT lv_field OF STRUCTURE <ls_header> TO FIELD-SYMBOL(<field_header>).
              IF sy-subrc = 0.
                <field_header> = <field_data>.
              ENDIF.
            WHEN zif_wfe_data=>cs_model_data-values-prefix.
              IF lv_add_values = abap_false.
                lv_add_values = abap_true.
                APPEND INITIAL LINE TO et_values ASSIGNING FIELD-SYMBOL(<ls_values>).
              ENDIF.
              ASSIGN COMPONENT lv_field OF STRUCTURE <ls_values> TO FIELD-SYMBOL(<field_values>).
              IF sy-subrc = 0.
                <field_values> = <field_data>.
              ENDIF.
            WHEN zif_wfe_data=>cs_model_data-steps-prefix.
              IF lv_add_steps = abap_false.
                lv_add_steps = abap_true.
                APPEND INITIAL LINE TO et_steps ASSIGNING FIELD-SYMBOL(<ls_steps>).
              ENDIF.
              ASSIGN COMPONENT lv_field OF STRUCTURE <ls_steps> TO FIELD-SYMBOL(<field_steps>).
              IF sy-subrc = 0.
                <field_steps> = <field_data>.
              ENDIF.
            WHEN zif_wfe_data=>cs_model_data-steps_approvers-prefix.
              IF lv_add_steps_approvers = abap_false.
                lv_add_steps_approvers = abap_true.
                APPEND INITIAL LINE TO et_steps_approvers ASSIGNING FIELD-SYMBOL(<ls_steps_approvers>).
              ENDIF.
              ASSIGN COMPONENT lv_field OF STRUCTURE <ls_steps_approvers> TO FIELD-SYMBOL(<field_steps_approvers>).
              IF sy-subrc = 0.
                <field_steps_approvers> = <field_data>.
              ENDIF.
          ENDCASE.
        ENDLOOP.

      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM et_header USING KEY primary_key.
      DELETE ADJACENT DUPLICATES FROM et_values USING KEY primary_key.
      DELETE ADJACENT DUPLICATES FROM et_steps USING KEY primary_key.
      DELETE ADJACENT DUPLICATES FROM et_steps_approvers USING KEY primary_key.

      " Borro aquellas tablas que puedan estar en blanco debido a que no siempre pueden tener datos. La manera
      " de identificarlo es con el campo WF_ID.
      DELETE et_steps_approvers WHERE wf_id IS INITIAL.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
