class ZCL_WFE_MODEL_DATA_CRUD definition
  public
  create public .

public section.

  types:
    BEGIN OF ts_crud_options,
        commit       TYPE sap_bool,
        langu        TYPE sy-langu,
        process_user TYPE zwfe_e_process_user,
      END OF ts_crud_options .
  types:
    BEGIN OF ts_header.
        INCLUDE TYPE zwfe_s_header_key_fields.
        INCLUDE TYPE zwfe_s_header_fields.
      TYPES:
      END OF ts_header .
  types:
    BEGIN OF ts_yheader.
        INCLUDE TYPE ts_header.
        INCLUDE TYPE zwfe_s_changelog_fields.
      TYPES:
      END OF ts_yheader .
  types:
    BEGIN OF ts_values.
        INCLUDE TYPE zwfe_s_header_key_fields.
        INCLUDE TYPE zwfe_s_values_key_fields.
        INCLUDE TYPE zwfe_s_values_fields.
      TYPES:
      END OF ts_values .
  types:
    tt_values TYPE STANDARD TABLE OF ts_values WITH EMPTY KEY .
  types:
    BEGIN OF ts_steps.
        INCLUDE TYPE zwfe_s_steps_key_fields.
        INCLUDE TYPE zwfe_s_steps_fields.
      TYPES: END OF ts_steps .
  types:
    tt_steps TYPE STANDARD TABLE OF ts_steps WITH EMPTY KEY .
  types:
    BEGIN OF ts_ysteps.
        INCLUDE TYPE ts_steps.
        INCLUDE TYPE zwfe_s_changelog_fields.
      TYPES:
      END OF ts_ysteps .
  types:
    tt_ysteps TYPE STANDARD TABLE OF ts_ysteps WITH EMPTY KEY .
  types:
    BEGIN OF ts_steps_approvers.
        INCLUDE TYPE zwfe_s_steps_approv_key_fields.
        INCLUDE TYPE zwfe_s_steps_approv_fields.
      TYPES: END OF ts_steps_approvers .
  types:
    tt_steps_approvers TYPE STANDARD TABLE OF ts_steps_approvers WITH EMPTY KEY .
  types TS_CHANGELOG_HEADER type ZWFE_T009 .
  types TS_CHANGELOG_DATA type ZWFE_T010 .
  types:
    tt_changelog_data TYPE STANDARD TABLE OF ts_changelog_data .

    "! <p class="shorttext synchronized">Gestiona las peticiones actualización</p>
  methods DISPATCH_DATA
    importing
      !IS_OPTIONS type TS_CRUD_OPTIONS optional
      !IV_UPDKZ type ZWFE_E_CHANGE_IND
      !IS_HEADER type TS_HEADER optional
      !IT_VALUES type TT_VALUES optional
      !IT_STEPS type TT_STEPS optional
      !IT_STEPS_APPROVERS type TT_STEPS_APPROVERS optional
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN
      !ES_HEADER_RESULT type TS_HEADER
      !ET_VALUES_RESULT type TT_VALUES
      !ET_STEPS_RESULT type TT_STEPS
      !ET_STEPS_APPROVERS_RESULT type TT_STEPS_APPROVERS .
protected section.

  data MS_XHEADER type TS_HEADER .
  data MS_YHEADER type TS_YHEADER .
  data MT_XVALUES type TT_VALUES .
  data MT_YVALUES type TT_VALUES .
  data MT_XSTEPS type TT_STEPS .
  data MT_YSTEPS type TT_YSTEPS .
  data MT_XSTEPS_APPROVERS type TT_STEPS_APPROVERS .
  data MT_YSTEPS_APPROVERS type TT_STEPS_APPROVERS .
  data MS_OPTIONS type TS_CRUD_OPTIONS .
  data MT_CHANGELOG_DATA type TT_CHANGELOG_DATA .
  data MV_UPDKZ type ZWFE_E_CHANGE_IND .

    "! <p class="shorttext synchronized">Edit workflow</p>
  methods EDIT_WORKFLOW
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
    "! <p class="shorttext synchronized">Add a new workflow</p>
  methods NEW_WORKFLOW
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
    "! <p class="shorttext synchronized">Save data to database</p>
  methods WRITE_TO_DB
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
    "! <p class="shorttext synchronized">Save header data to database</p>
  methods WRITE_HEADER_DATA
    raising
      ZCX_WFE .
    "! <p class="shorttext synchronized">Save values data to database</p>
  methods WRITE_VALUES_DATA
    raising
      ZCX_WFE .
    "! <p class="shorttext synchronized">Save steps data to database</p>
  methods WRITE_STEPS_DATA
    raising
      ZCX_WFE .
    "! <p class="shorttext synchronized">Save steps user data to database</p>
  methods WRITE_STEPS_USER_DATA
    raising
      ZCX_WFE .
    "! <p class="shorttext synchronized">Save changelog data to database</p>
  methods WRITE_CHANGELOG_DATA
    raising
      ZCX_WFE .
    "! <p class="shorttext synchronized">Complete delete of a workflow</p>
  methods DELETE_COMPLETE_WORKFLOW
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
    "! <p class="shorttext synchronized">Read header data</p>
  methods READ_HEADER_DATA
    raising
      ZCX_WFE .
    "! <p class="shorttext synchronized">Read values data</p>
  methods READ_VALUES_DATA .
    "! <p class="shorttext synchronized">Read steps data</p>
  methods READ_STEPS_DATA .
    "! <p class="shorttext synchronized">Read steps approver data</p>
  methods READ_STEPS_APPROVERS_DATA .
    "! <p class="shorttext synchronized">Fill the basic fields of the values</p>
  methods FILL_BASIC_DATA_VALUES .
    "! <p class="shorttext synchronized">Fill the basic fields of the steps</p>
  methods FILL_BASIC_DATA_STEPS .
    "! <p class="shorttext synchronized">Fill the basic fields of the steps approvers</p>
  methods FILL_BASIC_DATA_STEPS_APPROV .
    "! <p class="shorttext synchronized">Continue workflow step</p>
  methods CONTINUE_WORKFLOW_STEP
    exporting
      !ET_RETURN type ZIF_WFE_DATA=>TT_RETURN .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WFE_MODEL_DATA_CRUD IMPLEMENTATION.


  METHOD continue_workflow_step.

    " El proceso de continue workflow step es muy parecido al de editar pero sin tener en cuenta los valores
    " Para editar tiene que estar informado el ID del workflow
    IF ms_xheader-wf_id IS NOT INITIAL.

      TRY.
          " El primer paso es recuperar los valores previos. Si el WF_ID no existe el método
          " lanzará una excepción que hará que no se pueda continuar el proceso
          read_header_data( ).

          " Lectura de los pasos previos
          read_steps_data( ).


          " La teoria dice que el los valores que me puedan venir deberían de tener el campo WF_ID informado. Pero
          " prefiero curarme y lanzar el proceso que rellena los valores básicos para que se grabe bien.
          fill_basic_data_steps( ).
          fill_basic_data_steps_approv( ).

          " Se graban los datos en base de datos
          write_to_db( IMPORTING et_return = et_return ).

          " Si no hay errores se pone un mensaje generico
          IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
            INSERT VALUE #( type = zif_wfe_data=>cs_message-type-sucess
                            message = zcl_wfe_utilities=>fill_return( iv_number = '009' )-message ) INTO TABLE et_return.
          ENDIF.

        CATCH zcx_wfe INTO DATA(lx_wfe).
      ENDTRY.
    ELSE.
      INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                         message = zcl_wfe_utilities=>fill_return( iv_number = '006' )-message ) INTO TABLE et_return.
    ENDIF.
  ENDMETHOD.


  METHOD delete_complete_workflow.

    " El borrado completo de un workflow se eliminan todos los datos de todas las tablas
    IF ms_xheader-wf_id IS NOT INITIAL.

      DELETE FROM zwfe_t007 WHERE wf_id = ms_xheader-wf_id.
      IF sy-subrc = 0.
        DELETE FROM zwfe_t008 WHERE wf_id = ms_xheader-wf_id.
        DELETE FROM zwfe_t009 WHERE wf_id = ms_xheader-wf_id.
        DELETE FROM zwfe_t010 WHERE wf_id = ms_xheader-wf_id.
        DELETE FROM zwfe_t011 WHERE wf_id = ms_xheader-wf_id.
        DELETE FROM zwfe_t012 WHERE wf_id = ms_xheader-wf_id.

        IF ms_options-commit = abap_true.
          COMMIT WORK AND WAIT.
        ENDIF.
        INSERT VALUE #( type = zif_wfe_data=>cs_message-type-sucess
                           message = zcl_wfe_utilities=>fill_return( iv_number = '008' )-message ) INTO TABLE et_return.
      ELSE.
        INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                           message = zcl_wfe_utilities=>fill_return( iv_number = '007' )-message ) INTO TABLE et_return.
      ENDIF.

    ELSE.
      INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                         message = zcl_wfe_utilities=>fill_return( iv_number = '006' )-message ) INTO TABLE et_return.
    ENDIF.

  ENDMETHOD.


  METHOD dispatch_data.

    CLEAR: es_header_result, et_return, et_values_result.

    " Pasamos todos los datos variables globales para procesarlo dentro de la clase
    ms_options = is_options.
    ms_options-langu = COND #( WHEN ms_options-langu IS NOT INITIAL THEN ms_options-langu ELSE sy-langu ).
    ms_xheader = is_header.
    mt_xvalues = it_values.
    mt_xsteps = it_steps.
    mt_xsteps_approvers = it_steps_approvers.
    mv_updkz = iv_updkz.

    CASE iv_updkz.
      WHEN zif_wfe_data=>cs_model_data-update_ind-new_wf.
        new_workflow( IMPORTING et_return = et_return ).
      WHEN zif_wfe_data=>cs_model_data-update_ind-edit_wf.
        edit_workflow( IMPORTING et_return = et_return ).
      WHEN zif_wfe_data=>cs_model_data-update_ind-full_delete_wf.
        delete_complete_workflow( IMPORTING et_return = et_return ).
      WHEN zif_wfe_data=>cs_model_data-update_ind-continue_step.
        continue_workflow_step( IMPORTING et_return = et_return ).
    ENDCASE.


    " Si hay mensaje de error el resultado es el mismo que el de entrada.
    " En caso contrario se devuelve lo que ha podido ser modificado
    IF line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
      es_header_result = is_header.
      et_values_result = it_values.
    ELSE.
      es_header_result = ms_xheader.
      et_values_result = mt_xvalues.
    ENDIF.

  ENDMETHOD.


  METHOD edit_workflow.

    " Para editar tiene que estar informado el ID del workflow
    IF ms_xheader-wf_id IS NOT INITIAL.

      TRY.
          " El primer paso es recuperar los valores previos. Si el WF_ID no existe el método
          " lanzará una excepción que hará que no se pueda continuar el proceso
          read_header_data( ).

          " Lectura de los valores previos
          read_values_data( ).

          " Lectura de los pasos previos
          read_steps_data( ).

          " La teoria dice que el los valores que me puedan venir deberían de tener el campo WF_ID informado. Pero
          " prefiero curarme y lanzar el proceso que rellena los valores básicos para que se grabe bien.
          fill_basic_data_values( ).
          fill_basic_data_steps( ).
          fill_basic_data_steps_approv( ).

          " Se graban los datos en base de datos
          write_to_db( IMPORTING et_return = et_return ).

          " Si no hay errores se pone un mensaje generico
          IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
            INSERT VALUE #( type = zif_wfe_data=>cs_message-type-sucess
                            message = zcl_wfe_utilities=>fill_return( iv_number = '009' )-message ) INTO TABLE et_return.
          ENDIF.

        CATCH zcx_wfe INTO DATA(lx_wfe).
      ENDTRY.
    ELSE.
      INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                         message = zcl_wfe_utilities=>fill_return( iv_number = '006' )-message ) INTO TABLE et_return.
    ENDIF.
  ENDMETHOD.


  METHOD fill_basic_data_steps.
    LOOP AT mt_xsteps ASSIGNING FIELD-SYMBOL(<ls_steps>).
      <ls_steps>-wf_id = ms_xheader-wf_id.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_basic_data_steps_approv.
    LOOP AT mt_xsteps_approvers ASSIGNING FIELD-SYMBOL(<ls_steps_user>).
      <ls_steps_user>-wf_id = ms_xheader-wf_id.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_basic_data_values.
    LOOP AT mt_xvalues ASSIGNING FIELD-SYMBOL(<ls_values>).
      <ls_values>-wf_id = ms_xheader-wf_id.
    ENDLOOP.
  ENDMETHOD.


  METHOD new_workflow.

    " Creación de un workflow requiere tener informado los datos de cabecera. Y los valores son opcionales.

    " Primera validación, tiene que haber datos de cabecera.
    IF ms_xheader IS NOT INITIAL.
      " Se genera el id de workflow en la creación, si no viene informado.
      " No debería ocurrir pero lo dejo para garantizar la integridad de datos
      IF ms_xheader-wf_id IS INITIAL.
        ms_xheader-wf_id = /bobf/cl_frw_factory=>get_new_key( ).
      ENDIF.

      " Se rellenan los valores básicos para que los valores se graben bien.
      fill_basic_data_values( ).

      " Mismo proceso para los pasos
      fill_basic_data_steps( ).

      " lo mismo para los usuario del paso
      fill_basic_data_steps_approv( ).


      " Se graban los datos en base de datos
      write_to_db( IMPORTING et_return = et_return ).

      " Si no hay errores se pone un mensaje generico
      IF NOT line_exists( et_return[ type = zif_wfe_data=>cs_message-type-error ] ).
        INSERT VALUE #( type = zif_wfe_data=>cs_message-type-sucess
                        message = zcl_wfe_utilities=>fill_return( iv_number = '005' )-message ) INTO TABLE et_return.
      ENDIF.

    ELSE.

      INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                      message = zcl_wfe_utilities=>fill_return( iv_number = '001' )-message ) INTO TABLE et_return.
    ENDIF.


  ENDMETHOD.


  METHOD read_header_data.

    " Buscamos todos los datos
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ms_yheader
          FROM zwfe_t007
          WHERE wf_id = ms_xheader-wf_id.
    IF sy-subrc NE 0. " Si no existe se lanza excepción
      RAISE EXCEPTION TYPE zcx_wfe
        EXPORTING
          textid = zcx_wfe=>wf_id_not_exist.
    ENDIF.

  ENDMETHOD.


  METHOD read_steps_approvers_data.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_ysteps_approvers
                FROM zwfe_t012
                WHERE wf_id = ms_xheader-wf_id.
  ENDMETHOD.


  METHOD read_steps_data.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_ysteps
               FROM zwfe_t011
               WHERE wf_id = ms_xheader-wf_id.
  ENDMETHOD.


  METHOD read_values_data.
    " Aquí no hay control si existe o no, porque puede ocurrir que no haya valores
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_yvalues
            FROM zwfe_t008
            WHERE wf_id = ms_xheader-wf_id.
  ENDMETHOD.


  METHOD write_changelog_data.

    " Siempre se graba en la cabecera del changelog la acción solicitada.
    DATA(ls_header_db) = VALUE zwfe_t009( wf_id = ms_xheader-wf_id
                                       change_id = /bobf/cl_frw_factory=>get_new_key( )
                                       erdat = sy-datum
                                       ernam = sy-uname
                                       erzet = sy-uzeit
                                       ernam_process = ms_options-process_user
                                       change_ind = mv_updkz ).
    MODIFY zwfe_t009 FROM ls_header_db.
    IF sy-subrc = 0.

      " Se informan los cambios según se hayan ido informado en las actualizaciones de las tablas
      IF mt_changelog_data IS NOT INITIAL.

        " Actualizo los campos clave de los datos
        LOOP AT mt_changelog_data ASSIGNING FIELD-SYMBOL(<ls_changelog_data>).
          <ls_changelog_data>-wf_id = ls_header_db-wf_id.
          <ls_changelog_data>-change_id = ls_header_db-change_id.
        ENDLOOP.

        MODIFY zwfe_t010 FROM TABLE mt_changelog_data.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_wfe
            EXPORTING
              textid = zcx_wfe=>error_save_changelog.
        ENDIF.


      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_wfe
        EXPORTING
          textid = zcx_wfe=>error_save_changelog.
    ENDIF.

  ENDMETHOD.


  METHOD write_header_data.

    " Tiene que haber un ID de workflow para poder grabar.
    IF ms_xheader-wf_id IS NOT INITIAL.

      " El indicador de actualización a nivel de campo es simple. Si hay registro previo es una actualización,
      " en caso de existir es una actuaización.
      DATA(lv_change_ind) = COND #( WHEN ms_yheader IS INITIAL THEN zif_wfe_data=>cs_model_data-changelog-update_ind_pos-insert
                                    ELSE zif_wfe_data=>cs_model_data-changelog-update_ind_pos-update ).

      " En la cabecera solo comparo los campos donde habra datos.
      DATA(lt_components) = zcl_wfe_utilities=>get_struct_components_recus( is_structure = zif_wfe_data=>cs_model_data-header-structure-fields ).
      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_components>).
        " Los componentes deben existir porque todos usan las mismas estructuras de diccionario. Si alguien mete mano y la liea, que pegue un buen
        " dump.
        ASSIGN COMPONENT <ls_components>-name OF STRUCTURE ms_xheader TO FIELD-SYMBOL(<xvalue>).
        ASSIGN COMPONENT <ls_components>-name OF STRUCTURE ms_yheader TO FIELD-SYMBOL(<yvalue>).

        " Solo si hay diferencia se insertarán los valores
        IF <xvalue> NE <yvalue>.
          INSERT VALUE #( source = zif_wfe_data=>cs_model_data-changelog-source-header
                          field  = <ls_components>-name
                          chngind = lv_change_ind
                          value_new = <xvalue>
                          value_old = <yvalue> ) INTO TABLE mt_changelog_data.
        ENDIF.
      ENDLOOP.


      DATA(ls_db) = CORRESPONDING zwfe_t007( ms_xheader ).

      " Si no hay datos previos es que el registro se esta creando. Por lo que se informa la fecha de creación.
      " Si existe los registros provendrán de la tabla con los valores antiguos
      IF ms_yheader IS INITIAL.
        ls_db-erdat = sy-datum.
        ls_db-ernam = sy-uname.
        ls_db-erzet = sy-uzeit.
        ls_db-ernam_process = ms_options-process_user.
      ELSE.
        ls_db-erdat = ms_yheader-erdat.
        ls_db-ernam = ms_yheader-ernam.
        ls_db-erzet = ms_yheader-erzet.
        ls_db-ernam_process = ms_yheader-ernam_process.
      ENDIF.

      " Campos de datos de actualización
      ls_db-aedat = sy-datum.
      ls_db-aenam = sy-uname.
      ls_db-aenam_process = ms_options-process_user.
      ls_db-aetim = sy-uzeit.


      MODIFY zwfe_t007 FROM ls_db.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_wfe
          EXPORTING
            textid = zcx_wfe=>error_save_header_data.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD write_steps_data.
    DATA lt_db TYPE STANDARD TABLE OF zwfe_t011.

    IF mt_xsteps IS NOT INITIAL.

      " Recorro la tabla para rellenar algunos valores según su valor anterior.
      LOOP AT mt_xsteps ASSIGNING FIELD-SYMBOL(<ls_xsteps>).
        DATA(ls_db) = CORRESPONDING zwfe_t011( <ls_xsteps> ).

        READ TABLE mt_ysteps ASSIGNING FIELD-SYMBOL(<ls_ysteps>) WITH KEY status = <ls_xsteps>-status.
        IF sy-subrc NE 0. " Si no existe informo la datos de creación
          ls_db-erdat = sy-datum.
          ls_db-ernam = sy-uname.
          ls_db-erzet = sy-uzeit.
          ls_db-ernam_process = ms_options-process_user.
        ELSE. " Si existe los datos de creación es del registro encontrado
          ls_db-erdat = <ls_ysteps>-erdat.
          ls_db-ernam = <ls_ysteps>-ernam.
          ls_db-erzet = <ls_ysteps>-erzet.
          ls_db-ernam_process = <ls_ysteps>-ernam_process.
        ENDIF.

        " Campos de datos de actualización
        ls_db-aedat = sy-datum.
        ls_db-aenam = sy-uname.
        ls_db-aenam_process = ms_options-process_user.
        ls_db-aetim = sy-uzeit.

        INSERT ls_db INTO TABLE lt_db.
      ENDLOOP.

      MODIFY zwfe_t011 FROM TABLE lt_db.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_wfe
          EXPORTING
            textid = zcx_wfe=>error_save_steps_data.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD write_steps_user_data.
    DATA lt_db TYPE STANDARD TABLE OF zwfe_t012.

    IF mt_xsteps_approvers IS NOT INITIAL.

      lt_db = CORRESPONDING #( mt_xsteps_approvers ).
      MODIFY zwfe_t012 FROM TABLE lt_db.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_wfe
          EXPORTING
            textid = zcx_wfe=>error_save_steps_user_data.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD write_to_db.


    TRY.
        " Se escribe los datos de cabecera
        write_header_data( ).

        " Se escribe los datos de posición
        write_values_data( ).

        " Se escriben los datos de los pasos
        write_steps_data( ).

        " Se escriben los datos de los usuarios del pasos
        write_steps_user_data( ).

        " Se escribe el log de modificación
        write_changelog_data( ).

        " Se hace commit si así se ha indicado
        IF ms_options-commit = abap_true.
          COMMIT WORK AND WAIT.
        ENDIF.

      CATCH zcx_wfe INTO DATA(lo_wfe).

        " Se hace rollback si así se ha indicado
        IF ms_options-commit = abap_true.
          ROLLBACK WORK.
        ENDIF.

        INSERT VALUE #( type = zif_wfe_data=>cs_message-type-error
                        message = zcl_wfe_utilities=>fill_return( iv_id         = lo_wfe->if_t100_message~t100key-msgid
                                                                  iv_number     = lo_wfe->if_t100_message~t100key-msgno
                                                                  iv_message_v1 = lo_wfe->mv_msgv1
                                                                  iv_message_v2 = lo_wfe->mv_msgv2
                                                                  iv_message_v3 = lo_wfe->mv_msgv3
                                                                  iv_message_v4 = lo_wfe->mv_msgv4
                                                                  iv_langu      = ms_options-langu )-message ) INTO TABLE et_return.

    ENDTRY.


  ENDMETHOD.


  METHOD write_values_data.
    DATA lt_db TYPE STANDARD TABLE OF zwfe_t008.

    " Datos de los valores. Si esta informado.

    IF mt_xvalues IS NOT INITIAL OR mt_yvalues IS NOT INITIAL.

      " Los datos de modificación aquí es simple porque es simplemente una relación de clave->valor
      LOOP AT mt_xvalues ASSIGNING FIELD-SYMBOL(<ls_xpositions>).

        READ TABLE mt_yvalues ASSIGNING FIELD-SYMBOL(<ls_ypositions>) WITH KEY counter = <ls_xpositions>-counter
                                                                               field = <ls_xpositions>-field.
        IF sy-subrc = 0.
          " Si el valor cambia es cuando se actualiza.
          IF <ls_xpositions>-value NE <ls_ypositions>-value.
            INSERT VALUE #( source = zif_wfe_data=>cs_model_data-changelog-source-values
                          field  = <ls_xpositions>-field
                          chngind = zif_wfe_data=>cs_model_data-changelog-update_ind_pos-update
                          counter = <ls_xpositions>-counter
                          value_new = <ls_xpositions>-value
                          value_old = <ls_ypositions>-value
                          value_extended_new = <ls_xpositions>-value_extended
                          " No se guarda ya que se va usar para guardar archivos. Y no interesa.
                          "value_extended_old = <ls_ypositions>-value_extended
                           ) INTO TABLE mt_changelog_data.
          ENDIF.
        ELSE. " El campo no existe en el valor anterior
          INSERT VALUE #( source = zif_wfe_data=>cs_model_data-changelog-source-values
                          field  = <ls_xpositions>-field
                          counter = <ls_xpositions>-counter
                          chngind = zif_wfe_data=>cs_model_data-changelog-update_ind_pos-insert
                          value_new = <ls_xpositions>-value
                          " value_extended_new = <ls_xpositions>-value_extended
                          ) INTO TABLE mt_changelog_data.
        ENDIF.

      ENDLOOP.

      " Ahora se buscan los campos que estan en los valores anteriores pero no en los actuales
      LOOP AT mt_yvalues ASSIGNING <ls_ypositions>.
        READ TABLE mt_xvalues TRANSPORTING NO FIELDS WITH KEY counter = <ls_ypositions>-counter
                                                              field = <ls_ypositions>-field.
        IF sy-subrc NE 0.
          INSERT VALUE #( source = zif_wfe_data=>cs_model_data-changelog-source-values
                         chngind = zif_wfe_data=>cs_model_data-changelog-update_ind_pos-delete
                         counter = <ls_ypositions>-counter
                         field  = <ls_ypositions>-field
                         value_old = <ls_ypositions>-value
                         " value_extended_old = <ls_ypositions>-value_extended
                         ) INTO TABLE mt_changelog_data.
        ENDIF.
      ENDLOOP.

      " Saco el ID del workflo de los datos a actualizar o bien de los datos anteriores. Ya que contemplo la posibilidad
      " que se borren valores, aunque a día de hoy no esta contemplada.
      IF mt_xvalues IS NOT INITIAL.
        DATA(lv_wf_id) = mt_xvalues[ 1 ]-wf_id.
      ELSE.
        lv_wf_id = mt_yvalues[ 1 ]-wf_id.
      ENDIF.

      " Borro los valores del workflow, por si hay campos que desaparecen.
      DELETE FROM zwfe_t008 WHERE wf_id = lv_wf_id.

      lt_db = CORRESPONDING #( mt_xvalues ).

      MODIFY zwfe_t008 FROM TABLE lt_db.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_wfe
          EXPORTING
            textid = zcx_wfe=>error_save_values_data.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
