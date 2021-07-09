CLASS zcl_wfe_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized">Fill bapiret2</p>
    CLASS-METHODS fill_return
      IMPORTING
        !iv_type         TYPE any OPTIONAL
        !iv_id           TYPE any OPTIONAL
        !iv_number       TYPE any
        !iv_message_v1   TYPE any OPTIONAL
        !iv_message_v2   TYPE any OPTIONAL
        !iv_message_v3   TYPE any OPTIONAL
        !iv_message_v4   TYPE any OPTIONAL
        !iv_langu        TYPE sylangu DEFAULT sy-langu
        !iv_row          TYPE any OPTIONAL
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    "! <p class="shorttext synchronized">Get components of structure(recursive method)</p>
    CLASS-METHODS get_struct_components_recus
      IMPORTING
        !is_structure        TYPE any
      RETURNING
        VALUE(rt_components) TYPE cl_abap_structdescr=>component_table .
    "! <p class="shorttext synchronized">Convert params_sl to ranges</p>
    CLASS-METHODS conv_params_2_ranges
      IMPORTING
        !iv_name             TYPE any
        !it_params_sl        TYPE zif_wfe_data=>tt_params_sl
        !iv_apply_conversion TYPE abap_bool DEFAULT abap_false
      EXPORTING
        !et_r_ranges         TYPE STANDARD TABLE .
  PROTECTED SECTION.

    "! <p class="shorttext synchronized">Determine how get get the componentes</p>
    "! Útil cuando la estructura tiene includes
    "! @parameter is_component | <p class="shorttext synchronized">Tipo de objeto del componente</p>
    "! @parameter ct_components | <p class="shorttext synchronized">Componentes</p>
    CLASS-METHODS structure_comp_case
      IMPORTING
        !is_component  TYPE abap_componentdescr
      CHANGING
        !ct_components TYPE abap_component_tab .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wfe_utilities IMPLEMENTATION.


  METHOD conv_params_2_ranges.
    FIELD-SYMBOLS <ls_params> TYPE LINE OF zif_wfe_data=>tt_params_sl.
    FIELD-SYMBOLS <campo> TYPE any.
    FIELD-SYMBOLS <ls_ranges> TYPE any.
    DATA: ls_ranges TYPE REF TO data.

*   Crear una Work area del mismo tipo que una linea del ranges pasado por parámetro
    CREATE DATA ls_ranges LIKE LINE OF et_r_ranges.
    ASSIGN ls_ranges->* TO <ls_ranges>.

*   buscamos la mascara de edición adecuada:
    IF iv_apply_conversion EQ abap_true.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_ranges> TO <campo>.
      IF sy-subrc EQ 0.
        DATA(lo_elem_descr) = CAST cl_abap_elemdescr( cl_abap_datadescr=>describe_by_data( <campo> ) ).
        IF lo_elem_descr->edit_mask IS NOT INITIAL.
          DATA(lv_fm_name) = |CONVERSION_EXIT_{ lo_elem_descr->edit_mask+2 }_INPUT|.
        ENDIF.
      ENDIF.
      UNASSIGN <campo>.
    ENDIF.

    CLEAR et_r_ranges.
    LOOP AT it_params_sl ASSIGNING <ls_params> WHERE selname = iv_name
                                                  AND ( low IS NOT INITIAL OR
                                                        option IS NOT INITIAL ).

      CASE <ls_params>-kind.
        WHEN 'S'.

          IF <ls_params>-sign IS NOT INITIAL AND <ls_params>-option IS NOT INITIAL.
*   Asignamos campo Low. Si esta en blanco, no proceso el registro.
            ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_ranges> TO <campo>.
            IF sy-subrc = 0.

              IF iv_apply_conversion EQ abap_true AND lv_fm_name IS NOT INITIAL.
                CALL FUNCTION lv_fm_name
                  EXPORTING
                    input  = <ls_params>-low
                  IMPORTING
                    output = <campo>
                  EXCEPTIONS
                    OTHERS = 1.
              ELSE.
                <campo> = <ls_params>-low.
              ENDIF.

*   Asignamos y llenamos campo Sign
              ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_ranges> TO <campo>.
              IF sy-subrc = 0.
                <campo> = <ls_params>-sign.
              ENDIF.

*   Asignamos y llenamos campo Option
              ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_ranges> TO <campo>.
              IF sy-subrc = 0.
                <campo> = <ls_params>-option.
              ENDIF.

*   Asignamos campo high
              ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_ranges> TO <campo>.
              IF sy-subrc = 0.
                IF <ls_params>-high IS NOT INITIAL.
                  IF iv_apply_conversion EQ abap_true AND lv_fm_name IS NOT INITIAL.
                    CALL FUNCTION lv_fm_name
                      EXPORTING
                        input  = <ls_params>-low
                      IMPORTING
                        output = <campo>
                      EXCEPTIONS
                        OTHERS = 1.
                  ELSE.
                    <campo> = <ls_params>-high.
                  ENDIF.
                ENDIF.
              ENDIF.

              APPEND <ls_ranges> TO et_r_ranges.

            ENDIF.

          ENDIF.
        WHEN 'P'.
*   Asignamos y llenamos campo Sign
          ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_ranges> TO <campo>.
          IF sy-subrc = 0.
            <campo> = 'I'.
          ENDIF.
*   Asignamos y llenamos campo Option
          ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_ranges> TO <campo>.
          IF sy-subrc = 0.
            <campo> = 'EQ'.
          ENDIF.
*   Asignamos y llenamos campo Option
          ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_ranges> TO <campo>.
          IF sy-subrc = 0.
            IF iv_apply_conversion EQ abap_true AND lv_fm_name IS NOT INITIAL.
              CALL FUNCTION lv_fm_name
                EXPORTING
                  input  = <ls_params>-low
                IMPORTING
                  output = <campo>
                EXCEPTIONS
                  OTHERS = 1.
            ELSE.
              <campo> = <ls_params>-low.
            ENDIF.
          ENDIF.
          APPEND <ls_ranges> TO et_r_ranges.
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.


  METHOD fill_return.
    CLEAR rs_return.

    rs_return-type = COND #( WHEN iv_type IS INITIAL THEN zif_wfe_data=>cs_message-type-sucess ELSE iv_type ).
    rs_return-id = COND #( WHEN iv_id IS INITIAL THEN zif_wfe_data=>cs_message-id ELSE iv_id ).
    rs_return-number = iv_number.
    rs_return-message_v1 = iv_message_v1.
    rs_return-message_v2 = iv_message_v2.
    rs_return-message_v3 = iv_message_v3.
    rs_return-message_v4 = iv_message_v4.
    rs_return-row = iv_row.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = rs_return-id
        number     = rs_return-number
        language   = iv_langu
        textformat = 'ASC'
        message_v1 = rs_return-message_v1
        message_v2 = rs_return-message_v2
        message_v3 = rs_return-message_v3
        message_v4 = rs_return-message_v4
      IMPORTING
        message    = rs_return-message.
  ENDMETHOD.


  METHOD get_struct_components_recus.
    CLEAR rt_components.

    " Obtención de los componentes. Primero probamos por el valor y luego por nombre
    TRY.
        DATA(lo_struct) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_structure ) ).
      CATCH cx_root.
        lo_struct = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( is_structure ) ).
    ENDTRY.

    IF lo_struct IS BOUND.
      TRY.

          DATA(lt_components) = lo_struct->get_components(  ).

          LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_components>) WHERE name IS NOT INITIAL.
            structure_comp_case( EXPORTING is_component = <ls_components>
                                 CHANGING ct_components = rt_components ).
          ENDLOOP.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD structure_comp_case.
    CASE is_component-type->kind.
      WHEN cl_abap_typedescr=>kind_elem. "E Elementary Type
        INSERT is_component INTO TABLE ct_components.
      WHEN cl_abap_typedescr=>kind_table. "T Table
        INSERT is_component INTO TABLE ct_components.
      WHEN cl_abap_typedescr=>kind_struct. "S Structure
        DATA(lv_name) = is_component-type->get_relative_name( ).
        DATA(lt_comp_str) = get_struct_components_recus( is_structure = lv_name ).
        INSERT LINES OF lt_comp_str INTO TABLE ct_components.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
