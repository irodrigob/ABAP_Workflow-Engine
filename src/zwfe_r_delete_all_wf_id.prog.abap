*&---------------------------------------------------------------------*
*& Report  ZWFE_R_DELETE_ALL_WF_ID
*&
*&---------------------------------------------------------------------*
*& Description: Programa que borrar todos los datos de un workflow
*& a partir de workflow ID. Este programa solo se usa para borrar
*& los workflows generados en pruebas.
*& USAR BAJA LA RESPONSABILIDAD DE CADA UNO
*&---------------------------------------------------------------------*
REPORT zwfe_r_delete_all_wf_id.

TABLES zwfe_t007.

SELECT-OPTIONS: s_wf_id FOR zwfe_t007-wf_id.

PARAMETERS p_test AS CHECKBOX DEFAULT abap_true.

SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN COMMENT /1(50) text-c01.

START-OF-SELECTION.

  IF p_test = abap_false.

    DATA(lo_crud) = NEW zcl_wfe_model_data_crud( ).
    LOOP AT s_wf_id ASSIGNING FIELD-SYMBOL(<ls_wf_id>).

      lo_crud->dispatch_data(
        EXPORTING
          is_options       = VALUE #( commit = abap_true )
          iv_updkz         = zif_wfe_data=>cs_model_data-update_ind-full_delete_wf
         is_header        = VALUE #( wf_id = <ls_wf_id>-low )
        IMPORTING
          et_return        = DATA(lt_return) ).

      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) INDEX 1.
      IF sy-subrc = 0.
        WRITE:/ text-t02, <ls_wf_id>-low, ' : ', <ls_return>-message.
      ENDIF.
    ENDLOOP.

  ELSE.
    WRITE:/ text-t01.
  ENDIF.
