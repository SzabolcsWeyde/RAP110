CLASS zrap110_calc_trav_elem_022 DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.

    CLASS-METHODS calculate_trav_status_ind
      IMPORTING
        is_original_data TYPE  zrap110_c_traveltp_022
      RETURNING
        VALUE(result)    TYPE zrap110_c_traveltp_022.
ENDCLASS.


CLASS zrap110_calc_trav_elem_022 IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA lt_travel_original_data TYPE STANDARD TABLE OF zrap110_c_traveltp_022 WITH DEFAULT KEY.

    IF it_requested_calc_elements IS INITIAL.
      EXIT.
    ENDIF.
    LOOP AT it_requested_calc_elements ASSIGNING FIELD-SYMBOL(<fs_requested_calc_element>).
      CASE <fs_requested_calc_element>.
        WHEN 'OVERALLSTATUSINDICATOR'.
          lt_travel_original_data = CORRESPONDING #( it_original_data ).
          LOOP AT lt_travel_original_data ASSIGNING FIELD-SYMBOL(<fs_trav_original_data>).
            <fs_trav_original_data> = zrap110_calc_trav_elem_022=>calculate_trav_status_ind( <fs_trav_original_data> ).
          ENDLOOP.
          ct_calculated_data = CORRESPONDING #( lt_travel_original_data ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    IF iv_entity = 'ZRAP110_C_TRAVELTP_022'. " Travel BO node
      LOOP AT it_requested_calc_elements ASSIGNING FIELD-SYMBOL(<fs_travel_calc_element>).
        CASE <fs_travel_calc_element>.
          WHEN 'OVERALLSTATUSINDICATOR'.
            APPEND 'OVERALLSTATUS' TO et_requested_orig_elements.

        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD calculate_trav_status_ind.
    result = CORRESPONDING #( is_original_data ).

    CASE result-OverallStatus.
      WHEN 'X'. result-OverallStatusIndicator = 1.
      WHEN 'O'. result-OverallStatusIndicator = 2.
      WHEN 'A'. result-OverallStatusIndicator = 3.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


ENDCLASS.
