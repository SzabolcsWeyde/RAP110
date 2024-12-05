CLASS lsc_zrap110_r_traveltp_022 DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.
    METHODS adjust_numbers REDEFINITION.

ENDCLASS.


CLASS lsc_zrap110_r_traveltp_022 IMPLEMENTATION.


  METHOD adjust_numbers.
    DATA travel_id_max TYPE /dmo/travel_id.

    IF mapped-travel IS NOT INITIAL.
      TRY.
          cl_numberrange_runtime=>number_get( EXPORTING nr_range_nr       = '01'
                                                        object            = 'ZRAP110022'
                                                        quantity          = CONV #( lines( mapped-travel ) )
                                              IMPORTING number            = DATA(number_range_key)
                                                        returncode        = DATA(number_range_return_code)
                                                        returned_quantity = DATA(number_range_returned_quantity) ).

        CATCH cx_number_ranges INTO DATA(lx_number_ranges).
          RAISE SHORTDUMP NEW cx_number_ranges( previous = lx_number_ranges ).
      ENDTRY.

      ASSERT number_range_returned_quantity = lines( mapped-travel ).
      travel_id_max = number_range_key - number_range_returned_quantity.
      LOOP AT mapped-travel ASSIGNING FIELD-SYMBOL(<travel>).
        travel_id_max += 1.
        <travel>-TravelID = travel_id_max.
      ENDLOOP.
    ENDIF.
    " Child BO entity: Booking
    IF mapped-booking IS NOT INITIAL.
      READ ENTITIES OF ZRAP110_R_TravelTP_022 IN LOCAL MODE
           ENTITY Booking BY \_Travel
           FROM VALUE #( FOR booking IN mapped-booking WHERE ( %tmp-TravelID IS INITIAL )
                         ( %pid = booking-%pid
                           %key = booking-%tmp ) )
           LINK DATA(booking_to_travel_links).

      LOOP AT mapped-booking ASSIGNING FIELD-SYMBOL(<booking>).
        <booking>-TravelID =
          COND #( WHEN <booking>-%tmp-TravelID IS INITIAL
                  THEN mapped-travel[
                           %pid = booking_to_travel_links[ source-%pid = <booking>-%pid ]-target-%pid ]-TravelID
                  ELSE <booking>-%tmp-TravelID ).
      ENDLOOP.

      LOOP AT mapped-booking INTO DATA(mapped_booking) GROUP BY mapped_booking-TravelID.
        SELECT MAX( booking_id ) FROM zrap110_abook022
          WHERE travel_id = @mapped_booking-TravelID
          INTO @DATA(max_booking_id).
        LOOP AT GROUP mapped_booking ASSIGNING <booking>.
          max_booking_id += 10.
          <booking>-BookingID = max_booking_id.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


ENDCLASS.


CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF travel_status,
        open     TYPE c LENGTH 1 VALUE 'O',
        accepted TYPE c LENGTH 1 VALUE 'A',
        rejected TYPE c LENGTH 1 VALUE 'X',
      END OF travel_status.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING
      REQUEST requested_authorizations FOR Travel
      RESULT result.
    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCustomer.

    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.

    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel RESULT result.

    METHODS createTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~createTravel.

    METHODS recalTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Travel~recalTotalPrice.

    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel RESULT result.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTotalPrice.

    METHODS setInitialTravelValues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setInitialTravelValues.

ENDCLASS.


CLASS lhc_travel IMPLEMENTATION.


  METHOD get_global_authorizations.
  ENDMETHOD.


  METHOD validateAgency.

    DATA agencies TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.

    READ ENTITIES OF zrap110_r_traveltp_022 IN LOCAL MODE
         ENTITY travel
         FIELDS ( AgencyID )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    agencies = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING agency_id = AgencyID EXCEPT * ).
    DELETE agencies WHERE agency_id IS INITIAL.

    IF agencies IS NOT INITIAL.

      SELECT FROM /dmo/agency
        FIELDS ( agency_id )
        FOR ALL ENTRIES IN @agencies
        WHERE agency_id = @agencies-agency_id
        INTO TABLE @DATA(valid_agencies).

    ENDIF.

    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #( %tky        = travel-%tky
                      %state_area = 'VALIDATE_AGENCY' ) TO reported-travel.

      IF travel-AgencyID IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #(
            %tky              = travel-%tky
            %state_area       = 'VALIDATE_AGENCY'
            %msg              = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>enter_agency_id
                                                             severity = if_abap_behv_message=>severity-error )
            %element-AgencyID = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF NOT line_exists( valid_agencies[ agency_id = travel-AgencyID ] ).

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #(
            %tky              = travel-%tky
            %state_area       = 'VALIDATE_AGENCY'
            %msg              = NEW /dmo/cm_flight_messages( agency_id = travel-AgencyID
                                                             textid    = /dmo/cm_flight_messages=>agency_unkown
                                                             severity  = if_abap_behv_message=>severity-error )
            %element-AgencyID = if_abap_behv=>mk-on ) TO reported-travel.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD validateCustomer.

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    READ ENTITIES OF zrap110_r_traveltp_022 IN LOCAL MODE
         ENTITY Travel
         FIELDS ( CustomerID )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.

    IF customers IS NOT INITIAL.

      SELECT FROM /dmo/customer
        FIELDS customer_id
        FOR ALL ENTRIES IN @customers
        WHERE customer_id = @customers-customer_id
        INTO TABLE @DATA(valid_customers).

    ENDIF.

    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #( %tky        = travel-%tky
                      %state_area = 'VALIDATE_CUSTOMER'  ) TO reported-travel.

      DATA resp_textid TYPE scx_t100key.

      IF travel-CustomerID IS INITIAL.

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #(
            %tky                = travel-%tky
            %state_area         = 'VALIDATE_CUSTOMER'
            %msg                = NEW /dmo/cm_flight_messages( textid   = /dmo/cm_flight_messages=>enter_customer_id
                                                               severity = if_abap_behv_message=>severity-error )
            %element-CustomerID = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF NOT line_exists( valid_customers[ customer_id = travel-CustomerID ] ).

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #(
            %tky                = travel-%tky
            %state_area         = 'VALIDATE_CUSTOMER'
            %msg                = NEW /dmo/cm_flight_messages( customer_id = travel-CustomerID
                                                               textid      = /dmo/cm_flight_messages=>customer_unkown
                                                               severity    = if_abap_behv_message=>severity-error )
            %element-CustomerID = if_abap_behv=>mk-on ) TO reported-travel.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD validateDates.

    READ ENTITIES OF zrap110_r_traveltp_022 IN LOCAL MODE
         ENTITY Travel FIELDS ( BeginDate EndDate )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).
      APPEND VALUE #( %tky        = travel-%tky
                      %state_area = 'VALLIDATE_DATES' ) TO reported-travel.

      IF travel-BeginDate > travel-EndDate.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW /dmo/cm_flight_messages(
                                                     textid     = /dmo/cm_flight_messages=>begin_date_bef_end_date
                                                     severity   = if_abap_behv_message=>severity-error
                                                     begin_date = travel-BeginDate
                                                     end_date   = travel-EndDate
                                                     travel_id  = travel-TravelID )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ELSEIF travel-BeginDate < cl_abap_context_info=>get_system_date( ).

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW /dmo/cm_flight_messages(
                                                     textid     = /dmo/cm_flight_messages=>begin_date_on_or_bef_sysdate
                                                     severity   = if_abap_behv_message=>severity-error
                                                     begin_date = travel-BeginDate )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance_features.
  ENDMETHOD.


  METHOD acceptTravel.

    MODIFY ENTITIES OF Zrap110_r_traveltp_022 IN LOCAL MODE
           ENTITY travel
           UPDATE FIELDS ( OverallStatus ) WITH VALUE #( FOR key IN keys
                                                         ( %tky          = key-%tky
                                                           OverallStatus = travel_status-accepted ) ).

    READ ENTITIES OF zrap110_r_traveltp_022 IN LOCAL MODE
         ENTITY travel
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels
                      ( %tky   = travel-%tky
                        %param = travel ) ).

  ENDMETHOD.


  METHOD createTravel.

    IF keys IS NOT INITIAL.
      SELECT * FROM /dmo/flight
        FOR ALL ENTRIES IN @keys
        WHERE carrier_id    = @keys-%param-carrier_id
          AND connection_id = @keys-%param-connection_id
          AND flight_date   = @keys-%param-flight_date
        INTO TABLE @DATA(flights).

      MODIFY ENTITIES OF zrap110_r_traveltp_022 IN LOCAL MODE
             ENTITY travel
             CREATE
             FIELDS ( CustomerID Description )
             WITH VALUE #( FOR key IN keys
                           ( %cid        = key-%cid
                             %is_draft   = key-%param-%is_draft
                             CustomerID  = key-%param-customer_id
                             Description = 'Placeholder description for own create action' ) )
             CREATE BY \_Booking
             FIELDS ( CustomerID CarrierID ConnectionID FlightDate FlightPrice CurrencyCode )
             WITH VALUE #(
                 FOR key IN keys INDEX INTO i
                 ( %cid_ref  = key-%cid
                   %is_draft = key-%param-%is_draft
                   %target   = VALUE #(
                       ( %cid         = i
                         %is_draft    = key-%param-%is_draft
                         CustomerID   = key-%param-customer_id
                         CarrierID    = key-%param-carrier_id
                         ConnectionID = key-%param-connection_id
                         FlightDate   = key-%param-flight_date
                         FlightPrice  = VALUE #( flights[ carrier_id    = key-%param-carrier_id
                                                          connection_id = key-%param-connection_id
                                                          flight_date   = key-%param-flight_date ]-price OPTIONAL )
                         CurrencyCode = VALUE #( flights[ carrier_id    = key-%param-carrier_id
                                                          connection_id = key-%param-connection_id
                                                          flight_date   = key-%param-flight_date ]-currency_code OPTIONAL ) ) ) ) )
             MAPPED mapped.
    ENDIF.

  ENDMETHOD.


  METHOD recalTotalPrice.

    TYPES:
      BEGIN OF ty_amount_per_currencycode,
        amount       TYPE /dmo/total_price,
        currencycode TYPE /dmo/currency_code,
      END OF ty_amount_per_currencycode.

    DATA amounts_per_currencies TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    READ ENTITIES OF zrap110_r_traveltp_022 IN LOCAL MODE
         ENTITY travel
         FIELDS ( BookingFee CurrencyCode )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    DELETE travels WHERE CurrencyCode IS INITIAL.

    READ ENTITIES OF zrap110_r_traveltp_022
         ENTITY travel BY \_Booking
         FIELDS ( FlightPrice CurrencyCode )
         WITH CORRESPONDING #( travels )
         RESULT DATA(bookings)
         LINK DATA(booking_links).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      amounts_per_currencies = VALUE #( ( Amount       = <travel>-BookingFee
                                          CurrencyCode = <travel>-CurrencyCode ) ).

      LOOP AT booking_links INTO DATA(booking_link) USING KEY id WHERE source-%tky = <travel>-%tky.

        DATA(booking) = bookings[ %tky = booking_link-target-%tky ].

        COLLECT VALUE ty_amount_per_currencycode( Amount       = booking-FlightPrice
                                                  CurrencyCode = booking-CurrencyCode ) INTO amounts_per_currencies.

      ENDLOOP.

      DELETE amounts_per_currencies WHERE currencycode IS INITIAL.

      CLEAR <travel>-TotalPrice.
      LOOP AT amounts_per_currencies INTO DATA(amount_per_currencycode).
        " If needed do a Currency Conversion
        IF amount_per_currencycode-CurrencyCode = <travel>-CurrencyCode.
          <travel>-TotalPrice += amount_per_currencycode-Amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
            EXPORTING iv_amount               = amount_per_currencycode-Amount
                      iv_currency_code_source = amount_per_currencycode-CurrencyCode
                      iv_currency_code_target = <travel>-CurrencyCode
                      iv_exchange_rate_date   = cl_abap_context_info=>get_system_date( )
            IMPORTING ev_amount               = DATA(total_booking_price_per_curr) ).
          <travel>-TotalPrice += total_booking_price_per_curr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    MODIFY ENTITIES OF ZRAP110_R_TravelTP_022 IN LOCAL MODE
           ENTITY travel
           UPDATE FIELDS ( TotalPrice )
           WITH CORRESPONDING #( travels ).

  ENDMETHOD.


  METHOD rejectTravel.
    MODIFY ENTITIES OF Zrap110_r_traveltp_022 IN LOCAL MODE
           ENTITY travel
           UPDATE FIELDS ( OverallStatus ) WITH VALUE #( FOR key IN keys
                                                         ( %tky          = key-%tky
                                                           OverallStatus = travel_status-rejected ) ).

    READ ENTITIES OF zrap110_r_traveltp_022 IN LOCAL MODE
         ENTITY travel
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels
                      ( %tky   = travel-%tky
                        %param = travel ) ).
  ENDMETHOD.


  METHOD calculateTotalPrice.

    MODIFY ENTITIES OF zrap110_r_traveltp_022 IN LOCAL MODE
           ENTITY travel
           EXECUTE recalTotalPrice
           FROM CORRESPONDING #( keys ).

  ENDMETHOD.


  METHOD setInitialTravelValues.

    DATA update TYPE TABLE FOR UPDATE zrap110_r_traveltp_022\\travel.

    READ ENTITIES OF zrap110_r_traveltp_022
         ENTITY travel
         FIELDS ( BeginDate EndDate CurrencyCode OverallStatus )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).

    update = CORRESPONDING #( travels ).

    DELETE update WHERE     BeginDate     IS NOT INITIAL
                        AND EndDate       IS NOT INITIAL
                        AND CurrencyCode  IS NOT INITIAL
                        AND OverallStatus IS NOT INITIAL.

    LOOP AT update ASSIGNING FIELD-SYMBOL(<update>).

      IF <update>-BeginDate IS INITIAL.
        <update>-BeginDate = cl_abap_context_info=>get_system_date( ) + 1.
        <update>-%control-BeginDate = if_abap_behv=>mk-on.
      ENDIF.
      IF <update>-EndDate IS INITIAL.
        <update>-EndDate = cl_abap_context_info=>get_system_date( ) + 15.
        <update>-%control-EndDate = if_abap_behv=>mk-on.
      ENDIF.
      IF <update>-CurrencyCode IS INITIAL.
        <update>-CurrencyCode = 'EUR'.
        <update>-%control-CurrencyCode = if_abap_behv=>mk-on.
      ENDIF.
      IF <update>-OverallStatus IS INITIAL.
        <update>-OverallStatus = travel_status-open.
        <update>-%control-OverallStatus = if_abap_behv=>mk-on.
      ENDIF.

    ENDLOOP.

    IF update IS NOT INITIAL.
      MODIFY ENTITIES OF zrap110_r_traveltp_022 IN LOCAL MODE
             ENTITY travel
             UPDATE FROM update.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
