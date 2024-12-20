@AccessControl.authorizationCheck: #CHECK

@EndUserText.label: 'Projection View forBooking'

@Metadata.allowExtensions: true

@ObjectModel.semanticKey: [ 'BookingID' ]

@Search.searchable: true

define view entity ZRAP110_C_BOOKINGTP_022
  as projection on ZRAP110_R_BOOKINGTP_022 as Booking

{
          @Search.defaultSearchElement: true
          @Search.fuzzinessThreshold: 0.90
  key     TravelID,

          @Search.defaultSearchElement: true
          @Search.fuzzinessThreshold: 0.90
  key     BookingID,

          BookingDate,

          @Consumption.valueHelpDefinition: [ { entity: { name: '/DMO/I_Customer_StdVH', element: 'CustomerID' } } ]
          CustomerID,

          @Consumption.valueHelpDefinition: [ { entity: { name: '/DMO/I_Flight', element: 'AirlineID' },
                                                additionalBinding: [ { localElement: 'FlightDate',
                                                                       element: 'FlightDate',
                                                                       usage: #RESULT },
                                                                     { localElement: 'ConnectionID',
                                                                       element: 'ConnectionID',
                                                                       usage: #RESULT },
                                                                     { localElement: 'FlightPrice',
                                                                       element: 'Price',
                                                                       usage: #RESULT },
                                                                     { localElement: 'CurrencyCode',
                                                                       element: 'CurrencyCode',
                                                                       usage: #RESULT } ] } ]
          CarrierID,

          @Consumption.valueHelpDefinition: [ { entity: { name: '/DMO/I_Flight', element: 'ConnectionID' },
                                                additionalBinding: [ { localElement: 'FlightDate',
                                                                       element: 'FlightDate',
                                                                       usage: #RESULT },
                                                                     { localElement: 'CarrierID',
                                                                       element: 'AirlineID',
                                                                       usage: #FILTER_AND_RESULT },
                                                                     { localElement: 'FlightPrice',
                                                                       element: 'Price',
                                                                       usage: #RESULT },
                                                                     { localElement: 'CurrencyCode',
                                                                       element: 'CurrencyCode',
                                                                       usage: #RESULT } ] } ]
          ConnectionID,

          @Consumption.valueHelpDefinition: [ { entity: { name: '/DMO/I_Flight', element: 'FlightDate' },
                                                additionalBinding: [ { localElement: 'CarrierID',
                                                                       element: 'AirlineID',
                                                                       usage: #FILTER_AND_RESULT },
                                                                     { localElement: 'ConnectionID',
                                                                       element: 'ConnectionID',
                                                                       usage: #FILTER_AND_RESULT },
                                                                     { localElement: 'FlightPrice',
                                                                       element: 'Price',
                                                                       usage: #RESULT },
                                                                     { localElement: 'CurrencyCode',
                                                                       element: 'CurrencyCode',
                                                                       usage: #RESULT } ] } ]
          FlightDate,

          @Consumption.valueHelpDefinition: [ { entity: { name: '/DMO/I_Booking_Status_VH', element: 'BookingStatus' } } ]
          BookingStatus,

          @Consumption.valueHelpDefinition: [ { entity: { name: '/DMO/I_Flight', element: 'Price' },
                                                additionalBinding: [ { localElement: 'CarrierID',
                                                                       element: 'AirlineID',
                                                                       usage: #FILTER_AND_RESULT },
                                                                     { localElement: 'ConnectionID',
                                                                       element: 'ConnectionID',
                                                                       usage: #FILTER_AND_RESULT },
                                                                     { localElement: 'FlightDate',
                                                                       element: 'FlightDate',
                                                                       usage: #FILTER_AND_RESULT },
                                                                     { localElement: 'CurrencyCode',
                                                                       element: 'CurrencyCode',
                                                                       usage: #RESULT } ] } ]
          @Semantics.amount.currencyCode: 'CurrencyCode'
          FlightPrice,

          @Consumption.valueHelpDefinition: [ { entity: { name: 'I_CurrencyStdVH', element: 'Currency' } } ]
          CurrencyCode,

          @EndUserText.label: 'Overall Status Indicator'
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZRAP110_CALC_BOOK_ELEM_022'
  virtual BookingStatusIndicator : abap.int1,

          @EndUserText.label: 'Initial Days to Flight'
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZRAP110_CALC_BOOK_ELEM_022'
  virtual InitialDaysToFlight : abap.int1,

          @EndUserText.label: 'Days to Flight Indicator'
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZRAP110_CALC_BOOK_ELEM_022'
  virtual DaysToFlightIndicator : abap.int1,

          @EndUserText.label: 'Remaining Days to Flight'
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZRAP110_CALC_BOOK_ELEM_022'
  virtual RemainingDaysToFlight : abap.int1,

          LocalLastChangedAt,
          _Travel : redirected to parent ZRAP110_C_TRAVELTP_022,

          _Customer,
          _Carrier,
          _Connection,
          _Flight,
          _BookingStatus,
          _Currency
}
