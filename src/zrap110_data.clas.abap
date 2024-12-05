CLASS zrap110_data DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.


CLASS zrap110_data IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    READ ENTITIES OF zrap110_r_traveltp_022
         ENTITY travel BY \_Booking
         ALL FIELDS
         WITH VALUE #( ( travelid  = 8 )
                       ( travelid = 3 ) )
         RESULT DATA(travel)
         LINK DATA(link_travel).

    READ ENTITIES OF zrap110_r_traveltp_022
         ENTITY Booking BY \_travel
         ALL FIELDS
         WITH VALUE #( ( travelid  = 8 BookingID = 10 )
                       ( travelid = 3 bookingID = 10 ) ( travelid = 3 bookingID = 20 )  )
         RESULT DATA(booking)
         LINK DATA(link_booking).

    out->write( name = 'travel'
                data = travel ).

    out->write( name = 'link_travel'
                data = link_travel ).

    out->write( '---------------------------------------' ).

    out->write( name = 'booking'
                data = booking ).

    out->write( name = 'link_booking'
                data = link_booking ).

  ENDMETHOD.


ENDCLASS.
