CLASS zrap110_cl_datacheck DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  FOR BEHAVIOR OF ZRAP110_C_TRAVELTP_022.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.


CLASS zrap110_cl_datacheck IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    READ ENTITIES OF ZRAP110_C_TRAVELTP_022 ENTITY booking ALL FIELDS WITH value #( ( bookingID = 10 ) ) result data(result).

  ENDMETHOD.


ENDCLASS.
