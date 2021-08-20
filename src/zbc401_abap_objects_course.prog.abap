*&---------------------------------------------------------------------*
*& Report ZBC401_EX2
*&---------------------------------------------------------------------*
*&Excercises 2, 3, 4
*& ex 5 add contructor
*& Ex 6 add class constructor
*& EX 7 Inheritance
*& EX 8 Upcast
*& EX 9 Implement Polymorphism using inheritance.
*& EX 10 Implement Downcasts
*& EX 11 Implement local interface
*&---------------------------------------------------------------------*
REPORT zbc401_abap_objects_course.

include zbc401_agency.
include zbc401_carrier.


DATA: go_airplane  TYPE REF TO lcl_airplane.
DATA:"gt_airplanes TYPE TABLE OF REF TO lcl_airplane,
      gv_count     TYPE i,
      go_cargo     TYPE REF TO lcl_cargo_plane,
      go_passenger TYPE REF TO lcl_passenger_plane,
      go_carrier   TYPE REF TO lcl_carrier,
      go_agency    TYPE REF TO lcl_travel_agency.

START-OF-SELECTION.

*  lcl_airplane=>display_no_airplanes( ).

  create OBJECT go_agency
    EXPORTING
        iv_name = 'Travel&Smile Travel'.

  CREATE OBJECT go_carrier
    EXPORTING
      iv_name = 'Smile&Fly-Travel'.

*Insert carrier into business partner list of travel agency...
    go_agency->add_partner( io_partner = go_carrier ).

*Passenger plane...
  CREATE OBJECT go_passenger
    EXPORTING
      iv_name         = 'LH_Berlin'
      iv_planetype    = '747-400'
      iv_seats        = 345
    EXCEPTIONS
      wrong_planetype = 1.

  IF sy-subrc = 0.
*    APPEND go_passenger TO gt_airplanes.
    go_carrier->add_airplane( go_passenger ).
  ENDIF.

*Cargo plane...
  CREATE OBJECT go_cargo
    EXPORTING
      iv_name         = 'US Hercules'
      iv_planetype    = '747-200F'
      iv_cargo        = 533
    EXCEPTIONS
      wrong_planetype = 1.

  IF sy-subrc = 0.
*    APPEND go_cargo TO gt_airplanes.
    go_carrier->add_airplane( go_cargo ).
  ENDIF.

*Show attributes of all partners of the travel agency...
go_agency->display_attributes( ).

*--Output carrier (including list of airplanes)
go_carrier->lif_partner~display_partner(  ).
*go_carrier->display_attributes( ).



*  go_passenger->display_attributes( ).
*  go_cargo->display_attributes( ).

*  gv_count = lcl_airplane=>get_no_airplanes( ).
*
*  SKIP 2.
*  WRITE: / 'Number of airplanes'(cal), gv_count.

**********************************************************
* Exercises 1 to 6
**********************************************************

*  CREATE OBJECT go_airplane
*    EXPORTING
*      iv_name         = 'LH_Berlin'
*      iv_planetype    = 'A321-200'
*    EXCEPTIONS
*      wrong_planetype = 1.
*
*  IF sy-subrc = 0.
*    APPEND go_airplane TO gt_airplanes.
*  ENDIF.
*
*  CREATE OBJECT go_airplane
*    EXPORTING
*      iv_name         = 'AA New York'
*      iv_planetype    = '747-400'
*    EXCEPTIONS
*      wrong_planetype = 1.
*
*  IF sy-subrc = 0.
*    APPEND go_airplane TO gt_airplanes.
*  ENDIF.
*
*  CREATE OBJECT go_airplane
*    EXPORTING
*      iv_name         = 'US Hercules'
*      iv_planetype    = '747-200F'
*    EXCEPTIONS
*      wrong_planetype = 1.
*
*  IF sy-subrc = 0.
*    APPEND go_airplane TO gt_airplanes.
*  ENDIF.
