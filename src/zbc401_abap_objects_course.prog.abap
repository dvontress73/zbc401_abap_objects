*&---------------------------------------------------------------------*
*& Report ZBC401_EX2
*&---------------------------------------------------------------------*
*&Excercises 2, 3, 4
*& ex 5 add contructor
*& Ex 6 add class constructor
*& EX 7 Inheritance
*& EX 8 Upcast
*& EX 9 Implement Polymorphism using inheritance.
*&---------------------------------------------------------------------*
REPORT zbc401_abap_objects_course.

CLASS lcl_airplane DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name      TYPE string
          iv_planetype TYPE saplane-planetype
        EXCEPTIONS
          wrong_planetype,
*      set_attributes
*        IMPORTING
*          iv_name      TYPE string
*          iv_planetype TYPE saplane-planetype,

      display_attributes.

    CLASS-METHODS:
      class_constructor,
      display_no_airplanes,
      get_no_airplanes RETURNING VALUE(rv_count) TYPE i.


  PROTECTED SECTION.
    CONSTANTS: c_pos_1 TYPE i VALUE 30.
  PRIVATE SECTION.
    TYPES:
      ty_planetypes TYPE STANDARD TABLE OF saplane WITH NON-UNIQUE KEY planetype.

*    CONSTANTS: c_pos_1 TYPE i VALUE 30.

    DATA: mv_name      TYPE string,
          mv_planetype TYPE saplane-planetype,
          mv_weight    TYPE saplane-weight,
          mv_tankcap   TYPE saplane-tankcap.

    CLASS-DATA: gv_no_airplanes TYPE i,
                gt_planetypes   TYPE ty_planetypes.

    CLASS-METHODS:
      get_technical_attributes
        IMPORTING
          iv_type    TYPE saplane-planetype
        EXPORTING
          ev_weight  TYPE saplane-weight
          ev_tankcap TYPE saplane-tankcap
        EXCEPTIONS
          wrong_planetype.


ENDCLASS.
*-------------------------------------------------------------------------
*Class lcl_airplane implementation
*-------------------------------------------------------------------------
CLASS lcl_airplane IMPLEMENTATION.
  METHOD class_constructor.
    SELECT * FROM saplane INTO TABLE gt_planetypes.
  ENDMETHOD.

*  METHOD set_attributes.
*    mv_name = iv_name.
*    mv_planetype = iv_planetype.
*
*    gv_no_airplanes = gv_no_airplanes + 1.
*  ENDMETHOD.
  METHOD constructor.
*    DATA: ls_planetype TYPE saplane.

    mv_name = iv_name.
    mv_planetype = iv_planetype.

*    SELECT SINGLE * FROM saplane INTO ls_planetype
*      WHERE planetype = iv_planetype.

    get_technical_attributes(
      EXPORTING
        iv_type = iv_planetype
      IMPORTING
        ev_weight  = mv_weight
        ev_tankcap = mv_tankcap
      EXCEPTIONS
        wrong_planetype = 1 ).

    IF sy-subrc <> 0.
      RAISE wrong_planetype.
    ELSE.
*      mv_weight = ls_planetype-weight.
*      mv_tankcap = ls_planetype-tankcap.
      gv_no_airplanes = gv_no_airplanes + 1.
    ENDIF.
  ENDMETHOD.
  METHOD display_attributes.
    WRITE: / icon_ws_plane AS ICON,
           / 'Name of Airplane'(001), AT c_pos_1 mv_name,
           / 'Type of Airplane'(002), AT c_pos_1 mv_planetype,
           / 'Weight'(003), AT c_pos_1 mv_weight LEFT-JUSTIFIED,
           / 'Tank Capacity'(004), AT c_pos_1  mv_tankcap LEFT-JUSTIFIED.

  ENDMETHOD.

  METHOD display_no_airplanes.
    SKIP.
    WRITE: / 'Number of airplanes:'(cal), AT c_pos_1 gv_no_airplanes LEFT-JUSTIFIED.


  ENDMETHOD.

  METHOD get_no_airplanes.
    rv_count = gv_no_airplanes.
  ENDMETHOD.

  METHOD get_technical_attributes.
    DATA: ls_planetype TYPE saplane.

    READ TABLE gt_planetypes INTO ls_planetype WITH TABLE KEY planetype = iv_type
                                               TRANSPORTING weight tankcap.

    IF sy-subrc = 0.
      ev_weight  = ls_planetype-weight.
      ev_tankcap = ls_planetype-tankcap.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class lcl_cargo_plane
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_cargo_plane DEFINITION INHERITING FROM lcl_airplane.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name      TYPE string
          iv_planetype TYPE saplane-planetype
          iv_cargo     TYPE s_plan_car
        EXCEPTIONS
          wrong_planetype,

      display_attributes REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
          mv_cargo TYPE s_plan_car.

ENDCLASS.

CLASS lcl_cargo_plane IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      EXPORTING
          iv_name      = iv_name
          iv_planetype = iv_planetype
      EXCEPTIONS
          wrong_planetype = 1 ).

    IF sy-subrc <> 0.
      RAISE wrong_planetype.
    ELSE.

    ENDIF.

    mv_cargo = iv_cargo.
  ENDMETHOD.

  METHOD display_attributes.
    super->display_attributes( ).
    WRITE:
      / 'Max Cargo:'(005), AT c_pos_1 mv_cargo LEFT-JUSTIFIED.
    ULINE.

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class lcl_passenger_plane
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_passenger_plane DEFINITION INHERITING FROM lcl_airplane.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name      TYPE string
          iv_planetype TYPE saplane-planetype
          iv_seats     TYPE s_seatsmax
        EXCEPTIONS
          wrong_planetype,

      display_attributes REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mv_seats TYPE s_seatsmax.

ENDCLASS.

CLASS lcl_passenger_plane IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      EXPORTING
        iv_name      = iv_name
        iv_planetype = iv_planetype
      EXCEPTIONS
        wrong_planetype = 1 ).

    IF sy-subrc <> 0.
      RAISE wrong_planetype.
    ENDIF.

    mv_seats = iv_seats.
  ENDMETHOD.

  METHOD display_attributes.
    super->display_attributes( ).

    WRITE:
      / 'Max Seats'(006), AT c_pos_1 mv_seats LEFT-JUSTIFIED.

    ULINE.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Class lcl_carrier Definition
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_carrier DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_name TYPE string,

      display_attributes,

      add_airplane
        IMPORTING
          io_plane TYPE REF TO lcl_airplane.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      mv_name      TYPE string,
      mt_airplanes TYPE TABLE OF REF TO lcl_airplane.

    METHODS:
      display_airplanes.

ENDCLASS.

CLASS lcl_carrier IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.

  METHOD display_attributes.
    SKIP 2.
    WRITE: icon_flight AS ICON, mv_name.
    ULINE.
    ULINE.
    me->display_airplanes( ).
  ENDMETHOD.

  METHOD add_airplane.
    APPEND io_plane TO mt_airplanes.
  ENDMETHOD.

  METHOD display_airplanes.
    DATA: lo_plane TYPE REF TO lcl_airplane.

    LOOP AT mt_airplanes INTO lo_plane.
      lo_plane->display_attributes( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


DATA: go_airplane  TYPE REF TO lcl_airplane.
DATA: gt_airplanes TYPE TABLE OF REF TO lcl_airplane,
      gv_count     TYPE i,
      go_cargo     TYPE REF TO lcl_cargo_plane,
      go_passenger TYPE REF TO lcl_passenger_plane,
      go_carrier   TYPE REF TO lcl_carrier.

START-OF-SELECTION.

*  lcl_airplane=>display_no_airplanes( ).

  CREATE OBJECT go_carrier
    EXPORTING
      iv_name = 'Smile&Fly-Travel'.


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

*--Output carrier (including list of airplanes)
go_carrier->display_attributes( ).



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
