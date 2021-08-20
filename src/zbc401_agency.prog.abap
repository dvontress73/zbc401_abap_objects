*&---------------------------------------------------------------------*
*& Include zbc401_agency
*&---------------------------------------------------------------------*
INTERFACE lif_partner.
  METHODS display_partner.
ENDINTERFACE.

*&---------------------------------------------------------------------*
*& lcl_travel_agency definition
*&---------------------------------------------------------------------*
CLASS lcl_travel_agency DEFINITION.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          iv_name type string,

      add_partner
        IMPORTING
          io_partner TYPE REF TO lif_partner,

      display_agency_partners,

      display_attributes.

  PROTECTED SECTION.

  PRIVATE SECTION.
    data: mv_name type string.
    data: mt_partners TYPE TABLE OF REF TO lif_partner.

ENDCLASS.

*&---------------------------------------------------------------------*
*& lcl_travel_agency IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_travel_agency IMPLEMENTATION.

  METHOD add_partner.

  ENDMETHOD.

  METHOD constructor.

  ENDMETHOD.

  METHOD display_agency_partners.

  ENDMETHOD.

  METHOD display_attributes.

  ENDMETHOD.

ENDCLASS.
