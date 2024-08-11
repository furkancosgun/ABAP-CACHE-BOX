*&---------------------------------------------------------------------*
*& Report ZCACHE_BOX_P_EXAMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcache_box_p_example.

INCLUDE zcache_box_i_gen. "Include generator

INITIALIZATION.
  " Define cache classes for different data types
  generate_data_cache_box: scarr scarr.  " Cache for SCARR data
  generate_data_cache_box: string string. " Cache for string values
  generate_oref_cache_box: salv cl_salv_table. " Cache for object references

START-OF-SELECTION.
  " Example of cache usage with a value type (SCARR)

  " Retrieve the singleton instance of the SCARR cache
  DATA(lo_data_cache) = zcl_scarr_cache_box=>get_instance( ).

  " Fetch data from SCARR table
  SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).

  " Populate the cache with SCARR data
  LOOP AT lt_scarr INTO DATA(ls_scarr).
    lo_data_cache->put(
      EXPORTING
        key   = CONV #( ls_scarr-carrid )
        value = ls_scarr
    ).
  ENDLOOP.

  " Retrieve an entry from the cache
  lo_data_cache->get(
    EXPORTING
      key             = 'AA'
    IMPORTING
      value           = DATA(ls_scarr_aa)
    EXCEPTIONS
      entry_not_found = 1
      OTHERS          = 2
    ).
  IF sy-subrc EQ 0.
    WRITE: / 'SCARR data for AA:', ls_scarr_aa-carrid, ls_scarr_aa-carrname.
  ELSE.
    WRITE: / 'Entry not found'.
  ENDIF.

  " Display the size of the cache
  DATA(cache_size) = lo_data_cache->size( ).
  WRITE: / 'Cache size:', cache_size.

  " Delete an entry from the cache
  lo_data_cache->delete(
    EXPORTING
      key             = 'AA'
    EXCEPTIONS
      entry_not_found = 1
      OTHERS          = 2
  ).
  IF sy-subrc EQ 0.
    WRITE: / 'Entry with key AA deleted from cache.'.
  ELSE.
    WRITE: / 'Entry not found'.
  ENDIF.

  " Clear all entries from the cache
  lo_data_cache->clear( ).
  WRITE: / 'Cache cleared.'.

  " Example of cache usage with a value type (string)

  " Retrieve the singleton instance of the string cache
  DATA(lo_string_cache) = zcl_string_cache_box=>get_instance( ).

  " Add a string value to the cache
  lo_string_cache->put(
    EXPORTING
      key   = 'NAME'
      value = 'FURKAN'
  ).

  " Retrieve a string value from the cache

  lo_string_cache->get(
    EXPORTING
      key             = 'NAME'
    IMPORTING
      value           = DATA(lv_name)
    EXCEPTIONS
      entry_not_found = 1
      OTHERS          = 2
  ).
  IF sy-subrc EQ 0.
    WRITE: / 'Cached name:', lv_name.
  ELSE.
    WRITE: / 'Entry not found'.
  ENDIF.

  " Example of cache usage with a reference type (CL_SALV_TABLE)

  " Retrieve the singleton instance of the reference cache
  DATA(lo_oref_cache) = zcl_salv_cache_box=>get_instance( ).

  " Create an ALV table
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   = DATA(lo_salv)
    CHANGING
      t_table        = lt_scarr
  ).

  " Add the ALV table object to the cache
  lo_oref_cache->put(
    EXPORTING
      key   = 'SALV'
      value = lo_salv
  ).

  " Retrieve the ALV table object from the cache
  lo_oref_cache->get(
    EXPORTING
      key             = 'SALV'
    IMPORTING
      value           = DATA(lo_salv2)
    EXCEPTIONS
      entry_not_found = 1
      OTHERS          = 2
  ).
  IF sy-subrc EQ 0.
    " Display the ALV table
    lo_salv2->display( ).
  ELSE.
    WRITE: / 'Entry not found'.
  ENDIF.
