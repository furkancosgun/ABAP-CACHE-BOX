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
  TRY.
      lo_data_cache->get(
        EXPORTING
          key             = 'AA'
        IMPORTING
          value           = DATA(ls_scarr_aa)
      ).
      WRITE: / 'SCARR data for AA:', ls_scarr_aa-carrid, ls_scarr_aa-carrname.
    CATCH cx_root INTO DATA(lx_root).
      WRITE: / 'Error retrieving SCARR data:', lx_root->get_text( ).
  ENDTRY.

  " Display the size of the cache
  DATA(cache_size) = lo_data_cache->size( ).
  WRITE: / 'Cache size:', cache_size.

  " Delete an entry from the cache
  TRY.
      lo_data_cache->delete(
        EXPORTING
          key             = 'AA'
      ).
      WRITE: / 'Entry with key AA deleted from cache.'.
    CATCH cx_root INTO lx_root.
      WRITE: / 'Error deleting cache entry:', lx_root->get_text( ).
  ENDTRY.

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
  TRY.
      lo_string_cache->get(
        EXPORTING
          key             = 'NAME'
        IMPORTING
          value           = DATA(lv_name)
      ).
      WRITE: / 'Cached name:', lv_name.
    CATCH cx_root INTO lx_root.
      WRITE: / 'Error retrieving cached name:', lx_root->get_text( ).
  ENDTRY.

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
  TRY.
      lo_oref_cache->get(
        EXPORTING
          key             = 'SALV'
        IMPORTING
          value           = DATA(lo_salv2)
      ).
      " Display the ALV table
      lo_salv2->display( ).
    CATCH cx_root INTO lx_root.
      WRITE: / 'Error retrieving ALV table from cache:', lx_root->get_text( ).
  ENDTRY.
