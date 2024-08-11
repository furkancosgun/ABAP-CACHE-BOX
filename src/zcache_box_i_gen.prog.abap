*&---------------------------------------------------------------------*
*&  Include           ZCACHE_BOX_I_GEN
*&---------------------------------------------------------------------*
"For data type
DEFINE generate_data_cache_box.
  CLASS zcl_&1_cache_box DEFINITION.
    PUBLIC SECTION.
      TYPES: ty_cache_key TYPE string,
             BEGIN OF ty_cache_box,
               key   TYPE ty_cache_key,       " Cache key
               value TYPE &2,                  " Value to be cached (generic type)
             END OF ty_cache_box.

      " Method to get the singleton instance of the cache
      CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_&1_cache_box.

      " Method to add data to the cache
      METHODS: put
        IMPORTING
          key   TYPE ty_cache_key   " Cache key
          value TYPE &2.            " Data to be added

      " Method to retrieve data from the cache
      METHODS: get
        IMPORTING
          key   TYPE ty_cache_key   " Cache key
        EXPORTING
          value TYPE &2             " Retrieved data
        EXCEPTIONS
          entry_not_found.         " Entry not found

      " Method to get the number of entries in the cache
      METHODS: size RETURNING VALUE(r_size) TYPE i.   " Number of entries in the cache

      " Method to delete a specific entry from the cache
      METHODS: delete
        IMPORTING
          key TYPE ty_cache_key   " Key of the entry to be deleted
        EXCEPTIONS
          entry_not_found.        " Entry not found

      " Method to clear all entries from the cache
      METHODS: clear.

    PRIVATE SECTION.
      " Singleton instance of the class
      CLASS-DATA: instance TYPE REF TO zcl_&1_cache_box.
      " Hash table for storing cache data
      DATA: cache_box TYPE HASHED TABLE OF ty_cache_box WITH UNIQUE KEY key.
  ENDCLASS.
  CLASS zcl_&1_cache_box IMPLEMENTATION.

    METHOD get_instance.
      " Singleton pattern to get the class instance
      IF instance IS NOT BOUND.
        CREATE OBJECT instance.
      ENDIF.
      ro_instance = instance.
    ENDMETHOD.

    METHOD put.
      " Insert data into the cache
      INSERT VALUE #( key   = key
                      value = value ) INTO TABLE cache_box.
    ENDMETHOD.

    METHOD get.
      " Retrieve data from the cache
      FIELD-SYMBOLS: <fs_value> TYPE any.

      READ TABLE cache_box WITH KEY key = key REFERENCE INTO DATA(lr_cache).
      IF sy-subrc NE 0.
        RAISE entry_not_found.
      ENDIF.

      CHECK value IS REQUESTED.

      value = lr_cache->value.
    ENDMETHOD.

    METHOD size.
      " Get the number of entries in the cache
      r_size = lines( cache_box ).
    ENDMETHOD.

    METHOD delete.
      " Delete a specific entry from the cache
      DELETE cache_box WHERE key = key.
      IF sy-subrc NE 0.
        RAISE entry_not_found.
      ENDIF.
    ENDMETHOD.

    METHOD clear.
      " Clear all entries from the cache
      CLEAR cache_box.
    ENDMETHOD.

  ENDCLASS.
END-OF-DEFINITION.


"For referance type
DEFINE generate_oref_cache_box.
  CLASS zcl_&1_cache_box DEFINITION.
    PUBLIC SECTION.
      TYPES: ty_cache_key TYPE string,
             BEGIN OF ty_cache_box,
               key   TYPE ty_cache_key,       " Cache key
               value TYPE REF TO &2,           " Object reference to be cached
             END OF ty_cache_box.

      " Method to get the singleton instance of the cache
      CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_&1_cache_box.

      " Method to add data to the cache
      METHODS: put
        IMPORTING
          key   TYPE ty_cache_key   " Cache key
          value TYPE REF TO &2.    " Object reference to be added

      " Method to retrieve data from the cache
      METHODS: get
        IMPORTING
          key   TYPE ty_cache_key   " Cache key
        EXPORTING
          value TYPE REF TO &2     " Retrieved object reference
        EXCEPTIONS
          entry_not_found.        " Entry not found

      " Method to get the number of entries in the cache
      METHODS: size RETURNING VALUE(r_size) TYPE i.   " Number of entries in the cache

      " Method to delete a specific entry from the cache
      METHODS: delete
        IMPORTING
          key TYPE ty_cache_key   " Key of the entry to be deleted
        EXCEPTIONS
          entry_not_found.        " Entry not found

      " Method to clear all entries from the cache
      METHODS: clear.

    PRIVATE SECTION.
      " Singleton instance of the class
      CLASS-DATA: instance TYPE REF TO zcl_&1_cache_box.
      " Hash table for storing cache data
      DATA: cache_box TYPE HASHED TABLE OF ty_cache_box WITH UNIQUE KEY key.
  ENDCLASS.

  CLASS zcl_&1_cache_box IMPLEMENTATION.

    METHOD get_instance.
      " Singleton pattern to get the class instance
      IF instance IS NOT BOUND.
        CREATE OBJECT instance.
      ENDIF.
      ro_instance = instance.
    ENDMETHOD.

    METHOD put.
      " Insert object reference into the cache
      INSERT VALUE #( key   = key
                      value = value ) INTO TABLE cache_box.
    ENDMETHOD.

    METHOD get.
      " Retrieve object reference from the cache
      FIELD-SYMBOLS: <fs_value> TYPE any.

      READ TABLE cache_box WITH KEY key = key REFERENCE INTO DATA(lr_cache).
      IF sy-subrc NE 0.
        RAISE entry_not_found.
      ENDIF.

      CHECK value IS REQUESTED.

      value = lr_cache->value.
    ENDMETHOD.

    METHOD size.
      " Get the number of entries in the cache
      r_size = lines( cache_box ).
    ENDMETHOD.

    METHOD delete.
      " Delete a specific entry from the cache
      DELETE cache_box WHERE key = key.
      IF sy-subrc NE 0.
        RAISE entry_not_found.
      ENDIF.
    ENDMETHOD.

    METHOD clear.
      " Clear all entries from the cache
      CLEAR cache_box.
    ENDMETHOD.

  ENDCLASS.
END-OF-DEFINITION.
