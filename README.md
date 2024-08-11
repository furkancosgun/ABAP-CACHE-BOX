# ABAP Cache Box

This repository provides a flexible and reusable ABAP cache implementation using macros. The cache classes are dynamically generated based on the given class name and data type, making it easy to create caches for various data types without duplicating code.

## Overview

The provided macros allow you to generate cache classes for different data types dynamically. This approach avoids code duplication and simplifies the process of creating cache classes for various use cases. The cache implementation uses the Singleton pattern to ensure a single instance of each cache class and employs hashed tables for efficient data access.

# Features

- Dynamic Cache Generation: Use macros to generate cache classes for different data types without duplicating code.
- Singleton Pattern: Ensures a single instance of each cache class.
- Hash Table Storage: Efficient storage and retrieval of cache entries.
- Flexible Data Types: Supports various types, including value types and object references.
- Error Handling: Includes error handling for cache operations.

# Installation

- Copy the cache class macros and definitions into your ABAP development environment.
- Use the provided examples to integrate and test the cache classes in your application.

## Cache Class Generation

The cache classes are generated using macros. You can create a cache class for any data type by specifying the class name and data type in the macro. The class will be named using the provided class name with the suffix `_CACHE_BOX`.

### Macro Definitions

- ### `generate_data_cache_box`

  Generates a cache class for a value type.

  ### Syntax:

  ```abap
  generate_data_cache_box: <CLASS_NAME> <DATA_TYPE>.
  ```

  ### Example:

  ```abap
  generate_data_cache_box: scarr scarr.
  ```

  This will generate a class named zcl_scarr_cache_box with a cache for SCARR data.

- ### `generate_oref_cache_box`

  Generates a cache class for object references.

  ### Syntax:

  ```abap
  generate_oref_cache_box: <CLASS_NAME> <OBJECT_TYPE>.
  ```

  ### Example:

  ```abap
  generate_oref_cache_box: salv cl_salv_table.
  ```

  This will generate a class named zcl_salv_cache_box with a cache for cl_salv_table object references.

# How to Use

## 1. Generating and Using a Value Type Cache

To generate a cache for a specific value type, use the `generate_data_cache_box` macro. Here is an example of using it for the SCARR table.

```abap
REPORT sy-repid.

INCLUDE zcache_box_i_gen. "Include the macro generator

INITIALIZATION.
  generate_data_cache_box: scarr scarr. " Cache for SCARR data

START-OF-SELECTION.
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
       key = 'AA'
    IMPORTING
      value = DATA(ls_scarr_aa)
    EXCEPTIONS
      entry_not_found = 1
      OTHERS          = 2
  ).
  IF sy-subrc EQ 0.
    WRITE: / 'SCARR data for AA:', ls_scarr_aa-carrid, ls_scarr_aa-carrname.
  ELSE.
    WRITE: / 'Entry Not Found SCARR data for AA'.
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
    WRITE: / 'Entry Not Found SCARR data for AA'.
  ENDIF.

  " Clear all entries from the cache
  lo_data_cache->clear( ).
  WRITE: / 'Cache cleared.'.
```

## 2. Generating and Using an Object Reference Cache

To generate a cache for object references, use the `generate_oref_cache_box` macro. Here is an example of using it for cl_salv_table objects.

```abap

REPORT sy-repid.

INCLUDE zcache_box_i_gen. "Include the macro generator

INITIALIZATION.
  generate_oref_cache_box: salv cl_salv_table. " Cache for object references

START-OF-SELECTION.
  " Retrieve the singleton instance of the reference cache
  DATA(lo_oref_cache) = zcl_salv_cache_box=>get_instance( ).
  SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).
  " Create an ALV table
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = DATA(lo_salv)
    CHANGING
      t_table = lt_scarr
  ).

  " Add the ALV table object to the cache
  lo_oref_cache->put(
    EXPORTING
      key = 'SALV'
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
    WRITE : / 'Entry Not Found'.
  ENDIF.
```

# License

This project is licensed under the MIT License - see the LICENSE file for details.

# Contributing

Contributions to improve the functionality and performance of these cache classes are welcome. Please submit issues or pull requests for any enhancements or bug fixes.
