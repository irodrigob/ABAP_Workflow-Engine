*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 08.07.2021 at 18:21:34
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZWFE_T001.......................................*
DATA:  BEGIN OF STATUS_ZWFE_T001                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWFE_T001                     .
CONTROLS: TCTRL_ZWFE_T001
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZWFE_T002.......................................*
DATA:  BEGIN OF STATUS_ZWFE_T002                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWFE_T002                     .
CONTROLS: TCTRL_ZWFE_T002
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZWFE_T005.......................................*
DATA:  BEGIN OF STATUS_ZWFE_T005                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWFE_T005                     .
CONTROLS: TCTRL_ZWFE_T005
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZWFE_T006.......................................*
DATA:  BEGIN OF STATUS_ZWFE_T006                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWFE_T006                     .
CONTROLS: TCTRL_ZWFE_T006
            TYPE TABLEVIEW USING SCREEN '0008'.
*...processing: ZWFE_T013.......................................*
DATA:  BEGIN OF STATUS_ZWFE_T013                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWFE_T013                     .
CONTROLS: TCTRL_ZWFE_T013
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZWFE_T014.......................................*
DATA:  BEGIN OF STATUS_ZWFE_T014                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWFE_T014                     .
CONTROLS: TCTRL_ZWFE_T014
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZWFE_V001.......................................*
TABLES: ZWFE_V001, *ZWFE_V001. "view work areas
CONTROLS: TCTRL_ZWFE_V001
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZWFE_V001. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZWFE_V001.
* Table for entries selected to show on screen
DATA: BEGIN OF ZWFE_V001_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZWFE_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWFE_V001_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZWFE_V001_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZWFE_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWFE_V001_TOTAL.

*...processing: ZWFE_V002.......................................*
TABLES: ZWFE_V002, *ZWFE_V002. "view work areas
CONTROLS: TCTRL_ZWFE_V002
TYPE TABLEVIEW USING SCREEN '0004'.
DATA: BEGIN OF STATUS_ZWFE_V002. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZWFE_V002.
* Table for entries selected to show on screen
DATA: BEGIN OF ZWFE_V002_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZWFE_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWFE_V002_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZWFE_V002_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZWFE_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWFE_V002_TOTAL.

*...processing: ZWFE_V003.......................................*
TABLES: ZWFE_V003, *ZWFE_V003. "view work areas
CONTROLS: TCTRL_ZWFE_V003
TYPE TABLEVIEW USING SCREEN '0006'.
DATA: BEGIN OF STATUS_ZWFE_V003. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZWFE_V003.
* Table for entries selected to show on screen
DATA: BEGIN OF ZWFE_V003_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZWFE_V003.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWFE_V003_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZWFE_V003_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZWFE_V003.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZWFE_V003_TOTAL.

*.........table declarations:.................................*
TABLES: *ZWFE_T001                     .
TABLES: *ZWFE_T001T                    .
TABLES: *ZWFE_T002                     .
TABLES: *ZWFE_T002T                    .
TABLES: *ZWFE_T005                     .
TABLES: *ZWFE_T005T                    .
TABLES: *ZWFE_T006                     .
TABLES: *ZWFE_T013                     .
TABLES: *ZWFE_T014                     .
TABLES: ZWFE_T001                      .
TABLES: ZWFE_T001T                     .
TABLES: ZWFE_T002                      .
TABLES: ZWFE_T002T                     .
TABLES: ZWFE_T003                      .
TABLES: ZWFE_T004                      .
TABLES: ZWFE_T005                      .
TABLES: ZWFE_T005T                     .
TABLES: ZWFE_T006                      .
TABLES: ZWFE_T013                      .
TABLES: ZWFE_T014                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
