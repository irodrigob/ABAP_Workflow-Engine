*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZWFE_V001.......................................*
FORM GET_DATA_ZWFE_V001.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZWFE_T003 WHERE
(VIM_WHERETAB) .
    CLEAR ZWFE_V001 .
ZWFE_V001-MANDT =
ZWFE_T003-MANDT .
ZWFE_V001-WORKFLOW =
ZWFE_T003-WORKFLOW .
ZWFE_V001-STATUS =
ZWFE_T003-STATUS .
ZWFE_V001-OWNER =
ZWFE_T003-OWNER .
ZWFE_V001-INIT =
ZWFE_T003-INIT .
ZWFE_V001-DRAFT =
ZWFE_T003-DRAFT .
ZWFE_V001-REJECT =
ZWFE_T003-REJECT .
ZWFE_V001-COMPLETE =
ZWFE_T003-COMPLETE .
    SELECT SINGLE * FROM ZWFE_T001 WHERE
WORKFLOW = ZWFE_T003-WORKFLOW .
    IF SY-SUBRC EQ 0.
    ENDIF.
    SELECT SINGLE * FROM ZWFE_T002 WHERE
STATUS = ZWFE_T003-STATUS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZWFE_T002T WHERE
STATUS = ZWFE_T002-STATUS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZWFE_V001-DESC_STATUS =
ZWFE_T002T-DESCRIPTION .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM ZWFE_T005 WHERE
OWNER = ZWFE_T003-OWNER .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZWFE_T005T WHERE
OWNER = ZWFE_T005-OWNER AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZWFE_V001-DESC_OWNER =
ZWFE_T005T-DESCRIPTION .
      ENDIF.
    ENDIF.
<VIM_TOTAL_STRUC> = ZWFE_V001.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZWFE_V001 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZWFE_V001.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZWFE_V001-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZWFE_T003 WHERE
  WORKFLOW = ZWFE_V001-WORKFLOW AND
  STATUS = ZWFE_V001-STATUS .
    IF SY-SUBRC = 0.
    DELETE ZWFE_T003 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZWFE_T003 WHERE
  WORKFLOW = ZWFE_V001-WORKFLOW AND
  STATUS = ZWFE_V001-STATUS .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZWFE_T003.
    ENDIF.
ZWFE_T003-MANDT =
ZWFE_V001-MANDT .
ZWFE_T003-WORKFLOW =
ZWFE_V001-WORKFLOW .
ZWFE_T003-STATUS =
ZWFE_V001-STATUS .
ZWFE_T003-OWNER =
ZWFE_V001-OWNER .
ZWFE_T003-INIT =
ZWFE_V001-INIT .
ZWFE_T003-DRAFT =
ZWFE_V001-DRAFT .
ZWFE_T003-REJECT =
ZWFE_V001-REJECT .
ZWFE_T003-COMPLETE =
ZWFE_V001-COMPLETE .
    IF SY-SUBRC = 0.
    UPDATE ZWFE_T003 ##WARN_OK.
    ELSE.
    INSERT ZWFE_T003 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZWFE_V001-UPD_FLAG,
STATUS_ZWFE_V001-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZWFE_V001.
  SELECT SINGLE * FROM ZWFE_T003 WHERE
WORKFLOW = ZWFE_V001-WORKFLOW AND
STATUS = ZWFE_V001-STATUS .
ZWFE_V001-MANDT =
ZWFE_T003-MANDT .
ZWFE_V001-WORKFLOW =
ZWFE_T003-WORKFLOW .
ZWFE_V001-STATUS =
ZWFE_T003-STATUS .
ZWFE_V001-OWNER =
ZWFE_T003-OWNER .
ZWFE_V001-INIT =
ZWFE_T003-INIT .
ZWFE_V001-DRAFT =
ZWFE_T003-DRAFT .
ZWFE_V001-REJECT =
ZWFE_T003-REJECT .
ZWFE_V001-COMPLETE =
ZWFE_T003-COMPLETE .
    SELECT SINGLE * FROM ZWFE_T001 WHERE
WORKFLOW = ZWFE_T003-WORKFLOW .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
    SELECT SINGLE * FROM ZWFE_T002 WHERE
STATUS = ZWFE_T003-STATUS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZWFE_T002T WHERE
STATUS = ZWFE_T002-STATUS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZWFE_V001-DESC_STATUS =
ZWFE_T002T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZWFE_V001-DESC_STATUS .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZWFE_V001-DESC_STATUS .
    ENDIF.
    SELECT SINGLE * FROM ZWFE_T005 WHERE
OWNER = ZWFE_T003-OWNER .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZWFE_T005T WHERE
OWNER = ZWFE_T005-OWNER AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZWFE_V001-DESC_OWNER =
ZWFE_T005T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZWFE_V001-DESC_OWNER .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZWFE_V001-DESC_OWNER .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZWFE_V001 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZWFE_V001-WORKFLOW TO
ZWFE_T003-WORKFLOW .
MOVE ZWFE_V001-STATUS TO
ZWFE_T003-STATUS .
MOVE ZWFE_V001-MANDT TO
ZWFE_T003-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZWFE_T003'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZWFE_T003 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZWFE_T003'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZWFE_V001 USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZWFE_T003-MANDT =
ZWFE_V001-MANDT .
ZWFE_T003-WORKFLOW =
ZWFE_V001-WORKFLOW .
ZWFE_T003-STATUS =
ZWFE_V001-STATUS .
ZWFE_T003-OWNER =
ZWFE_V001-OWNER .
ZWFE_T003-INIT =
ZWFE_V001-INIT .
ZWFE_T003-DRAFT =
ZWFE_V001-DRAFT .
ZWFE_T003-REJECT =
ZWFE_V001-REJECT .
ZWFE_T003-COMPLETE =
ZWFE_V001-COMPLETE .
    SELECT SINGLE * FROM ZWFE_T001 WHERE
WORKFLOW = ZWFE_T003-WORKFLOW .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
    SELECT SINGLE * FROM ZWFE_T002 WHERE
STATUS = ZWFE_T003-STATUS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZWFE_T002T WHERE
STATUS = ZWFE_T002-STATUS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZWFE_V001-DESC_STATUS =
ZWFE_T002T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZWFE_V001-DESC_STATUS .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZWFE_V001-DESC_STATUS .
    ENDIF.
    SELECT SINGLE * FROM ZWFE_T005 WHERE
OWNER = ZWFE_T003-OWNER .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZWFE_T005T WHERE
OWNER = ZWFE_T005-OWNER AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZWFE_V001-DESC_OWNER =
ZWFE_T005T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZWFE_V001-DESC_OWNER .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZWFE_V001-DESC_OWNER .
    ENDIF.
ENDFORM.
*...processing: ZWFE_V002.......................................*
FORM GET_DATA_ZWFE_V002.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZWFE_T004 WHERE
(VIM_WHERETAB) .
    CLEAR ZWFE_V002 .
ZWFE_V002-MANDT =
ZWFE_T004-MANDT .
ZWFE_V002-WORKFLOW =
ZWFE_T004-WORKFLOW .
ZWFE_V002-STATUS =
ZWFE_T004-STATUS .
ZWFE_V002-STATUS_NEXT =
ZWFE_T004-STATUS_NEXT .
    SELECT SINGLE * FROM ZWFE_T002 WHERE
STATUS = ZWFE_T004-STATUS_NEXT .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZWFE_T002T WHERE
STATUS = ZWFE_T002-STATUS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZWFE_V002-DESCRIPTION =
ZWFE_T002T-DESCRIPTION .
      ENDIF.
    ENDIF.
<VIM_TOTAL_STRUC> = ZWFE_V002.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZWFE_V002 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZWFE_V002.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZWFE_V002-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZWFE_T004 WHERE
  WORKFLOW = ZWFE_V002-WORKFLOW AND
  STATUS = ZWFE_V002-STATUS AND
  STATUS_NEXT = ZWFE_V002-STATUS_NEXT .
    IF SY-SUBRC = 0.
    DELETE ZWFE_T004 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZWFE_T004 WHERE
  WORKFLOW = ZWFE_V002-WORKFLOW AND
  STATUS = ZWFE_V002-STATUS AND
  STATUS_NEXT = ZWFE_V002-STATUS_NEXT .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZWFE_T004.
    ENDIF.
ZWFE_T004-MANDT =
ZWFE_V002-MANDT .
ZWFE_T004-WORKFLOW =
ZWFE_V002-WORKFLOW .
ZWFE_T004-STATUS =
ZWFE_V002-STATUS .
ZWFE_T004-STATUS_NEXT =
ZWFE_V002-STATUS_NEXT .
    IF SY-SUBRC = 0.
    UPDATE ZWFE_T004 ##WARN_OK.
    ELSE.
    INSERT ZWFE_T004 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZWFE_V002-UPD_FLAG,
STATUS_ZWFE_V002-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZWFE_V002.
  SELECT SINGLE * FROM ZWFE_T004 WHERE
WORKFLOW = ZWFE_V002-WORKFLOW AND
STATUS = ZWFE_V002-STATUS AND
STATUS_NEXT = ZWFE_V002-STATUS_NEXT .
ZWFE_V002-MANDT =
ZWFE_T004-MANDT .
ZWFE_V002-WORKFLOW =
ZWFE_T004-WORKFLOW .
ZWFE_V002-STATUS =
ZWFE_T004-STATUS .
ZWFE_V002-STATUS_NEXT =
ZWFE_T004-STATUS_NEXT .
    SELECT SINGLE * FROM ZWFE_T002 WHERE
STATUS = ZWFE_T004-STATUS_NEXT .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZWFE_T002T WHERE
STATUS = ZWFE_T002-STATUS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZWFE_V002-DESCRIPTION =
ZWFE_T002T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZWFE_V002-DESCRIPTION .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZWFE_V002-DESCRIPTION .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZWFE_V002 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZWFE_V002-WORKFLOW TO
ZWFE_T004-WORKFLOW .
MOVE ZWFE_V002-STATUS TO
ZWFE_T004-STATUS .
MOVE ZWFE_V002-STATUS_NEXT TO
ZWFE_T004-STATUS_NEXT .
MOVE ZWFE_V002-MANDT TO
ZWFE_T004-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZWFE_T004'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZWFE_T004 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZWFE_T004'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZWFE_V002 USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZWFE_T004-MANDT =
ZWFE_V002-MANDT .
ZWFE_T004-WORKFLOW =
ZWFE_V002-WORKFLOW .
ZWFE_T004-STATUS =
ZWFE_V002-STATUS .
ZWFE_T004-STATUS_NEXT =
ZWFE_V002-STATUS_NEXT .
    SELECT SINGLE * FROM ZWFE_T002 WHERE
STATUS = ZWFE_T004-STATUS_NEXT .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZWFE_T002T WHERE
STATUS = ZWFE_T002-STATUS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZWFE_V002-DESCRIPTION =
ZWFE_T002T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZWFE_V002-DESCRIPTION .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZWFE_V002-DESCRIPTION .
    ENDIF.
ENDFORM.
*...processing: ZWFE_V003.......................................*
FORM GET_DATA_ZWFE_V003.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZWFE_T006 WHERE
(VIM_WHERETAB) .
    CLEAR ZWFE_V003 .
ZWFE_V003-MANDT =
ZWFE_T006-MANDT .
ZWFE_V003-OWNER =
ZWFE_T006-OWNER .
ZWFE_V003-COUNTER =
ZWFE_T006-COUNTER .
ZWFE_V003-APPROVER =
ZWFE_T006-APPROVER .
<VIM_TOTAL_STRUC> = ZWFE_V003.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZWFE_V003 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZWFE_V003.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZWFE_V003-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZWFE_T006 WHERE
  OWNER = ZWFE_V003-OWNER AND
  COUNTER = ZWFE_V003-COUNTER .
    IF SY-SUBRC = 0.
    DELETE ZWFE_T006 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZWFE_T006 WHERE
  OWNER = ZWFE_V003-OWNER AND
  COUNTER = ZWFE_V003-COUNTER .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZWFE_T006.
    ENDIF.
ZWFE_T006-MANDT =
ZWFE_V003-MANDT .
ZWFE_T006-OWNER =
ZWFE_V003-OWNER .
ZWFE_T006-COUNTER =
ZWFE_V003-COUNTER .
ZWFE_T006-APPROVER =
ZWFE_V003-APPROVER .
    IF SY-SUBRC = 0.
    UPDATE ZWFE_T006 ##WARN_OK.
    ELSE.
    INSERT ZWFE_T006 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZWFE_V003-UPD_FLAG,
STATUS_ZWFE_V003-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZWFE_V003.
  SELECT SINGLE * FROM ZWFE_T006 WHERE
OWNER = ZWFE_V003-OWNER AND
COUNTER = ZWFE_V003-COUNTER .
ZWFE_V003-MANDT =
ZWFE_T006-MANDT .
ZWFE_V003-OWNER =
ZWFE_T006-OWNER .
ZWFE_V003-COUNTER =
ZWFE_T006-COUNTER .
ZWFE_V003-APPROVER =
ZWFE_T006-APPROVER .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZWFE_V003 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZWFE_V003-OWNER TO
ZWFE_T006-OWNER .
MOVE ZWFE_V003-COUNTER TO
ZWFE_T006-COUNTER .
MOVE ZWFE_V003-MANDT TO
ZWFE_T006-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZWFE_T006'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZWFE_T006 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZWFE_T006'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*

* base table related FORM-routines.............
INCLUDE LSVIMFTX .
