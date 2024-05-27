&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rcdb             PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

create widget-pool.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brMovto

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES movto banco ccusto conta item

/* Definitions for BROWSE brMovto                                       */
&Scoped-define FIELDS-IN-QUERY-brMovto movto.id movto.data-mvto ~
banco.descricao + " " + conta.ag + " / " + conta.conta ~
ccusto.cc-cod + " - " + ccusto.descricao item.descricao movto.seq ~
movto.valor movto.usuario movto.descricao movto.narrativa 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brMovto 
&Scoped-define QUERY-STRING-brMovto FOR EACH movto NO-LOCK, ~
      EACH banco WHERE banco.banco-cod  = movto.banco-cod NO-LOCK, ~
      EACH ccusto WHERE ccusto.cc-cod = movto.cc-cod NO-LOCK, ~
      EACH conta WHERE conta.banco-cod = movto.banco-cod ~
  AND conta.ag = movto.ag ~
  AND conta.conta = movto.conta NO-LOCK, ~
      EACH item WHERE item.it-cod = movto.it-cod NO-LOCK ~
    BY movto.data-mvto INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brMovto OPEN QUERY brMovto FOR EACH movto NO-LOCK, ~
      EACH banco WHERE banco.banco-cod  = movto.banco-cod NO-LOCK, ~
      EACH ccusto WHERE ccusto.cc-cod = movto.cc-cod NO-LOCK, ~
      EACH conta WHERE conta.banco-cod = movto.banco-cod ~
  AND conta.ag = movto.ag ~
  AND conta.conta = movto.conta NO-LOCK, ~
      EACH item WHERE item.it-cod = movto.it-cod NO-LOCK ~
    BY movto.data-mvto INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brMovto movto banco ccusto conta item
&Scoped-define FIRST-TABLE-IN-QUERY-brMovto movto
&Scoped-define SECOND-TABLE-IN-QUERY-brMovto banco
&Scoped-define THIRD-TABLE-IN-QUERY-brMovto ccusto
&Scoped-define FOURTH-TABLE-IN-QUERY-brMovto conta
&Scoped-define FIFTH-TABLE-IN-QUERY-brMovto item


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brMovto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 btFiltrar cbFiltroPeriodo ~
rsOrdenarPor BtLimparFiltro fill-DataI fill-DataF cbConta gill-geral ~
brMovto EDITOR-1 
&Scoped-Define DISPLAYED-OBJECTS cbFiltroPeriodo rsOrdenarPor fill-DataI ~
fill-DataF cbConta gill-geral EDITOR-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btFiltrar 
     LABEL "Filtrar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BtLimparFiltro 
     LABEL "Limpar" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbConta AS CHARACTER FORMAT "X(256)":U 
     LABEL "Conta" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 42 BY 1 TOOLTIP "Filtrar por conta bancária" NO-UNDO.

DEFINE VARIABLE cbFiltroPeriodo AS CHARACTER FORMAT "X(256)":U INITIAL "30 dias" 
     LABEL "Período" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "7 dias","15 dias","30 dias","60 dias","90 dias","Personalizar" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 TOOLTIP "Filtrar movimentação por período" NO-UNDO.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 255 BY 3.57 NO-UNDO.

DEFINE VARIABLE fill-DataF AS DATE FORMAT "99/99/9999":U 
     LABEL "Até" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Data final" NO-UNDO.

DEFINE VARIABLE fill-DataI AS DATE FORMAT "99/99/9999":U 
     LABEL "De" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Data inicial" NO-UNDO.

DEFINE VARIABLE gill-geral AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE rsOrdenarPor AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Mais novos", 1,
"Mais antigos", 2
     SIZE 22 BY 1.91 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 254.6 BY 4.05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brMovto FOR 
      movto, 
      banco, 
      ccusto, 
      conta, 
      item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brMovto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brMovto C-Win _STRUCTURED
  QUERY brMovto NO-LOCK DISPLAY
      movto.id COLUMN-LABEL "ID" FORMAT "->,>>>,>>9":U
      movto.data-mvto FORMAT "99/99/9999":U
      banco.descricao + " " + conta.ag + " / " + conta.conta COLUMN-LABEL "Conta bancária" FORMAT "x(40)":U
      ccusto.cc-cod + " - " + ccusto.descricao COLUMN-LABEL "Centro de custo" FORMAT "x(30)":U
            WIDTH 25
      item.descricao COLUMN-LABEL "Item" FORMAT "x(30)":U
      movto.seq FORMAT ">,>>>,>>9":U WIDTH 10
      movto.valor FORMAT "->,>>>,>>9.99":U WIDTH 20
      movto.usuario FORMAT "x(8)":U
      movto.descricao FORMAT "x(20)":U
      movto.narrativa FORMAT "x(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 255 BY 24.29
         FONT 10 ROW-HEIGHT-CHARS .91 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btFiltrar AT ROW 1.81 COL 237.6 WIDGET-ID 18
     cbFiltroPeriodo AT ROW 1.91 COL 11 COLON-ALIGNED WIDGET-ID 6
     rsOrdenarPor AT ROW 2.43 COL 192 NO-LABEL WIDGET-ID 22
     BtLimparFiltro AT ROW 3.38 COL 237.6 WIDGET-ID 20
     fill-DataI AT ROW 3.57 COL 11 COLON-ALIGNED WIDGET-ID 8
     fill-DataF AT ROW 3.57 COL 37 COLON-ALIGNED WIDGET-ID 10
     cbConta AT ROW 3.57 COL 66.2 COLON-ALIGNED WIDGET-ID 12
     gill-geral AT ROW 3.57 COL 111.4 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     brMovto AT ROW 5.38 COL 1 WIDGET-ID 200
     EDITOR-1 AT ROW 29.86 COL 1 NO-LABEL WIDGET-ID 28
     "Ordenar por" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.71 COL 192 WIDGET-ID 26
     "Filtrar por id, item, descrição ou narrativa" VIEW-AS TEXT
          SIZE 72 BY .62 AT ROW 2.67 COL 113.4 WIDGET-ID 16
     RECT-2 AT ROW 1.19 COL 1.8 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 255.4 BY 32.48 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 32.48
         WIDTH              = 255.4
         MAX-HEIGHT         = 32.48
         MAX-WIDTH          = 271
         VIRTUAL-HEIGHT     = 32.48
         VIRTUAL-WIDTH      = 271
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brMovto gill-geral DEFAULT-FRAME */
ASSIGN 
       brMovto:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       brMovto:COLUMN-MOVABLE IN FRAME DEFAULT-FRAME         = TRUE
       brMovto:SEPARATOR-FGCOLOR IN FRAME DEFAULT-FRAME      = 15.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brMovto
/* Query rebuild information for BROWSE brMovto
     _TblList          = "rcdb.movto,rcdb.banco WHERE rcdb.movto ...,rcdb.ccusto WHERE rcdb.movto ...,rcdb.conta WHERE rcdb.movto ...,rcdb.item WHERE rcdb.movto ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ",,,,"
     _OrdList          = "rcdb.movto.data-mvto|yes"
     _JoinCode[2]      = "banco.banco-cod  = movto.banco-cod"
     _JoinCode[3]      = "ccusto.cc-cod = movto.cc-cod"
     _JoinCode[4]      = "conta.banco-cod = movto.banco-cod
  AND conta.ag = movto.ag
  AND conta.conta = movto.conta"
     _JoinCode[5]      = "item.it-cod = movto.it-cod"
     _FldNameList[1]   > rcdb.movto.id
"movto.id" "ID" ? "int64" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "FILL-IN" "," ? ? 5 no 0 no no
     _FldNameList[2]   = rcdb.movto.data-mvto
     _FldNameList[3]   > "_<CALC>"
"banco.descricao + "" "" + conta.ag + "" / "" + conta.conta" "Conta bancária" "x(40)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"ccusto.cc-cod + "" - "" + ccusto.descricao" "Centro de custo" "x(30)" ? ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > rcdb.item.descricao
"item.descricao" "Item" "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > rcdb.movto.seq
"movto.seq" ? ">,>>>,>>9" "integer" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > rcdb.movto.valor
"movto.valor" ? ? "decimal" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = rcdb.movto.usuario
     _FldNameList[9]   = rcdb.movto.descricao
     _FldNameList[10]   > rcdb.movto.narrativa
"movto.narrativa" ? ? "character" ? ? ? ? ? ? no ? no no "68.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brMovto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON end-error OF C-Win /* <insert window title> */
or endkey of {&WINDOW-NAME} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON window-close OF C-Win /* <insert window title> */
do:
  /* This event will close the window and terminate the procedure.  */
  apply "CLOSE":U to this-procedure.
  return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brMovto
&Scoped-define SELF-NAME brMovto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brMovto C-Win
ON mouse-select-dblclick OF brMovto IN FRAME DEFAULT-FRAME
do:
    
    message "double click" movto.id movto.it-cod
    view-as alert-box.  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brMovto C-Win
ON row-display OF brMovto IN FRAME DEFAULT-FRAME
do:
    if movto.movto-tp = 1 then do: 
//        Op:fgcolor in browse brMovto = 9.
        movto.valor:fgcolor in browse brMovto = 9.
    end.
    else do: 
  //      Op:fgcolor in browse brMovto = 12.
        movto.valor:fgcolor in browse brMovto = 12.
    end.
    
    //if movto.pendente then movto.pendente:fgcolor = 12.
   // else movto.pendente:fgcolor = 0.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFiltroPeriodo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFiltroPeriodo C-Win
ON value-changed OF cbFiltroPeriodo IN FRAME DEFAULT-FRAME /* Período */
do:
    assign cbFiltroPeriodo.

    if cbFiltroPeriodo = "Personalizar" then 
        assign fill-DataI:hidden = false
               fill-DataF:hidden = false.
    else   
        assign fill-DataI:hidden = true
               fill-DataF:hidden = true.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
assign CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
on close of this-procedure 
   run disable_UI.

/* Best default for GUI applications is...                              */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
   on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
       
  run enable_UI.
  do with frame {&FRAME-NAME}:
     assign fill-DataI:hidden = true
            fill-DataF:hidden = true.
  end.
  if not this-procedure:persistent then
    wait-for close of this-procedure.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cbFiltroPeriodo rsOrdenarPor fill-DataI fill-DataF cbConta gill-geral 
          EDITOR-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 btFiltrar cbFiltroPeriodo rsOrdenarPor BtLimparFiltro 
         fill-DataI fill-DataF cbConta gill-geral brMovto EDITOR-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

