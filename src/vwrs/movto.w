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
define variable c-query   as character no-undo.
define variable c-order   as character no-undo.
define variable l-conta   as logical   no-undo.
define variable l-cc      as logical   no-undo.
define variable l-geral   as logical   no-undo.
define variable d-dataI   as date      no-undo.
define variable d-dataF   as date      no-undo.
define variable c-cc      as character no-undo.
define variable c-banco   as character no-undo.
define variable c-ag      as character no-undo.
define variable c-cta     as character no-undo.
define variable h-query   as handle    no-undo.

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
movto.valor movto.usuario movto.descricao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brMovto 
&Scoped-define QUERY-STRING-brMovto FOR EACH movto NO-LOCK, ~
      EACH banco WHERE banco.banco-cod  = movto.banco-cod NO-LOCK, ~
      EACH ccusto WHERE ccusto.cc-cod = movto.cc-cod NO-LOCK, ~
      EACH conta WHERE conta.banco-cod = movto.banco-cod ~
  AND conta.ag = movto.ag ~
  AND conta.conta = movto.conta NO-LOCK, ~
      EACH item WHERE item.it-cod = movto.it-cod NO-LOCK ~
    BY movto.data-mvto ~
       BY movto.conta ~
        BY movto.cc-cod ~
         BY movto.it-cod ~
          BY movto.seq INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brMovto OPEN QUERY brMovto FOR EACH movto NO-LOCK, ~
      EACH banco WHERE banco.banco-cod  = movto.banco-cod NO-LOCK, ~
      EACH ccusto WHERE ccusto.cc-cod = movto.cc-cod NO-LOCK, ~
      EACH conta WHERE conta.banco-cod = movto.banco-cod ~
  AND conta.ag = movto.ag ~
  AND conta.conta = movto.conta NO-LOCK, ~
      EACH item WHERE item.it-cod = movto.it-cod NO-LOCK ~
    BY movto.data-mvto ~
       BY movto.conta ~
        BY movto.cc-cod ~
         BY movto.it-cod ~
          BY movto.seq INDEXED-REPOSITION.
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
&Scoped-Define ENABLED-OBJECTS cbFiltroPeriodo fill-DataI fill-DataF ~
cbConta cbFiltroCC fill-item btFiltrar BtLimparFiltro brMovto RECT-2 
&Scoped-Define DISPLAYED-OBJECTS cbFiltroPeriodo fill-DataI fill-DataF ~
cbConta cbFiltroCC fill-item edNarrativa 

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
     DROP-DOWN-LIST
     SIZE 42 BY 1 TOOLTIP "Filtrar por conta bancária" NO-UNDO.

DEFINE VARIABLE cbFiltroCC AS CHARACTER FORMAT "X(256)":U 
     LABEL "Centro de custo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE cbFiltroPeriodo AS CHARACTER FORMAT "X(256)":U INITIAL "30 dias" 
     LABEL "Período" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "7 dias","15 dias","30 dias","60 dias","90 dias","Personalizar" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 TOOLTIP "Filtrar movimentação por período" NO-UNDO.

DEFINE VARIABLE edNarrativa AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 255 BY 3.67
     FONT 12 NO-UNDO.

DEFINE VARIABLE fill-DataF AS DATE FORMAT "99/99/9999":U 
     LABEL "Até" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Data final" NO-UNDO.

DEFINE VARIABLE fill-DataI AS DATE FORMAT "99/99/9999":U 
     LABEL "De" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Data inicial" NO-UNDO.

DEFINE VARIABLE fill-item AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 TOOLTIP "Filtrar pela descrição do item" NO-UNDO.

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
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 255 BY 24.29
         BGCOLOR 15 FGCOLOR 0 FONT 10 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbFiltroPeriodo AT ROW 1.91 COL 11 COLON-ALIGNED WIDGET-ID 6
     fill-DataI AT ROW 3.62 COL 11 COLON-ALIGNED WIDGET-ID 8
     fill-DataF AT ROW 3.62 COL 37 COLON-ALIGNED WIDGET-ID 10
     cbConta AT ROW 3.62 COL 66.2 COLON-ALIGNED WIDGET-ID 12
     cbFiltroCC AT ROW 3.62 COL 127 COLON-ALIGNED WIDGET-ID 30
     fill-item AT ROW 3.62 COL 169 COLON-ALIGNED WIDGET-ID 14
     btFiltrar AT ROW 1.81 COL 237.6 WIDGET-ID 18
     BtLimparFiltro AT ROW 3.48 COL 237.6 WIDGET-ID 20
     brMovto AT ROW 5.38 COL 1 WIDGET-ID 200
     edNarrativa AT ROW 29.81 COL 1 NO-LABEL WIDGET-ID 28
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
         TITLE              = "Movimentos"
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("icorealcontrole2.ico":U) THEN
    MESSAGE "Unable to load icon: icorealcontrole2.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brMovto BtLimparFiltro DEFAULT-FRAME */
ASSIGN 
       brMovto:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       brMovto:COLUMN-MOVABLE IN FRAME DEFAULT-FRAME         = TRUE
       brMovto:SEPARATOR-FGCOLOR IN FRAME DEFAULT-FRAME      = 15.

/* SETTINGS FOR EDITOR edNarrativa IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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
     _OrdList          = "rcdb.movto.data-mvto|yes,rcdb.movto.conta|yes,rcdb.movto.cc-cod|yes,rcdb.movto.it-cod|yes,rcdb.movto.seq|yes"
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
     _Query            is OPENED
*/  /* BROWSE brMovto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON end-error OF C-Win /* Movimentos */
or endkey of {&WINDOW-NAME} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON window-close OF C-Win /* Movimentos */
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
ON mouse-select-click OF brMovto IN FRAME DEFAULT-FRAME
do:
    if length(movto.narrativa) > 0 then 
        assign edNarrativa:screen-value in frame {&FRAME-NAME} = movto.narrativa.
    else 
        assign edNarrativa:screen-value in frame {&FRAME-NAME} = "".  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brMovto C-Win
ON mouse-select-dblclick OF brMovto IN FRAME DEFAULT-FRAME
do:
    
    run vwrs/lanc-edit(movto.id, 
                       rowid(item),
                       movto.data-mvto,
                       abs(movto.valor),
                       movto.movto-tp,
                       movto.descricao,
                       substitute("&1;&2;&3", movto.banco-cod,movto.ag, movto.conta),
                       movto.cc-cod,
                       movto.narrativa).
    
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


&Scoped-define SELF-NAME btFiltrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFiltrar C-Win
ON choose OF btFiltrar IN FRAME DEFAULT-FRAME /* Filtrar */
do:
    assign cbFiltroPeriodo cbFiltroCC cbConta fill-DataI fill-DataF fill-item.
    run prFiltrar.  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtLimparFiltro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtLimparFiltro C-Win
ON choose OF BtLimparFiltro IN FRAME DEFAULT-FRAME /* Limpar */
do:
    do with frame {&FRAME-NAME}:
        assign 
            cbFiltroPeriodo:screen-value = "30 dias"
            fill-dataI:screen-value      = ?
            fill-dataF:screen-value      = ?
            cbConta:screen-value         = "Todas"
            cbFiltroCC:screen-value      = "Todos"
            fill-item:screen-value       = ""
            fill-DataI:read-only         = true 
            fill-DataF:read-only         = true.
    end.
    apply "choose" to btFiltrar. 
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFiltroPeriodo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFiltroPeriodo C-Win
ON value-changed OF cbFiltroPeriodo IN FRAME DEFAULT-FRAME /* Período */
do:
    assign cbFiltroPeriodo
           fill-DataI:screen-value in frame {&frame-name} = ""
           fill-DataF:screen-value in frame {&frame-name} = "".
    
    if cbFiltroPeriodo = "Personalizar" then do: 
        assign fill-DataI:read-only = false
               fill-DataF:read-only = false.
        apply "entry" to fill-DataI.
    end.
    else   
        assign fill-DataI:read-only = true
               fill-DataF:read-only = true.
               
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
subscribe to "prUpdateBrowser" anywhere.

run prSetCombos.

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
     assign fill-DataI:read-only = true
            fill-DataF:read-only = true.
     apply "choose" to btFiltrar.
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
  DISPLAY cbFiltroPeriodo fill-DataI fill-DataF cbConta cbFiltroCC fill-item 
          edNarrativa 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cbFiltroPeriodo fill-DataI fill-DataF cbConta cbFiltroCC fill-item 
         btFiltrar BtLimparFiltro brMovto RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prFiltrar C-Win 
PROCEDURE prFiltrar :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    if cbFiltroPeriodo = "Personalizar" and fill-DataI = ? and fill-DataF = ? then do:
        run smr/dialog.w ("error", "Período obrigatário!" ,"Informe data inicial e final!").
        apply "entry" to fill-DataI in frame {&FRAME-NAME}.
        return no-apply.            
    end.
    
    assign 
        l-conta = (if cbConta <> "Todas" then true else false)
        l-cc    = (if cbFiltroCC <> "Todos" then true else false)
        l-geral = (if length(fill-item) > 0 then true else false).

    do with frame {&FRAME-NAME}:
        assign 
            h-query = brMovto:handle.
        if l-conta then 
            assign c-banco = entry(1, entry(lookup(cbConta:screen-value, cbConta:list-item-pairs), cbConta:list-item-pairs), ";")
                   c-ag    = entry(2, entry(lookup(cbConta:screen-value, cbConta:list-item-pairs), cbConta:list-item-pairs), ";")
                   c-cta   = entry(3, entry(lookup(cbConta:screen-value, cbConta:list-item-pairs), cbConta:list-item-pairs), ";").
        if l-cc then
            assign c-cc    = entry(lookup(cbFiltroCC:screen-value, cbFiltroCC:list-item-pairs), cbFiltroCC:list-item-pairs). 
    end.
    
    case cbFiltroPeriodo:
        when "7 dias"  then 
            assign d-dataI = today - 7
                   d-dataF = today + 5000.
        when "15 dias" then
            assign d-dataI = today - 15
                   d-dataF = today + 5000.
        when "30 dias" then
            assign d-dataI = today - 30
                   d-dataF = today + 5000.
        when "60 dias" then
            assign d-dataI = today - 60
                   d-dataF = today + 5000.
        when "90 dias" then
            assign d-dataI = today - 90
                   d-dataF = today + 5000.
        otherwise 
            assign d-dataI = fill-DataI
                   d-dataF = fill-DataF.
    end case. 
    
    run prFiltrarPorBrowserQueryHandle.
    // run prFiltrarPorPreprocessador. 
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prFiltrarPorBrowserQueryHandle C-Win 
PROCEDURE prFiltrarPorBrowserQueryHandle :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    /* --------------------------------------------------------------------------------
             SOLUCAO 1 - USANDO HANDLE DA QUERY DO BROWSER
    -------------------------------------------------------------------------------- */ 
    
    assign 
        c-query = "for each movto no-lock"
        c-order = "by movto.data-mvto indexed-reposition"
        c-query = substitute("&1 where movto.data-mvto >= date('&2') and movto.data-mvto <= date('&3'),", c-query, d-dataI, d-dataF)
        c-query = substitute("&1 each banco no-lock where banco.banco-cod = movto.banco-cod,", c-query).
    
    if l-cc then 
        assign c-query = substitute("&1 each ccusto no-lock where ccusto.cc-cod = movto.cc-cod and ccusto.cc-cod = '&2',", c-query, c-cc).
    else 
        assign c-query = substitute("&1 each ccusto no-lock where ccusto.cc-cod = movto.cc-cod,", c-query).

    if l-conta then
        assign c-query = substitute("&1 each conta no-lock where (conta.banco-cod = movto.banco-cod and conta.banco-cod = '&2') and (conta.ag = movto.ag and conta.ag = '&3') and (conta.conta = movto.conta and conta.conta = '&4'),", c-query, c-banco, c-ag, c-cta).
    else 
        assign c-query = substitute("&1 each conta no-lock where conta.banco-cod = movto.banco-cod and conta.ag = movto.ag and conta.conta = movto.conta,", c-query).
        
    if l-geral then 
        assign c-query = substitute("&1 each item no-lock where item.it-cod = movto.it-cod and item.descricao matches('*&2*')", c-query, fill-item).
    else 
        assign c-query = substitute("&1 each item no-lock where item.it-cod = movto.it-cod ", c-query).
    
    assign 
        c-query = c-query + c-order.
        
    /* https://community.progress.com/s/article/000049501 */
    define variable hQuery as handle no-undo.
    hQuery = query brMovto:handle. 
    hQuery:query-prepare(c-query).
    hQuery:query-open().

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prFiltrarPorPreprocessador C-Win 
PROCEDURE prFiltrarPorPreprocessador :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    &scoped-define re true

    /* --------------------------------------------------------------------------------
             SOLUCAO 2 - USANDO PREPROCESSORS
    -------------------------------------------------------------------------------- */
    
    &undefine OPEN-QUERY-brMovto
    &Scoped-define OPEN-QUERY-brMovto OPEN QUERY brMovto ~
    for each movto no-lock~
        where movto.data-mvto >= d-dataI ~
          and movto.data-mvto <= d-dataF, ~
    each banco no-lock ~
        where banco.banco-cod = movto.banco-cod, ~
    each ccusto no-lock ~
        where ccusto.cc-cod = movto.cc-cod, ~
    each conta no-lock ~
        where (conta.banco-cod = movto.banco-cod and (if l-conta then conta.banco-cod = c-banco else true)) ~
          and (conta.ag = movto.ag and (if l-conta then conta.ag = c-ag else true)) ~
          and (conta.conta = movto.conta and (if l-conta then conta.conta = c-cta else true)), ~
    each item no-lock ~
        where item.it-cod = movto.it-cod ~
          and (if l-geral then movto.descricao matches('*' + fill-item + '*') else true) ~
    by movto.data-mvto ~
    indexed-reposition.
    
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prSetCombos C-Win 
PROCEDURE prSetCombos :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable c-conta  as character no-undo.
    define variable c-ccusto as character no-undo.
    
    for each conta no-lock,
        first banco where banco.banco-cod = conta.banco-cod no-lock:
        c-conta = c-conta + "," + banco.descricao + " " + conta.ag + " / " + conta.conta + "," + banco.banco-cod + ";" + conta.ag + ";" + conta.conta.
    end.
    c-conta = "Todas as contas,Todas" + "," + trim(c-conta, ",").
    cbConta:list-item-pairs in frame {&FRAME-NAME} = c-conta.
    assign cbConta:screen-value in frame {&FRAME-NAME} = entry(lookup("Todas as contas", cbConta:list-item-pairs in frame {&FRAME-NAME}) + 1, c-conta).
    
    for each ccusto no-lock:
        c-ccusto = c-ccusto + "," + ccusto.cc-cod + " - " + ccusto.descricao + "," + ccusto.cc-cod.        
    end.
    c-ccusto = "Todos os centros de custo,Todos" + "," + trim(c-ccusto, ",").
    cbFiltroCC:list-item-pairs in frame {&FRAME-NAME} = c-ccusto.
    assign cbFiltroCC:screen-value in frame {&FRAME-NAME} = entry(lookup("Todos os centros de custo", cbFiltroCC:list-item-pairs in frame {&FRAME-NAME}) + 1, c-ccusto).
    
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prUpdateBrowser C-Win 
PROCEDURE prUpdateBrowser :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

