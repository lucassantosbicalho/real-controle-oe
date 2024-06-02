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
define var C-Win as widget-handle no-undo.

/* Definitions of the field level widgets                               */
define button btFiltrar 
     label "Filtrar" 
     size 15 by 1.14.

define button BtLimparFiltro 
     label "Limpar" 
     size 15 by 1.14.

define variable cbConta as character format "X(256)":U 
     label "Conta" 
     view-as combo-box inner-lines 5
     drop-down-list
     size 42 by 1 tooltip "Filtrar por conta bancária" no-undo.

define variable cbFiltroCC as character format "X(256)":U 
     label "Centro de custo" 
     view-as combo-box inner-lines 5
     drop-down-list
     size 35 by 1 no-undo.

define variable cbFiltroPeriodo as character format "X(256)":U initial "30 dias" 
     label "Período" 
     view-as combo-box inner-lines 6
     list-items "7 dias","15 dias","30 dias","60 dias","90 dias","Personalizar" 
     drop-down-list
     size 20 by 1 tooltip "Filtrar movimentação por período" no-undo.

define variable edNarrativa as character 
     view-as editor no-word-wrap scrollbar-horizontal scrollbar-vertical
     size 255 by 3.67
     font 12 no-undo.

define variable fill-DataF as date format "99/99/9999":U 
     label "Até" 
     view-as fill-in 
     size 20 by 1 tooltip "Data final" no-undo.

define variable fill-DataI as date format "99/99/9999":U 
     label "De" 
     view-as fill-in 
     size 20 by 1 tooltip "Data inicial" no-undo.

define variable fill-item as character format "X(256)":U 
     label "Item" 
     view-as fill-in 
     size 64 by 1 tooltip "Filtrar pela descrição do item" no-undo.

define rectangle RECT-2
     edge-pixels 2 graphic-edge  no-fill   
     size 254.6 by 4.05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query brMovto for 
      movto, 
      banco, 
      ccusto, 
      conta, 
      item scrolling.
&ANALYZE-RESUME

/* Browse definitions                                                   */
define browse brMovto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brMovto C-Win _STRUCTURED
  query brMovto no-lock display
      movto.id column-label "ID" format "->,>>>,>>9":U
      movto.data-mvto format "99/99/9999":U
      banco.descricao + " " + conta.ag + " / " + conta.conta column-label "Conta bancária" format "x(40)":U
      ccusto.cc-cod + " - " + ccusto.descricao column-label "Centro de custo" format "x(30)":U
            width 25
      item.descricao column-label "Item" format "x(30)":U
      movto.seq format ">,>>>,>>9":U width 10
      movto.valor format "->,>>>,>>9.99":U width 20
      movto.usuario format "x(8)":U
      movto.descricao format "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 255 BY 24.29
         BGCOLOR 15 FGCOLOR 0 FONT 10 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

define frame DEFAULT-FRAME
     cbFiltroPeriodo at row 1.91 col 11 colon-aligned widget-id 6
     fill-DataI at row 3.62 col 11 colon-aligned widget-id 8
     fill-DataF at row 3.62 col 37 colon-aligned widget-id 10
     cbConta at row 3.62 col 66.2 colon-aligned widget-id 12
     cbFiltroCC at row 3.62 col 127 colon-aligned widget-id 30
     fill-item at row 3.62 col 169 colon-aligned widget-id 14
     btFiltrar at row 1.81 col 237.6 widget-id 18
     BtLimparFiltro at row 3.48 col 237.6 widget-id 20
     brMovto at row 5.38 col 1 widget-id 200
     edNarrativa at row 29.81 col 1 no-label widget-id 28
     RECT-2 at row 1.19 col 1.8 widget-id 4
    with 1 down no-box keep-tab-order overlay 
         side-labels no-underline three-d 
         at col 1 row 1
         size 255.4 by 32.48 widget-id 100.


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
if session:display-type = "GUI":U then
  create window C-Win assign
         hidden             = yes
         title              = "Movimentos"
         height             = 32.48
         width              = 255.4
         max-height         = 32.48
         max-width          = 271
         virtual-height     = 32.48
         virtual-width      = 271
         resize             = yes
         scroll-bars        = no
         status-area        = no
         bgcolor            = ?
         fgcolor            = ?
         keep-frame-z-order = yes
         three-d            = yes
         message-area       = no
         sensitive          = yes.
else {&WINDOW-NAME} = current-window.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
if not C-Win:load-icon("icorealcontrole2.ico":U) then
    message "Unable to load icon: icorealcontrole2.ico"
            view-as alert-box warning buttons ok.
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
assign 
       brMovto:COLUMN-RESIZABLE in frame DEFAULT-FRAME       = true
       brMovto:COLUMN-MOVABLE in frame DEFAULT-FRAME         = true
       brMovto:SEPARATOR-FGCOLOR in frame DEFAULT-FRAME      = 15.

/* SETTINGS FOR EDITOR edNarrativa IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
then C-Win:hidden = no.

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
     _Query            is OPENED
*/  /* BROWSE brMovto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on end-error of C-Win /* Movimentos */
or endkey of {&WINDOW-NAME} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on window-close of C-Win /* Movimentos */
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
on mouse-select-click of brMovto in frame DEFAULT-FRAME
do:
    if length(movto.narrativa) > 0 then 
        assign edNarrativa:screen-value in frame {&FRAME-NAME} = movto.narrativa.
    else 
        assign edNarrativa:screen-value in frame {&FRAME-NAME} = "".  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brMovto C-Win
on mouse-select-dblclick of brMovto in frame DEFAULT-FRAME
do:
    
    message "double click" movto.id movto.it-cod
    view-as alert-box.  
    
    run vwrs/lanc-edit.
    
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brMovto C-Win
on row-display of brMovto in frame DEFAULT-FRAME
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
on choose of btFiltrar in frame DEFAULT-FRAME /* Filtrar */
do:
    assign cbFiltroPeriodo cbFiltroCC cbConta fill-DataI fill-DataF fill-item.
    run prFiltrar.  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtLimparFiltro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtLimparFiltro C-Win
on choose of BtLimparFiltro in frame DEFAULT-FRAME /* Limpar */
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
on value-changed of cbFiltroPeriodo in frame DEFAULT-FRAME /* Período */
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
procedure disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
  then delete widget C-Win.
  if this-procedure:persistent then delete procedure this-procedure.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
procedure enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  display cbFiltroPeriodo fill-DataI fill-DataF cbConta cbFiltroCC fill-item 
          edNarrativa 
      with frame DEFAULT-FRAME in window C-Win.
  enable cbFiltroPeriodo fill-DataI fill-DataF cbConta cbFiltroCC fill-item 
         btFiltrar BtLimparFiltro brMovto RECT-2 
      with frame DEFAULT-FRAME in window C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  view C-Win.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prFiltrar C-Win 
procedure prFiltrar :
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
                   d-dataF = today.
        when "15 dias" then
            assign d-dataI = today - 15
                   d-dataF = today.
        when "30 dias" then
            assign d-dataI = today - 30
                   d-dataF = today.
        when "60 dias" then
            assign d-dataI = today - 60
                   d-dataF = today.
        when "90 dias" then
            assign d-dataI = today - 90
                   d-dataF = today.
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
procedure prFiltrarPorBrowserQueryHandle :
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
procedure prFiltrarPorPreprocessador :
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
procedure prSetCombos :
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

