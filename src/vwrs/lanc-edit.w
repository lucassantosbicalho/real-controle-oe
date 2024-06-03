&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rcdb             PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
using src.cls.MovimentoControl from propath.
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
define buffer b-conta for conta.
define buffer b-banco for banco.
define buffer b-ccusto for ccusto.

define variable controlador         as MovimentoControl     no-undo.
define variable cCCusto             as character            no-undo.
define variable cBanco              as character            no-undo.    
define variable ico-dialog          as character            no-undo.

define input  parameter ip-id       as integer              no-undo.
define input  parameter ip-row-item as rowid                no-undo.
define input  parameter ip-data     as date                 no-undo.
define input  parameter ip-valor    as decimal              no-undo.
define input  parameter ip-op       as integer              no-undo.
define input  parameter ip-desc     as character            no-undo.
define input  parameter ip-cta      as character            no-undo.
define input  parameter ip-cc       as character            no-undo.
define input  parameter ip-narr     as character            no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brItem

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES item

/* Definitions for BROWSE brItem                                        */
&Scoped-define FIELDS-IN-QUERY-brItem item.descricao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brItem 
&Scoped-define QUERY-STRING-brItem FOR EACH item NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brItem OPEN QUERY brItem FOR EACH item NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brItem item
&Scoped-define FIRST-TABLE-IN-QUERY-brItem item


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brItem}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-6 brItem rsTpMovto fill-valor ~
fill-data-movto fill-descricao cb-ccusto cb-banco fill-narrativa BtnOK ~
BtnCancel BtnExcluir 
&Scoped-Define DISPLAYED-OBJECTS rsTpMovto fill-valor fill-data-movto ~
fill-descricao cb-ccusto cb-banco fill-narrativa 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
define var C-Win as widget-handle no-undo.

/* Definitions of the field level widgets                               */
define button BtnCancel auto-end-key default 
     label "Cancelar" 
     size 15 by 1.14 tooltip "Cancelar"
     bgcolor 8 .

define button BtnExcluir 
     label "Excluir" 
     size 15 by 1.14 tooltip "Excluir registro. Ação irreversível.".

define button BtnOK auto-go default 
     label "Alterar" 
     size 15 by 1.14 tooltip "Alterar"
     bgcolor 8 .

define variable cb-banco as character format "X(256)":U 
     label "Conta bancária" 
     view-as combo-box inner-lines 5
     list-item-pairs "Item 1","Item 1"
     drop-down-list
     size 72 by 1 no-undo.

define variable cb-ccusto as character format "X(256)":U 
     label "Centro de custo" 
     view-as combo-box inner-lines 5
     list-item-pairs "Item 1","Item 1"
     drop-down-list
     size 72 by 1 no-undo.

define variable fill-narrativa as character 
     view-as editor max-chars 300
     size 72 by 3.05 no-undo.

define variable fill-data-movto as date format "99/99/9999":U 
     label "Data" 
     view-as fill-in 
     size 18 by 1 no-undo.

define variable fill-descricao as character format "X(256)":U 
     label "Descrição" 
     view-as fill-in 
     size 72 by 1 no-undo.

define variable fill-valor as decimal format "->>,>>9.99":U initial 0 
     label "Valor" 
     view-as fill-in 
     size 18 by 1 no-undo.

define variable rsTpMovto as integer 
     view-as radio-set horizontal
     radio-buttons 
          "Crédito", 1,
"Débito", -1
     size 28.4 by 1.19 no-undo.

define rectangle RECT-4
     edge-pixels 2 graphic-edge  no-fill   
     size 98 by 18.

define rectangle RECT-6
     edge-pixels 2 graphic-edge  no-fill   
     size 95 by 6.52.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query brItem for 
      item scrolling.
&ANALYZE-RESUME

/* Browse definitions                                                   */
define browse brItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brItem C-Win _STRUCTURED
  query brItem no-lock display
      item.descricao format "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 5.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

define frame DEFAULT-FRAME
     brItem at row 3.38 col 5.6 widget-id 200
     rsTpMovto at row 9.86 col 69.6 no-label widget-id 32
     fill-valor at row 9.95 col 46.4 colon-aligned widget-id 30
     fill-data-movto at row 10 col 20.8 colon-aligned widget-id 18
     fill-descricao at row 11.33 col 20.8 colon-aligned widget-id 38
     cb-ccusto at row 12.71 col 20.8 colon-aligned widget-id 22
     cb-banco at row 14.14 col 20.8 colon-aligned widget-id 20
     fill-narrativa at row 15.57 col 22.8 no-label widget-id 36
     BtnOK at row 20.05 col 4 widget-id 44
     BtnCancel at row 20.05 col 20 widget-id 46
     BtnExcluir at row 20.05 col 84 widget-id 42
     " Item:" view-as text
          size 5.6 by .62 at row 2.33 col 7.6 widget-id 26
     RECT-4 at row 1.33 col 2.6 widget-id 40
     RECT-6 at row 2.67 col 4.2 widget-id 24
    with 1 down no-box keep-tab-order overlay 
         side-labels no-underline three-d 
         at col 1 row 1
         size 101 by 20.71
         default-button BtnOK cancel-button BtnCancel widget-id 100.


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
         title              = "Editar lançamento"
         height             = 20.71
         width              = 101
         max-height         = 24.33
         max-width          = 187.2
         virtual-height     = 24.33
         virtual-width      = 187.2
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
if not C-Win:load-icon("images/icorealcontrole2.ico":U) then
    message "Unable to load icon: images/icorealcontrole2.ico"
            view-as alert-box warning buttons ok.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brItem RECT-6 DEFAULT-FRAME */
if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
then C-Win:hidden = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brItem
/* Query rebuild information for BROWSE brItem
     _TblList          = "rcdb.item"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > rcdb.item.descricao
"item.descricao" ? "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brItem */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on end-error of C-Win /* Editar lançamento */
or endkey of {&WINDOW-NAME} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on window-close of C-Win /* Editar lançamento */
do:
  /* This event will close the window and terminate the procedure.  */
  apply "CLOSE":U to this-procedure.
  return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
on choose of BtnCancel in frame DEFAULT-FRAME /* Cancelar */
do:
    apply "close" to this-procedure.
    return no-apply.  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnExcluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnExcluir C-Win
on choose of BtnExcluir in frame DEFAULT-FRAME /* Excluir */
do:
    message "Deseja excluir o movimento?" skip "Ação irreversível!"
    view-as alert-box warning buttons yes-no update lChoice as logical .
    if lChoice then do:
        controlador = new MovimentoControl().
        controlador:excluir(ip-id).

        if controlador:cReturn begins "Erro" 
        then 
            ico-dialog = "error".
        else
            ico-dialog = "success".
                    
        run smr/dialog.w (ico-dialog, controlador:cReturn, "").
    end.
    
    publish "prUpdateBrowser".
    apply "choose" to BtnCancel.
    
    finally:
        if valid-object (controlador) then
            delete object controlador.
    end finally.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define BROWSE-NAME brItem
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
for each b-conta no-lock,
    each b-banco no-lock
        where b-banco.banco-cod = b-conta.banco-cod:
    assign cBanco = cBanco + "," + substitute("&1 &2 / &3", b-banco.descricao, b-conta.ag, b-conta.conta) + "," + substitute("&1;&2;&3", b-conta.banco-cod, b-conta.ag, b-conta.conta).
end.
cBanco = trim(cBanco, ",").
cb-banco:list-item-pairs = cBanco.

for each b-ccusto no-lock:
    assign cCCusto = cCCusto + "," + b-ccusto.cc-cod + " - " + b-ccusto.descricao + "," + b-ccusto.cc-cod.
end.
cCCusto = trim(cCCusto, ",").
cb-ccusto:list-item-pairs = cCCusto.

assign
    cb-banco        = ip-cta
    cb-ccusto       = ip-cc
    fill-data-movto = ip-data
    fill-valor      = ip-valor
    fill-descricao  = ip-desc
    fill-narrativa  = ip-narr
    rsTpMovto       = ip-op.

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
  
  reposition brItem to rowid ip-row-item.
  
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
  display rsTpMovto fill-valor fill-data-movto fill-descricao cb-ccusto cb-banco 
          fill-narrativa 
      with frame DEFAULT-FRAME in window C-Win.
  enable RECT-4 RECT-6 brItem rsTpMovto fill-valor fill-data-movto 
         fill-descricao cb-ccusto cb-banco fill-narrativa BtnOK BtnCancel 
         BtnExcluir 
      with frame DEFAULT-FRAME in window C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  view C-Win.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

