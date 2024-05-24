&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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
define temp-table ttCtas no-undo
    field lbl       as char
    field banco-cod as char 
    field ag        as char 
    field conta     as char 
    field dCred     as decimal 
    field dDeb      as decimal 
    field dSaldo    as decimal.

define temp-table ttMovto no-undo
    like movto.

define variable dCredT  as decimal   no-undo.
define variable dDebT   as decimal   no-undo.
define variable dSaldoT as decimal   no-undo.
define variable cLbl    as character no-undo init "Total entre contas".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCtas

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 ttCtas.lbl ttCtas.dCred ttCtas.dDeb ttCtas.dSaldo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH ttCtas
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH ttCtas.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 ttCtas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 ttCtas


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-3 RECT-3 fill-data-saldo 
&Scoped-Define DISPLAYED-OBJECTS fill-data-saldo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
define var C-Win as widget-handle no-undo.

/* Definitions of the field level widgets                               */
define variable fill-data-saldo as date format "99/99/9999":U 
     label "Calcular saldo até a data" 
     view-as fill-in 
     size 16 by .95 tooltip "Escolha a data final para cálculo do saldo das contas" no-undo.

define image IMAGE-3
     filename "Telas/menu.bmp":U
     size 133 by 22.38.

define rectangle RECT-3
     edge-pixels 2 graphic-edge  no-fill   
     size 129 by 15.24.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query BROWSE-4 for 
      ttCtas scrolling.
&ANALYZE-RESUME

/* Browse definitions                                                   */
define browse BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  query BROWSE-4 display
      ttCtas.lbl column-label "Conta bancária" format "x(50)" width 50 label-font 6 
      ttCtas.dCred column-label "Crédito" width 20 label-font 6
      ttCtas.dDeb  column-label "Débito"  width 20 label-font 6
      ttCtas.dSaldo column-label "Saldo"  width 20 label-font 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 122 BY 12.67
         BGCOLOR 15 FONT 1 ROW-HEIGHT-CHARS .86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

define frame DEFAULT-FRAME
     BROWSE-4 at row 6.86 col 6.6 widget-id 200
     fill-data-saldo at row 19.86 col 110.6 colon-aligned widget-id 4
     "Saldo consolidado entre contas bancárias:" view-as text
          size 41 by .62 at row 5.62 col 5.8 widget-id 8
     IMAGE-3 at row 1 col 1 widget-id 2
     RECT-3 at row 6 col 3.2 widget-id 6
    with 1 down no-box keep-tab-order overlay 
         side-labels no-underline three-d 
         at col 1.2 row 1
         size 133 by 22.38 widget-id 100.


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
         title              = "Real Controle - Gestão de finanças pessoais"
         height             = 22.38
         width              = 133
         max-height         = 28.86
         max-width          = 170
         virtual-height     = 28.86
         virtual-width      = 170
         max-button         = no
         resize             = no
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
if not C-Win:load-icon("Telas/icorealcontrole2.ico":U) then
    message "Unable to load icon: Telas/icorealcontrole2.ico"
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
/* BROWSE-TAB BROWSE-4 RECT-3 DEFAULT-FRAME */
/* SETTINGS FOR BROWSE BROWSE-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
then C-Win:hidden = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCtas.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on end-error of C-Win /* Real Controle - Gestão de finanças pessoais */
or endkey of {&WINDOW-NAME} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on window-close of C-Win /* Real Controle - Gestão de finanças pessoais */
do:
  /* This event will close the window and terminate the procedure.  */
  apply "CLOSE":U to this-procedure.
  return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 C-Win
on row-display of BROWSE-4 in frame DEFAULT-FRAME
do:
    define variable fg as integer no-undo init 15.
    define variable bg as integer no-undo init 0.
    define variable fn as integer no-undo init 12.
    
    if ttCtas.lbl = cLbl then do:
        assign 
            ttCtas.lbl:fgcolor    in browse BROWSE-4 = bg
            ttCtas.lbl:bgcolor    in browse BROWSE-4 = fg
/*            ttCtas.lbl:font       in browse BROWSE-4 = fn*/
            ttCtas.dCred:fgcolor  in browse BROWSE-4 = bg
            ttCtas.dCred:bgcolor  in browse BROWSE-4 = fg
/*            ttCtas.dCred:font     in browse BROWSE-4 = fn*/
            ttCtas.dDeb:fgcolor   in browse BROWSE-4 = bg
            ttCtas.dDeb:bgcolor   in browse BROWSE-4 = fg
/*            ttCtas.dDeb:font      in browse BROWSE-4 = fn*/
            ttCtas.dSaldo:fgcolor in browse BROWSE-4 = bg
            ttCtas.dSaldo:bgcolor in browse BROWSE-4 = fg
/*            ttCtas.dSaldo:font    in browse BROWSE-4 = fn*/
        .
    end.
    
    if ttCtas.dSaldo < 0 then
        assign
            ttCtas.dSaldo:fgcolor in browse BROWSE-4 = 12.
            
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 

subscribe to "prCalculaSaldo" anywhere.

assign 
    fill-data-saldo = today.
    
on return of fill-data-saldo do:
    assign fill-data-saldo.
    run prCalculaSaldo(fill-data-saldo).
end.

on tab of fill-data-saldo do:
    assign fill-data-saldo.
    run prCalculaSaldo(fill-data-saldo).
end.
    
run prCalculaSaldo(today).

{cad/include/menu.i}

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
assign CURRENT-WINDOW                = {&WINDOW-NAME}
       current-window:menu-bar       = menu mBarra:handle
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
  display fill-data-saldo 
      with frame DEFAULT-FRAME in window C-Win.
  enable IMAGE-3 RECT-3 fill-data-saldo 
      with frame DEFAULT-FRAME in window C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  view C-Win.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prCalculaSaldo C-Win 
procedure prCalculaSaldo :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
define input  parameter d-mov as date no-undo.
    message "source-procedure:file-name" source-procedure:file-name skip
            "target-procedure:file-name" target-procedure:file-name
    view-as alert-box.
    /* --------------------------------------------------------------------------------
             Cria labels das contas unicas
    -------------------------------------------------------------------------------- */ 
    empty temp-table ttCtas.
    create ttCtas.
        assign 
            ttCtas.lbl       = cLbl
            ttCtas.banco-cod = "" 
            ttCtas.ag        = ""
            ttCtas.conta     = "".
    
    for each conta no-lock,
        each banco no-lock
            where banco.banco-cod = conta.banco-cod:
                
        create ttCtas.
        assign 
            ttCtas.lbl       = substitute("   &1 &2 / &3", banco.descricao, conta.ag, conta.conta)
            ttCtas.banco-cod = conta.banco-cod 
            ttCtas.ag        = conta.ag
            ttCtas.conta     = conta.conta.
    end.
    
    /* --------------------------------------------------------------------------------
             Copia registros do mov para uma tt
    -------------------------------------------------------------------------------- */ 
    empty temp-table ttMovto.
    for each movto
        where movto.data-mvto <= d-mov:
          
        create ttMovto.
        buffer-copy movto to ttMovto.
    end.
    
    /* --------------------------------------------------------------------------------
             Calcula total cred e deb por conta e entre contas
    -------------------------------------------------------------------------------- */ 
    assign 
        dCredT  = 0
        dDebT   = 0.
        
    for each ttMovto exclusive-lock:
        for each ttCtas no-lock
            where ttCtas.banco-cod = ttMovto.banco-cod
              and ttCtas.ag        = ttMovto.ag
              and ttCtas.conta     = ttMovto.conta:
           
            if ttMovto.valor >= 0 
            then
                assign
                    ttCtas.dCred = ttCtas.dCred + ttMovto.valor 
                    dCredT       = dCredT + ttMovto.valor.
            else 
                assign 
                    ttCtas.dDeb = ttCtas.dDeb + ttMovto.valor
                    dDebT   = dDebT + ttMovto.valor.
                    
        end.
    end.
    
    /* --------------------------------------------------------------------------------
             Calcula saldo total por conta e entre contas
    -------------------------------------------------------------------------------- */ 
    for each ttCtas exclusive-lock:
        assign 
            ttCtas.dCred  = (if ttCtas.lbl = cLbl then dCredT else ttCtas.dCred)
            ttCtas.dDeb   = (if ttCtas.lbl = cLbl then dDebT else ttCtas.dDeb)
            ttCtas.dSaldo = (if ttCtas.lbl = cLbl then dCredT + dDebT else ttCtas.dCred + ttCtas.dDeb).
    end.

    {&OPEN-QUERY-BROWSE-4}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

