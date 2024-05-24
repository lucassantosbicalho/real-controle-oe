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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttCtas NO-UNDO
    FIELD lbl       AS CHAR
    FIELD banco-cod AS CHAR 
    FIELD ag        AS CHAR 
    FIELD conta     AS CHAR 
    FIELD dCred     AS DECIMAL 
    FIELD dDeb      AS DECIMAL 
    FIELD dSaldo    AS DECIMAL.

DEFINE TEMP-TABLE ttMovto NO-UNDO
    LIKE movto.

DEFINE VARIABLE dCredT  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dDebT   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dSaldoT AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cLbl    AS CHARACTER NO-UNDO INIT "Total entre contas".

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
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fill-data-saldo AS DATE FORMAT "99/99/9999":U 
     LABEL "Calcular saldo até a data" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .95 TOOLTIP "Escolha a data final para cálculo do saldo das contas" NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "Telas/menu.bmp":U
     SIZE 133 BY 22.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 129 BY 15.24.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      ttCtas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  QUERY BROWSE-4 DISPLAY
      ttCtas.lbl COLUMN-LABEL "Conta bancária" FORMAT "x(50)" WIDTH 50 LABEL-FONT 6 
      ttCtas.dCred COLUMN-LABEL "Crédito" WIDTH 20 LABEL-FONT 6
      ttCtas.dDeb  COLUMN-LABEL "Débito"  WIDTH 20 LABEL-FONT 6
      ttCtas.dSaldo COLUMN-LABEL "Saldo"  WIDTH 20 LABEL-FONT 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 122 BY 12.67
         BGCOLOR 15 FONT 1 ROW-HEIGHT-CHARS .86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-4 AT ROW 6.86 COL 6.6 WIDGET-ID 200
     fill-data-saldo AT ROW 19.86 COL 110.6 COLON-ALIGNED WIDGET-ID 4
     "Saldo consolidado entre contas bancárias:" VIEW-AS TEXT
          SIZE 41 BY .62 AT ROW 5.62 COL 5.8 WIDGET-ID 8
     IMAGE-3 AT ROW 1 COL 1 WIDGET-ID 2
     RECT-3 AT ROW 6 COL 3.2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 133 BY 22.38 WIDGET-ID 100.


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
         TITLE              = "Real Controle - Gestão de finanças pessoais"
         HEIGHT             = 22.38
         WIDTH              = 133
         MAX-HEIGHT         = 28.86
         MAX-WIDTH          = 170
         VIRTUAL-HEIGHT     = 28.86
         VIRTUAL-WIDTH      = 170
         MAX-BUTTON         = NO
         RESIZE             = NO
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Telas/icorealcontrole2.ico":U) THEN
    MESSAGE "Unable to load icon: Telas/icorealcontrole2.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

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
ON END-ERROR OF C-Win /* Real Controle - Gestão de finanças pessoais */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Real Controle - Gestão de finanças pessoais */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 C-Win
ON ROW-DISPLAY OF BROWSE-4 IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE fg AS INTEGER NO-UNDO INIT 15.
    DEFINE VARIABLE bg AS INTEGER NO-UNDO INIT 0.
    DEFINE VARIABLE fn AS INTEGER NO-UNDO INIT 12.
    
    IF ttCtas.lbl = cLbl THEN DO:
        ASSIGN 
            ttCtas.lbl:fgcolor    IN BROWSE BROWSE-4 = bg
            ttCtas.lbl:bgcolor    IN BROWSE BROWSE-4 = fg
/*            ttCtas.lbl:font       in browse BROWSE-4 = fn*/
            ttCtas.dCred:fgcolor  IN BROWSE BROWSE-4 = bg
            ttCtas.dCred:bgcolor  IN BROWSE BROWSE-4 = fg
/*            ttCtas.dCred:font     in browse BROWSE-4 = fn*/
            ttCtas.dDeb:fgcolor   IN BROWSE BROWSE-4 = bg
            ttCtas.dDeb:bgcolor   IN BROWSE BROWSE-4 = fg
/*            ttCtas.dDeb:font      in browse BROWSE-4 = fn*/
            ttCtas.dSaldo:fgcolor IN BROWSE BROWSE-4 = bg
            ttCtas.dSaldo:bgcolor IN BROWSE BROWSE-4 = fg
/*            ttCtas.dSaldo:font    in browse BROWSE-4 = fn*/
        .
    END.
    
    IF ttCtas.dSaldo < 0 THEN
        ASSIGN
            ttCtas.dSaldo:fgcolor IN BROWSE BROWSE-4 = 12.
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 

/*
ON "CTRL-I" ANYWHERE DO:
    RUN smr/cad-item.w.
END.

ON "CTRL-B" ANYWHERE DO:
    RUN smr/cad-conta.w.
END.

ON "CTRL-C" ANYWHERE DO:
    RUN smr/cad-ccusto.w.
END.

ON "CTRL-L" ANYWHERE DO:
    RUN smr/lanc.w.
END.
*/
ASSIGN 
    fill-data-saldo = TODAY.
    
ON "return" OF fill-data-saldo DO:
    ASSIGN fill-data-saldo.
    RUN prCalculaSaldo.
END.

ON "tab" OF fill-data-saldo DO:
    ASSIGN fill-data-saldo.
    RUN prCalculaSaldo.
END.
    
RUN prCalculaSaldo.

{cad/include/menu.i}

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
       current-window:menu-bar       = menu mBarra:handle
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY fill-data-saldo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE IMAGE-3 RECT-3 fill-data-saldo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prCalculaSaldo C-Win 
PROCEDURE prCalculaSaldo :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------------
         Cria labels das contas unicas
-------------------------------------------------------------------------------- */ 
EMPTY TEMP-TABLE ttCtas.
CREATE ttCtas.
    ASSIGN 
        ttCtas.lbl       = cLbl
        ttCtas.banco-cod = "" 
        ttCtas.ag        = ""
        ttCtas.conta     = "".

FOR EACH conta NO-LOCK,
    EACH banco NO-LOCK
        WHERE banco.banco-cod = conta.banco-cod:
            
    CREATE ttCtas.
    ASSIGN 
        ttCtas.lbl       = SUBSTITUTE("   &1 &2 / &3", banco.descricao, conta.ag, conta.conta)
        ttCtas.banco-cod = conta.banco-cod 
        ttCtas.ag        = conta.ag
        ttCtas.conta     = conta.conta.
END.

/* --------------------------------------------------------------------------------
         Copia registros do mov para uma tt
-------------------------------------------------------------------------------- */ 
EMPTY TEMP-TABLE ttMovto.
FOR EACH movto
    WHERE movto.data-mvto < fill-data-saldo:
      //and movto.pendente  = true:
      
    CREATE ttMovto.
    BUFFER-COPY movto TO ttMovto.
END.

/* --------------------------------------------------------------------------------
         Calcula total cred e deb por conta e entre contas
-------------------------------------------------------------------------------- */ 
ASSIGN 
    dCredT  = 0
    dDebT   = 0.
    
FOR EACH ttMovto EXCLUSIVE-LOCK:
    FOR EACH ttCtas NO-LOCK
        WHERE ttCtas.banco-cod = ttMovto.banco-cod
          AND ttCtas.ag        = ttMovto.ag
          AND ttCtas.conta     = ttMovto.conta:
       
        IF ttMovto.valor >= 0 
        THEN
            ASSIGN
                ttCtas.dCred = ttCtas.dCred + ttMovto.valor 
                dCredT       = dCredT + ttMovto.valor.
        ELSE 
            ASSIGN 
                ttCtas.dDeb = ttCtas.dDeb + ttMovto.valor
                dDebT   = dDebT + ttMovto.valor.
                
    END.
END.

/* --------------------------------------------------------------------------------
         Calcula saldo total por conta e entre contas
-------------------------------------------------------------------------------- */ 
FOR EACH ttCtas EXCLUSIVE-LOCK:
    ASSIGN 
        ttCtas.dCred  = (IF ttCtas.lbl = cLbl THEN dCredT ELSE ttCtas.dCred)
        ttCtas.dDeb   = (IF ttCtas.lbl = cLbl THEN dDebT ELSE ttCtas.dDeb)
        ttCtas.dSaldo = (IF ttCtas.lbl = cLbl THEN dCredT + dDebT ELSE ttCtas.dCred + ttCtas.dDeb).
END.

{&OPEN-QUERY-BROWSE-4}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

