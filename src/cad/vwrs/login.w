&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS Procedure
USING src.cad.cls.LoginControl FROM PROPATH.
&ANALYZE-RESUME
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
ASSIGN 
    SESSION:DEBUG-ALERT         = YES
    LOG-MANAGER:LOGFILE-NAME    = 'E:\jobs\realcontrole\out\clientlog\clientlog.txt'
    LOG-MANAGER:LOGGING-LEVEL   = 4
    LOG-MANAGER:LOG-ENTRY-TYPES = "4GLMessages,4GLTrace,DB.Connects,DynObjects.DB,DynObjects.XML,DynObjects.Other,DynObjects.CLASS,DynObjects.UI,FileID,ProEvents.UI.CHAR,ProEvents.UI.COMMAND,ProEvents.Other,SAX".

/* Parameters Definitions ---                                           */
DEFINE VARIABLE controlador-login AS LoginControl NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-2 filUsuario filSenha btCancelar ~
btEntrar 
&Scoped-Define DISPLAYED-OBJECTS filUsuario filSenha 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancelar 
     LABEL "Cancelar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btEntrar 
     LABEL "Entrar" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE filSenha AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     FONT 9 NO-UNDO.

DEFINE VARIABLE filUsuario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     FONT 9 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "Telas/login.bmp":U
     SIZE 97 BY 24.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     filUsuario AT ROW 17.24 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     filSenha AT ROW 19.38 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 6 PASSWORD-FIELD 
     btCancelar AT ROW 22.71 COL 30 WIDGET-ID 8
     btEntrar AT ROW 22.71 COL 55 WIDGET-ID 10
     "Usuário" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 16.48 COL 29 WIDGET-ID 12
          FONT 9
     "Senha" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 18.67 COL 29 WIDGET-ID 14
          FONT 9
     IMAGE-2 AT ROW 1 COL 1 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97 BY 24.76 WIDGET-ID 100.


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
         HEIGHT             = 24.76
         WIDTH              = 97
         MAX-HEIGHT         = 30.52
         MAX-WIDTH          = 118.6
         VIRTUAL-HEIGHT     = 30.52
         VIRTUAL-WIDTH      = 118.6
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
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


&Scoped-define SELF-NAME btCancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancelar C-Win
ON CHOOSE OF btCancelar IN FRAME DEFAULT-FRAME /* Cancelar */
DO:
    APPLY "close":u TO THIS-PROCEDURE.
    
    FINALLY:
        IF VALID-OBJECT (controlador-login) THEN
            DELETE OBJECT controlador-login.    
    END FINALLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEntrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEntrar C-Win
ON CHOOSE OF btEntrar IN FRAME DEFAULT-FRAME /* Entrar */
DO:
    DEFINE VARIABLE l-exit AS LOGICAL NO-UNDO.
    DEFINE VARIABLE c-mess AS CHARACTER NO-UNDO.
    
    ASSIGN filUsuario filSenha.
    
    IF LENGTH(filUsuario) = 0 AND LENGTH(filSenha) > 0 THEN 
        ASSIGN l-exit = TRUE
               c-mess = "Usuário obrigatório!".
    ELSE IF LENGTH(filUsuario) > 0 AND LENGTH(filSenha) = 0 THEN 
        ASSIGN l-exit = TRUE
               c-mess = "Senha obrigatória!".
    ELSE IF LENGTH(filUsuario) = 0 AND LENGTH(filSenha) = 0 THEN 
        ASSIGN l-exit = TRUE
               c-mess = "Usuário e senha obrigatórios!".
        
    IF l-exit THEN DO:
        RUN smr/dialog.w ("error", "Erro de login!", c-mess).
        APPLY "entry" TO filUsuario.
        RETURN NO-APPLY.
    END.
        
    IF NOT VALID-OBJECT (controlador-login) THEN 
        ASSIGN controlador-login = NEW LoginControl().
    
    filSenha = ENCODE(filSenha).
    IF NOT controlador-login:logar(filUsuario, filSenha) THEN DO:
        
        RUN smr/dialog.w ("error", "Erro de login!", "Usuário ou senha inválidos!").
        
        APPLY "entry" TO filUsuario.
        RETURN NO-APPLY.
    END. 
    
    APPLY "close":u TO THIS-PROCEDURE.
    
    RUN cad/vwrs/main.w. 
    
    FINALLY:
        IF VALID-OBJECT (controlador-login) THEN
            DELETE OBJECT controlador-login.    
    END FINALLY.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME filSenha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filSenha C-Win
ON RETURN OF filSenha IN FRAME DEFAULT-FRAME
DO:
    APPLY "choose" TO btEntrar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    
    IF SESSION:DEBUG-ALERT = TRUE THEN DO:
        SESSION:DEBUG-ALERT = NO.
        LOG-MANAGER:CLOSE-LOG().
    END.
     
    RUN disable_UI.
END.

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
  DISPLAY filUsuario filSenha 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE IMAGE-2 filUsuario filSenha btCancelar btEntrar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

