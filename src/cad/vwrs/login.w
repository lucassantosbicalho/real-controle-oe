&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS Procedure
using src.cad.cls.LoginControl from propath.
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

create widget-pool.

/* ***************************  Definitions  ************************** */
assign 
    session:debug-alert         = yes
    log-manager:logfile-name    = 'E:\jobs\realcontrole\out\clientlog\clientlog.txt'
    log-manager:logging-level   = 5
    log-manager:log-entry-types = "4GLMessages,4GLTrace,DB.Connects,DynObjects.DB,DynObjects.XML,DynObjects.Other,DynObjects.CLASS,DynObjects.UI,FileID,ProEvents.UI.CHAR,ProEvents.UI.COMMAND,ProEvents.Other,SAX".

/* Parameters Definitions ---                                           */
define variable controlador-login as LoginControl no-undo.

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
define var C-Win as widget-handle no-undo.

/* Definitions of the field level widgets                               */
define button btCancelar 
     label "Cancelar" 
     size 15 by 1.14.

define button btEntrar 
     label "Entrar" 
     size 15 by 1.14.

define variable filSenha as character format "X(256)":U 
     view-as fill-in 
     size 42 by 1
     font 9 no-undo.

define variable filUsuario as character format "X(256)":U 
     view-as fill-in 
     size 42 by 1
     font 9 no-undo.

define image IMAGE-2
     filename "images/login.bmp":U
     size 97 by 24.76.


/* ************************  Frame Definitions  *********************** */

define frame DEFAULT-FRAME
     filUsuario at row 17.24 col 27 colon-aligned no-label widget-id 4
     filSenha at row 19.38 col 27 colon-aligned no-label widget-id 6 password-field 
     btCancelar at row 22.71 col 30 widget-id 8
     btEntrar at row 22.71 col 55 widget-id 10
     "Usuário" view-as text
          size 12 by .62 at row 16.48 col 29 widget-id 12
          font 9
     "Senha" view-as text
          size 12 by .62 at row 18.67 col 29 widget-id 14
          font 9
     IMAGE-2 at row 1 col 1 widget-id 16
    with 1 down no-box keep-tab-order overlay 
         side-labels no-underline three-d 
         at col 1 row 1
         size 97 by 24.76 widget-id 100.


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
         height             = 24.76
         width              = 97
         max-height         = 30.52
         max-width          = 118.6
         virtual-height     = 30.52
         virtual-width      = 118.6
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
if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
then C-Win:hidden = no.

/* _RUN-TIME-ATTRIBUTES-END */
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


&Scoped-define SELF-NAME btCancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancelar C-Win
on choose of btCancelar in frame DEFAULT-FRAME /* Cancelar */
do:
    apply "close":u to this-procedure.
    
    finally:
        if valid-object (controlador-login) then
            delete object controlador-login.    
    end finally.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEntrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEntrar C-Win
on choose of btEntrar in frame DEFAULT-FRAME /* Entrar */
do:
    define variable l-exit as logical no-undo.
    define variable c-mess as character no-undo.
    
    assign filUsuario filSenha.
    
    if length(filUsuario) = 0 and LENGTH(filSenha) > 0 then 
        assign l-exit = true
               c-mess = "Usuário obrigatório!".
    else if length(filUsuario) > 0 and LENGTH(filSenha) = 0 then 
        assign l-exit = true
               c-mess = "Senha obrigatória!".
    else if length(filUsuario) = 0 and LENGTH(filSenha) = 0 then 
        assign l-exit = true
               c-mess = "Usuário e senha obrigatórios!".
        
    if l-exit then do:
        run smr/dialog.w ("error", "Erro de login!", c-mess).
        apply "entry" to filUsuario.
        return no-apply.
    end.
        
    if not valid-object (controlador-login) then 
        assign controlador-login = new LoginControl().
    
    filSenha = encode(filSenha).
    if not controlador-login:logar(filUsuario, filSenha) then do:
        
        run smr/dialog.w ("error", "Erro de login!", "Usuário ou senha inválidos!").
        
        apply "entry" to filUsuario.
        return no-apply.
    end. 
    
    apply "close":u to this-procedure.
    
    publish "prCalculaSaldo" (today).
    
    run cad/vwrs/main.w. 
    
    finally:
        if valid-object (controlador-login) then
            delete object controlador-login.    
    end finally.
     
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME filSenha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filSenha C-Win
on return of filSenha in frame DEFAULT-FRAME
do:
    apply "choose" to btEntrar.

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
on close of this-procedure do:
    
    if session:debug-alert = true then do:
        session:debug-alert = no.
        log-manager:close-log().
    end.
     
    run disable_UI.
end.

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
  display filUsuario filSenha 
      with frame DEFAULT-FRAME in window C-Win.
  enable IMAGE-2 filUsuario filSenha btCancelar btEntrar 
      with frame DEFAULT-FRAME in window C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  view C-Win.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

