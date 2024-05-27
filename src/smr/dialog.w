&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME dialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

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

{src/adm2/widgetprto.i}
define input parameter cTipo        as character no-undo. //success, error, alert
define input parameter cTitulo      as character no-undo.
define input parameter cMensagem    as character no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 btOk btCancela 
&Scoped-Define DISPLAYED-OBJECTS filMsg filTitulo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancela AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btOk AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE filMsg AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 82 BY 4.76
     FONT 13 NO-UNDO.

DEFINE VARIABLE filTitulo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 73 BY 1.19
     FONT 12 NO-UNDO.

DEFINE IMAGE ico-alert
     FILENAME "Telas/circle-alert.ico":U
     SIZE 7 BY 1.67.

DEFINE IMAGE ico-error
     FILENAME "Telas/circle-error.ico":U
     SIZE 7 BY 1.67.

DEFINE IMAGE ico-success
     FILENAME "Telas/circle-check.ico":U
     SIZE 7 BY 1.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 2.14
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME dialog
     filMsg AT ROW 3.91 COL 2.6 NO-LABEL WIDGET-ID 4
     btOk AT ROW 9.52 COL 5.6
     btCancela AT ROW 9.52 COL 66.6
     filTitulo AT ROW 1.71 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     RECT-2 AT ROW 9 COL 2.6 WIDGET-ID 8
     ico-success AT ROW 1.76 COL 3 WIDGET-ID 14
     ico-alert AT ROW 1.76 COL 3 WIDGET-ID 16
     ico-error AT ROW 1.76 COL 3 WIDGET-ID 18
     SPACE(76.19) SKIP(7.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-HELP 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Info"
         DEFAULT-BUTTON btOk CANCEL-BUTTON btCancela WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME dialog:SCROLLABLE       = FALSE
       FRAME dialog:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR filMsg IN FRAME dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN filTitulo IN FRAME dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE ico-alert IN FRAME dialog
   NO-ENABLE                                                            */
ASSIGN 
       ico-alert:HIDDEN IN FRAME dialog           = TRUE.

/* SETTINGS FOR IMAGE ico-error IN FRAME dialog
   NO-ENABLE                                                            */
ASSIGN 
       ico-error:HIDDEN IN FRAME dialog           = TRUE.

/* SETTINGS FOR IMAGE ico-success IN FRAME dialog
   NO-ENABLE                                                            */
ASSIGN 
       ico-success:HIDDEN IN FRAME dialog           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX dialog
/* Query rebuild information for DIALOG-BOX dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dialog dialog
ON window-close OF FRAME dialog /* Info */
do:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  apply "END-ERROR":U to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dialog 


/* ***************************  Main Block  *************************** */
ico-success:hidden = true.
ico-error:hidden   = true.
ico-alert:hidden   = true.
case cTipo:
    when "success" then do:
        ico-success:visible = true.
        ico-error:visible   = false.
        ico-alert:visible   = false.
    end.
    when "error" then do:
        ico-success:visible = false.
        ico-error:visible   = true.
        ico-alert:visible   = false.
    end.
    when "alert" then do:
        ico-success:visible = false.
        ico-error:visible   = false.
        ico-alert:visible   = true.
    end.
    otherwise do:
        ico-success:visible = false.
        ico-error:visible   = false.
        ico-alert:visible   = false.
    end.
end case.

assign 
    filMsg    = cMensagem
    filTitulo = cTitulo.    

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI dialog  _DEFAULT-ENABLE
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
  DISPLAY filMsg filTitulo 
      WITH FRAME dialog.
  ENABLE RECT-2 btOk btCancela 
      WITH FRAME dialog.
  VIEW FRAME dialog.
  {&OPEN-BROWSERS-IN-QUERY-dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

