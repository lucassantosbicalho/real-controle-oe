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
define button btCancela auto-end-key 
     label "Cancel" 
     size 15 by 1.14.

define button btOk auto-go 
     label "OK" 
     size 15 by 1.14.

define variable filMsg as character 
     view-as editor no-word-wrap scrollbar-horizontal scrollbar-vertical
     size 82 by 4.76
     font 13 no-undo.

define variable filTitulo as character format "X(256)":U 
      view-as text 
     size 73 by 1.19
     font 12 no-undo.

define image ico-alert
     filename "Telas/circle-alert.ico":U
     size 7 by 1.67.

define image ico-error
     filename "Telas/circle-error.ico":U
     size 7 by 1.67.

define image ico-success
     filename "Telas/circle-check.ico":U
     size 7 by 1.67.

define rectangle RECT-2
     edge-pixels 2 graphic-edge  no-fill   
     size 82 by 2.14
     bgcolor 15 .


/* ************************  Frame Definitions  *********************** */

define frame dialog
     filMsg at row 3.91 col 2.6 no-label widget-id 4
     btOk at row 9.52 col 5.6
     btCancela at row 9.52 col 66.6
     filTitulo at row 1.71 col 9 colon-aligned no-label widget-id 20
     RECT-2 at row 9 col 2.6 widget-id 8
     ico-success at row 1.76 col 3 widget-id 14
     ico-alert at row 1.76 col 3 widget-id 16
     ico-error at row 1.76 col 3 widget-id 18
     space(76.19) skip(7.99)
    with view-as dialog-box keep-tab-order no-help 
         side-labels no-underline three-d  scrollable 
         title "Info"
         default-button btOk cancel-button btCancela widget-id 100.


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
assign 
       frame dialog:SCROLLABLE       = false
       frame dialog:HIDDEN           = true.

/* SETTINGS FOR EDITOR filMsg IN FRAME dialog
   NO-ENABLE                                                            */
assign 
       filMsg:HIDDEN in frame dialog           = true
       filMsg:READ-ONLY in frame dialog        = true.

/* SETTINGS FOR FILL-IN filTitulo IN FRAME dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE ico-alert IN FRAME dialog
   NO-ENABLE                                                            */
assign 
       ico-alert:HIDDEN in frame dialog           = true.

/* SETTINGS FOR IMAGE ico-error IN FRAME dialog
   NO-ENABLE                                                            */
assign 
       ico-error:HIDDEN in frame dialog           = true.

/* SETTINGS FOR IMAGE ico-success IN FRAME dialog
   NO-ENABLE                                                            */
assign 
       ico-success:HIDDEN in frame dialog           = true.

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
on window-close of frame dialog /* Info */
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
procedure adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dialog  _DEFAULT-DISABLE
procedure disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  hide frame dialog.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI dialog  _DEFAULT-ENABLE
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
  display filMsg filTitulo 
      with frame dialog.
  enable RECT-2 btOk btCancela 
      with frame dialog.
  view frame dialog.
  {&OPEN-BROWSERS-IN-QUERY-dialog}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

