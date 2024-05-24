&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          rcdb             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME cad-item

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS cad-item 
USING src.cad.cls.ItemControl.
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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE controlador AS ItemControl NO-UNDO.
DEFINE VARIABLE ico-dialog  AS CHARACTER   NO-UNDO.    

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS cad-item 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 09/26/23 - 12:33 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME cad-item
&Scoped-define BROWSE-NAME brItem

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES item

/* Definitions for BROWSE brItem                                        */
&Scoped-define FIELDS-IN-QUERY-brItem item.it-cod item.descricao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brItem item.descricao 
&Scoped-define ENABLED-TABLES-IN-QUERY-brItem item
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brItem item
&Scoped-define QUERY-STRING-brItem FOR EACH item NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brItem OPEN QUERY brItem FOR EACH item NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brItem item
&Scoped-define FIRST-TABLE-IN-QUERY-brItem item


/* Definitions for DIALOG-BOX cad-item                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-cad-item ~
    ~{&OPEN-QUERY-brItem}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-4 RECT-4 RECT-5 fill-cod-it ~
fill-desc-it brItem Btn_Novo Btn_Cancelar Btn_Excluir 
&Scoped-Define DISPLAYED-OBJECTS fill-cod-it fill-desc-it 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancelar AUTO-GO 
     LABEL "Cancelar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Excluir 
     LABEL "Excluir" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Novo AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fill-cod-it AS CHARACTER FORMAT "X(256)":U 
     LABEL "Código" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fill-desc-it AS CHARACTER FORMAT "X(30)":U 
     LABEL "Descrição" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "Telas/telas.bmp":U
     SIZE 100 BY 15.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 11.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brItem FOR 
      item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brItem cad-item _STRUCTURED
  QUERY brItem NO-LOCK DISPLAY
      item.it-cod FORMAT "x(35)":U WIDTH 16
      item.descricao FORMAT "x(30)":U WIDTH 73.4
  ENABLE
      item.descricao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93.6 BY 8.1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME cad-item
     fill-cod-it AT ROW 2.38 COL 10.2 COLON-ALIGNED WIDGET-ID 2
     fill-desc-it AT ROW 2.38 COL 44 COLON-ALIGNED WIDGET-ID 4
     brItem AT ROW 4.29 COL 4.4 WIDGET-ID 200
     Btn_Novo AT ROW 13.81 COL 4.4 WIDGET-ID 6
     Btn_Cancelar AT ROW 13.81 COL 21.6
     Btn_Excluir AT ROW 13.81 COL 82.8 WIDGET-ID 8
     IMAGE-4 AT ROW 1 COL 1 WIDGET-ID 12
     RECT-4 AT ROW 1.29 COL 2.2 WIDGET-ID 14
     RECT-5 AT ROW 13.24 COL 2.2 WIDGET-ID 16
     SPACE(0.80) SKIP(0.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-HELP 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Cadastrar item"
         DEFAULT-BUTTON Btn_Cancelar WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB cad-item 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX cad-item
   FRAME-NAME                                                           */
/* BROWSE-TAB brItem fill-desc-it cad-item */
ASSIGN 
       FRAME cad-item:SCROLLABLE       = FALSE
       FRAME cad-item:HIDDEN           = TRUE.

ASSIGN 
       brItem:ALLOW-COLUMN-SEARCHING IN FRAME cad-item = TRUE
       brItem:COLUMN-RESIZABLE IN FRAME cad-item       = TRUE.

ASSIGN 
       item.it-cod:AUTO-RESIZE IN BROWSE brItem = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brItem
/* Query rebuild information for BROWSE brItem
     _TblList          = "rcdb.item"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > rcdb.item.it-cod
"item.it-cod" ? "x(35)" "character" ? ? ? ? ? ? no ? no no "16" yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > rcdb.item.descricao
"item.descricao" ? "x(30)" "character" ? ? ? ? ? ? yes ? no no "73.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brItem */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX cad-item
/* Query rebuild information for DIALOG-BOX cad-item
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX cad-item */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME cad-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cad-item cad-item
ON WINDOW-CLOSE OF FRAME cad-item /* Cadastrar item */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brItem



&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar cad-item
ON CHOOSE OF Btn_Cancelar IN FRAME cad-item /* Cancelar */
DO:
    
    APPLY "close" TO THIS-PROCEDURE.
    
    FINALLY:
        IF VALID-OBJECT (controlador) THEN
            DELETE OBJECT controlador.
    END FINALLY.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Excluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Excluir cad-item
ON CHOOSE OF Btn_Excluir IN FRAME cad-item /* Excluir */
DO:
    
    MESSAGE "Deseja excluir o item selecionado?" SKIP "Ação irreversível!"
    VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE lChoice AS LOGICAL .
    IF lChoice THEN DO:
        controlador = NEW ItemControl().
        controlador:excluir(item.it-cod).

        IF controlador:cReturn BEGINS "Erro" 
        THEN 
            ico-dialog = "error".
        ELSE
            ico-dialog = "success".
                    
        {&OPEN-QUERY-brItem}
        
        RUN smr/dialog.w (ico-dialog, controlador:cReturn, "").
        
    END.
    
    FINALLY:
        IF VALID-OBJECT (controlador) THEN
            DELETE OBJECT controlador.
    END FINALLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Novo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Novo cad-item
ON CHOOSE OF Btn_Novo IN FRAME cad-item /* OK */
DO:
    
    IF fill-cod-it:input-value = "" THEN DO:
        ico-dialog = "error".
        RUN smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Campo Código obrigatório!").
        
        APPLY "entry" TO fill-cod-it.
        RETURN NO-APPLY.
    END. 
    
    controlador = NEW ItemControl().
    controlador:cadastrar(fill-cod-it:input-value, 
                          fill-desc-it:input-value).

    {&OPEN-QUERY-brItem}

    IF controlador:cReturn BEGINS "Erro" THEN DO:
        ico-dialog = "error".
        RUN smr/dialog.w (ico-dialog, controlador:cReturn, "").
    END.
    
    fill-cod-it:clear ().
    fill-desc-it:clear ().
    
    APPLY "entry" TO fill-cod-it.
    RETURN NO-APPLY.
    
    FINALLY:
        IF VALID-OBJECT (controlador) THEN
            DELETE OBJECT controlador.
    END FINALLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK cad-item 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects cad-item  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI cad-item  _DEFAULT-DISABLE
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
  HIDE FRAME cad-item.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI cad-item  _DEFAULT-ENABLE
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
  DISPLAY fill-cod-it fill-desc-it 
      WITH FRAME cad-item.
  ENABLE IMAGE-4 RECT-4 RECT-5 fill-cod-it fill-desc-it brItem Btn_Novo 
         Btn_Cancelar Btn_Excluir 
      WITH FRAME cad-item.
  VIEW FRAME cad-item.
  {&OPEN-BROWSERS-IN-QUERY-cad-item}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

