&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          rcdb             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME cad-item

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS cad-item 
using src.cls.ItemControl.
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
define variable controlador as ItemControl no-undo.
define variable ico-dialog  as character   no-undo.    

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
define button Btn_Cancelar auto-go 
     label "Cancelar" 
     size 15 by 1.14.

define button Btn_Excluir 
     label "Excluir" 
     size 15 by 1.14.

define button Btn_Novo auto-go 
     label "OK" 
     size 15 by 1.14.

define variable fill-cod-it as character format "X(256)":U 
     label "Código" 
     view-as fill-in 
     size 20 by 1 no-undo.

define variable fill-desc-it as character format "X(30)":U 
     label "Descrição" 
     view-as fill-in 
     size 52 by 1 no-undo.

define image IMAGE-4
     filename "images/telas.bmp":U
     size 100 by 15.

define rectangle RECT-4
     edge-pixels 2 graphic-edge  no-fill   
     size 98 by 11.62.

define rectangle RECT-5
     edge-pixels 2 graphic-edge  no-fill   
     size 98 by 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query brItem for 
      item scrolling.
&ANALYZE-RESUME

/* Browse definitions                                                   */
define browse brItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brItem cad-item _STRUCTURED
  query brItem no-lock display
      item.it-cod format "x(35)":U width 16
      item.descricao format "x(30)":U width 73.4
  ENABLE
      item.descricao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93.6 BY 8.1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

define frame cad-item
     fill-cod-it at row 2.38 col 10.2 colon-aligned widget-id 2
     fill-desc-it at row 2.38 col 44 colon-aligned widget-id 4
     brItem at row 4.29 col 4.4 widget-id 200
     Btn_Novo at row 13.81 col 4.4 widget-id 6
     Btn_Cancelar at row 13.81 col 21.6
     Btn_Excluir at row 13.81 col 82.8 widget-id 8
     IMAGE-4 at row 1 col 1 widget-id 12
     RECT-4 at row 1.29 col 2.2 widget-id 14
     RECT-5 at row 13.24 col 2.2 widget-id 16
     space(0.80) skip(0.38)
    with view-as dialog-box keep-tab-order no-help 
         side-labels no-underline three-d  scrollable 
         title "Cadastrar item"
         default-button Btn_Cancelar widget-id 100.


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
assign 
       frame cad-item:SCROLLABLE       = false
       frame cad-item:HIDDEN           = true.

assign 
       brItem:ALLOW-COLUMN-SEARCHING in frame cad-item = true
       brItem:COLUMN-RESIZABLE in frame cad-item       = true.

assign 
       item.it-cod:AUTO-RESIZE in browse brItem = true.

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
on window-close of frame cad-item /* Cadastrar item */
do:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  apply "END-ERROR":U to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brItem



&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar cad-item
on choose of Btn_Cancelar in frame cad-item /* Cancelar */
do:
    
    apply "close" to this-procedure.
    
    finally:
        if valid-object (controlador) then
            delete object controlador.
    end finally.   
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Excluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Excluir cad-item
on choose of Btn_Excluir in frame cad-item /* Excluir */
do:
    
    message "Deseja excluir o item selecionado?" skip "Ação irreversível!"
    view-as alert-box warning buttons yes-no update lChoice as logical .
    if lChoice then do:
        controlador = new ItemControl().
        controlador:excluir(item.it-cod).

        if controlador:cReturn begins "Erro" 
        then 
            ico-dialog = "error".
        else
            ico-dialog = "success".
                    
        {&OPEN-QUERY-brItem}
        
        run smr/dialog.w (ico-dialog, controlador:cReturn, "").
        
    end.
    
    finally:
        if valid-object (controlador) then
            delete object controlador.
    end finally.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Novo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Novo cad-item
on choose of Btn_Novo in frame cad-item /* OK */
do:
    
    if fill-cod-it:input-value = "" then do:
        ico-dialog = "error".
        run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Campo Código obrigatório!").
        
        apply "entry" to fill-cod-it.
        return no-apply.
    end. 
    
    controlador = new ItemControl().
    controlador:cadastrar(fill-cod-it:input-value, 
                          fill-desc-it:input-value).

    {&OPEN-QUERY-brItem}

    if controlador:cReturn begins "Erro" then do:
        ico-dialog = "error".
        run smr/dialog.w (ico-dialog, controlador:cReturn, "").
    end.
    
    fill-cod-it:clear ().
    fill-desc-it:clear ().
    
    apply "entry" to fill-cod-it.
    return no-apply.
    
    finally:
        if valid-object (controlador) then
            delete object controlador.
    end finally.
end.

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
procedure adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI cad-item  _DEFAULT-DISABLE
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
  hide frame cad-item.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI cad-item  _DEFAULT-ENABLE
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
  display fill-cod-it fill-desc-it 
      with frame cad-item.
  enable IMAGE-4 RECT-4 RECT-5 fill-cod-it fill-desc-it brItem Btn_Novo 
         Btn_Cancelar Btn_Excluir 
      with frame cad-item.
  view frame cad-item.
  {&OPEN-BROWSERS-IN-QUERY-cad-item}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

