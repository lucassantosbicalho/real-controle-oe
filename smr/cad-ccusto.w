&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          rcdb             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME cad-ccusto

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS cad-ccusto 
using src.cad.cls.CentroDeCustoControl.
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
define variable controlador as CentroDeCustoControl no-undo.
define variable ico-dialog  as character            no-undo.    
    
{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS cad-ccusto 
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
&Scoped-define FRAME-NAME cad-ccusto
&Scoped-define BROWSE-NAME brCCusto

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ccusto

/* Definitions for BROWSE brCCusto                                      */
&Scoped-define FIELDS-IN-QUERY-brCCusto ccusto.cc-cod ccusto.descricao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCCusto ccusto.descricao 
&Scoped-define ENABLED-TABLES-IN-QUERY-brCCusto ccusto
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brCCusto ccusto
&Scoped-define QUERY-STRING-brCCusto FOR EACH ccusto NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brCCusto OPEN QUERY brCCusto FOR EACH ccusto NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brCCusto ccusto
&Scoped-define FIRST-TABLE-IN-QUERY-brCCusto ccusto


/* Definitions for DIALOG-BOX cad-ccusto                                */
&Scoped-define OPEN-BROWSERS-IN-QUERY-cad-ccusto ~
    ~{&OPEN-QUERY-brCCusto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-4 RECT-4 RECT-5 fill-cod-cc ~
fill-desc-cc brCCusto Btn_Novo Btn_Cancelar Btn_Excluir 
&Scoped-Define DISPLAYED-OBJECTS fill-cod-cc fill-desc-cc 

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

define variable fill-cod-cc as character format "X(256)":U 
     label "Código" 
     view-as fill-in 
     size 20 by 1 no-undo.

define variable fill-desc-cc as character format "X(30)":U 
     label "Descrição" 
     view-as fill-in 
     size 52 by 1 no-undo.

define image IMAGE-4
     filename "Telas/telas.bmp":U
     size 100 by 15.

define rectangle RECT-4
     edge-pixels 2 graphic-edge  no-fill   
     size 98 by 11.62.

define rectangle RECT-5
     edge-pixels 2 graphic-edge  no-fill   
     size 98 by 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query brCCusto for 
      ccusto scrolling.
&ANALYZE-RESUME

/* Browse definitions                                                   */
define browse brCCusto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCCusto cad-ccusto _STRUCTURED
  query brCCusto no-lock display
      ccusto.cc-cod format "x(8)":U width 15
      ccusto.descricao format "x(20)":U width 70
  ENABLE
      ccusto.descricao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93.6 BY 8.1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

define frame cad-ccusto
     fill-cod-cc at row 2.38 col 10.2 colon-aligned widget-id 2
     fill-desc-cc at row 2.38 col 44 colon-aligned widget-id 4
     brCCusto at row 4.29 col 4.4 widget-id 200
     Btn_Novo at row 13.81 col 4.4 widget-id 6
     Btn_Cancelar at row 13.81 col 21.6
     Btn_Excluir at row 13.81 col 82.8 widget-id 8
     IMAGE-4 at row 1 col 1 widget-id 12
     RECT-4 at row 1.29 col 2.2 widget-id 14
     RECT-5 at row 13.24 col 2.2 widget-id 16
     space(0.80) skip(0.38)
    with view-as dialog-box keep-tab-order no-help 
         side-labels no-underline three-d  scrollable 
         title "Cadastrar centro de custo"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB cad-ccusto 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX cad-ccusto
   FRAME-NAME                                                           */
/* BROWSE-TAB brCCusto fill-desc-cc cad-ccusto */
assign 
       frame cad-ccusto:SCROLLABLE       = false
       frame cad-ccusto:HIDDEN           = true.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCCusto
/* Query rebuild information for BROWSE brCCusto
     _TblList          = "rcdb.ccusto"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > rcdb.ccusto.cc-cod
"cc-cod" ? ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > rcdb.ccusto.descricao
"descricao" ? ? "character" ? ? ? ? ? ? yes ? no no "70" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brCCusto */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX cad-ccusto
/* Query rebuild information for DIALOG-BOX cad-ccusto
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX cad-ccusto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME cad-ccusto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cad-ccusto cad-ccusto
on window-close of frame cad-ccusto /* Cadastrar centro de custo */
do:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  apply "END-ERROR":U to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar cad-ccusto
on choose of Btn_Cancelar in frame cad-ccusto /* Cancelar */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Excluir cad-ccusto
on choose of Btn_Excluir in frame cad-ccusto /* Excluir */
do:
    
    message "Deseja excluir o centro de custo selecionado?" skip "Ação irreversível!"
    view-as alert-box warning buttons yes-no update lChoice as logical .
    if lChoice then do:
        controlador = new CentroDeCustoControl().
        controlador:excluir(ccusto.cc-cod).

        if controlador:cReturn begins "Erro" 
        then 
            ico-dialog = "error".
        else
            ico-dialog = "success".
                    
        {&OPEN-QUERY-brCCusto}
        
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Novo cad-ccusto
on choose of Btn_Novo in frame cad-ccusto /* OK */
do:
    
    if fill-cod-cc:input-value = "" then do:
        ico-dialog = "error".
        run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Campo Código obrigatório!").
        apply "entry" to fill-cod-cc.
        return no-apply.
    end. 
    
    controlador = new CentroDeCustoControl().
    controlador:cadastrar(fill-cod-cc:input-value, 
                          fill-desc-cc:input-value).

    {&OPEN-QUERY-brCCusto}
    
    if controlador:cReturn begins "Erro" then do:
        ico-dialog = "error".
        run smr/dialog.w (ico-dialog, controlador:cReturn, "").
    end.
    
    fill-cod-cc:clear ().
    fill-desc-cc:clear ().
    
    apply "entry" to fill-cod-cc.
    return no-apply.
    
    finally:
        if valid-object (controlador) then
            delete object controlador.
    end finally.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCCusto
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK cad-ccusto 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects cad-ccusto  _ADM-CREATE-OBJECTS
procedure adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI cad-ccusto  _DEFAULT-DISABLE
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
  hide frame cad-ccusto.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI cad-ccusto  _DEFAULT-ENABLE
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
  display fill-cod-cc fill-desc-cc 
      with frame cad-ccusto.
  enable IMAGE-4 RECT-4 RECT-5 fill-cod-cc fill-desc-cc brCCusto Btn_Novo 
         Btn_Cancelar Btn_Excluir 
      with frame cad-ccusto.
  view frame cad-ccusto.
  {&OPEN-BROWSERS-IN-QUERY-cad-ccusto}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

