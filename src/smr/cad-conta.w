&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          rcdb             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME cad-conta

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS cad-conta 
using src.cls.ContaBancariaControl.
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
define variable controlador as ContaBancariaControl no-undo.
define variable cTitular    as character            no-undo.
define variable cBanco      as character            no-undo.    
define variable ico-dialog  as character            no-undo.

define buffer b-banco for banco.
define buffer b-titular for pessoa-fisica.

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS cad-conta 
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
&Scoped-define FRAME-NAME cad-conta
&Scoped-define BROWSE-NAME brConta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES conta banco pessoa-fisica

/* Definitions for BROWSE brConta                                       */
&Scoped-define FIELDS-IN-QUERY-brConta conta.ag conta.conta banco.descricao ~
pessoa-fisica.nome  + " "  + pessoa-fisica.sobrenome conta.tp-conta ~
conta.apelido 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brConta conta.tp-conta conta.apelido 
&Scoped-define ENABLED-TABLES-IN-QUERY-brConta conta
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brConta conta
&Scoped-define QUERY-STRING-brConta FOR EACH conta NO-LOCK, ~
      EACH banco WHERE banco.banco-cod = conta.banco-cod NO-LOCK, ~
      EACH pessoa-fisica WHERE pessoa-fisica.cpf = conta.titular-conta NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brConta OPEN QUERY brConta FOR EACH conta NO-LOCK, ~
      EACH banco WHERE banco.banco-cod = conta.banco-cod NO-LOCK, ~
      EACH pessoa-fisica WHERE pessoa-fisica.cpf = conta.titular-conta NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brConta conta banco pessoa-fisica
&Scoped-define FIRST-TABLE-IN-QUERY-brConta conta
&Scoped-define SECOND-TABLE-IN-QUERY-brConta banco
&Scoped-define THIRD-TABLE-IN-QUERY-brConta pessoa-fisica


/* Definitions for DIALOG-BOX cad-conta                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-cad-conta ~
    ~{&OPEN-QUERY-brConta}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-4 RECT-4 RECT-5 cb-banco fill-ag ~
fill-conta cb-titular tg-tpConta fill-apelido brConta Btn_Novo Btn_Cancelar ~
Btn_Excluir 
&Scoped-Define DISPLAYED-OBJECTS cb-banco fill-ag fill-conta cb-titular ~
tg-tpConta fill-apelido 

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

define variable cb-banco as character format "X(256)":U 
     label "Banco" 
     view-as combo-box inner-lines 10
     drop-down-list
     size 54 by 1 no-undo.

define variable cb-titular as character format "X(256)":U 
     label "Titular" 
     view-as combo-box inner-lines 5
     drop-down-list
     size 54.2 by 1 no-undo.

define variable fill-ag as character format "X(256)":U 
     label "Agência" 
     view-as fill-in 
     size 18 by 1 no-undo.

define variable fill-apelido as character format "X(40)":U 
     label "Apelido da conta" 
     view-as fill-in 
     size 54.4 by 1 no-undo.

define variable fill-conta as character format "X(256)":U 
     label "Conta" 
     view-as fill-in 
     size 27.8 by 1 no-undo.

define image IMAGE-4
     filename "images/telas.bmp":U
     size 101 by 19.05.

define variable tg-tpConta as character 
     view-as radio-set horizontal
     radio-buttons 
          "Conta corrente", "cc",
"Conta poupança", "cp"
     size 44 by .95 no-undo.

define rectangle RECT-4
     edge-pixels 2 graphic-edge  no-fill   
     size 98 by 15.43.

define rectangle RECT-5
     edge-pixels 2 graphic-edge  no-fill   
     size 98 by 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query brConta for 
      conta, 
      banco, 
      pessoa-fisica scrolling.
&ANALYZE-RESUME

/* Browse definitions                                                   */
define browse brConta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brConta cad-conta _STRUCTURED
  query brConta no-lock display
      conta.ag format "x(15)":U width 9
      conta.conta format "x(15)":U width 15
      banco.descricao column-label "Banco" format "x(20)":U width 16
      pessoa-fisica.nome  + " "  + pessoa-fisica.sobrenome column-label "Titular" format "x(40)":U
            width 20
      conta.tp-conta column-label "Tp" format "x(2)":U width 4
            column-fgcolor 9
      conta.apelido format "x(30)":U width 20 column-fgcolor 9
  ENABLE
      conta.tp-conta
      conta.apelido
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93.6 BY 8.1 FIT-LAST-COLUMN TOOLTIP "Campos com fonte azul permitem edição".


/* ************************  Frame Definitions  *********************** */

define frame cad-conta
     cb-banco at row 1.76 col 20 colon-aligned widget-id 18
     fill-ag at row 3.1 col 19.8 colon-aligned widget-id 2
     fill-conta at row 3.1 col 46.2 colon-aligned widget-id 24
     cb-titular at row 4.43 col 19.8 colon-aligned widget-id 26
     tg-tpConta at row 5.62 col 22 no-label widget-id 20
     fill-apelido at row 6.67 col 19.6 colon-aligned widget-id 4
     brConta at row 8 col 4.4 widget-id 200
     Btn_Novo at row 17.71 col 4.4 widget-id 6
     Btn_Cancelar at row 17.71 col 21.6
     Btn_Excluir at row 17.71 col 82.8 widget-id 8
     IMAGE-4 at row 1 col 1 widget-id 12
     RECT-4 at row 1.29 col 2.2 widget-id 14
     RECT-5 at row 17.14 col 2.2 widget-id 16
     space(1.80) skip(0.53)
    with view-as dialog-box keep-tab-order no-help 
         side-labels no-underline three-d  scrollable 
         title "Cadastrar conta"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB cad-conta 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX cad-conta
   FRAME-NAME                                                           */
/* BROWSE-TAB brConta fill-apelido cad-conta */
assign 
       frame cad-conta:SCROLLABLE       = false
       frame cad-conta:HIDDEN           = true.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brConta
/* Query rebuild information for BROWSE brConta
     _TblList          = "rcdb.conta,rcdb.banco WHERE rcdb.conta ...,rcdb.pessoa-fisica WHERE rcdb.conta ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ",,"
     _JoinCode[2]      = "banco.banco-cod = conta.banco-cod"
     _JoinCode[3]      = "pessoa-fisica.cpf = conta.titular-conta"
     _FldNameList[1]   > rcdb.conta.ag
"conta.ag" ? "x(15)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > rcdb.conta.conta
"conta.conta" ? "x(15)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > rcdb.banco.descricao
"banco.descricao" "Banco" ? "character" ? ? ? ? ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"pessoa-fisica.nome  + "" ""  + pessoa-fisica.sobrenome" "Titular" "x(40)" ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > rcdb.conta.tp-conta
"conta.tp-conta" "Tp" "x(2)" "character" ? 9 ? ? ? ? yes ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > rcdb.conta.apelido
"conta.apelido" ? "x(30)" "character" ? 9 ? ? ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brConta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX cad-conta
/* Query rebuild information for DIALOG-BOX cad-conta
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX cad-conta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME cad-conta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cad-conta cad-conta
on window-close of frame cad-conta /* Cadastrar conta */
do:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  apply "END-ERROR":U to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar cad-conta
on choose of Btn_Cancelar in frame cad-conta /* Cancelar */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Excluir cad-conta
on choose of Btn_Excluir in frame cad-conta /* Excluir */
do:
    
    message "Deseja excluir a conta selecionada?" skip "Ação irreversível!"
    view-as alert-box warning buttons yes-no update lChoice as logical .
    if lChoice then do:
        controlador = new ContaBancariaControl().
        controlador:excluir(conta.banco-cod,
                            conta.ag,
                            conta.conta).

        if controlador:cReturn begins "Erro" 
        then 
            ico-dialog = "error".
        else
            ico-dialog = "success".
                    
        {&OPEN-QUERY-brConta}
        
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Novo cad-conta
on choose of Btn_Novo in frame cad-conta /* OK */
do:
    
    if cb-banco:input-value = "" then do:
        ico-dialog = "error".
        run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Banco obrigatório!").
        apply "entry" to cb-banco.
        return no-apply.
    end.
    if fill-ag:input-value = "" then do:
        ico-dialog = "error".
        run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Agência obrigatória!").
        apply "entry" to fill-ag.
        return no-apply.
    end.
    if fill-conta:input-value = "" then do:
        ico-dialog = "error".
        run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Conta obrigatória!").
        apply "entry" to fill-conta.
        return no-apply.
    end.
    if cb-titular:input-value = "" then do:
        ico-dialog = "error".
        run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Titular da conta obrigatório!").
        apply "entry" to cb-titular.
        return no-apply.
    end.
    
    controlador = new ContaBancariaControl().
    controlador:cadastrar(cb-banco:input-value,
                          fill-ag:input-value,
                          fill-conta:input-value,
                          tg-tpConta:input-value,
                          cb-titular:input-value,
                          fill-apelido:input-value).

    {&OPEN-QUERY-brConta}

    if controlador:cReturn begins "Erro" then do:
        ico-dialog = "error".
        run smr/dialog.w (ico-dialog, controlador:cReturn, "").
    end.

    
    fill-ag:clear ().
    fill-conta:clear ().
    fill-apelido:clear ().
    tg-tpConta = "1".
    cb-titular = entry(2, cTitular).
    cb-banco = entry(2, cBanco).

    apply "entry" to cb-banco.
    return no-apply.
    
    finally:
        if valid-object (controlador) then
            delete object controlador.
    end finally.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brConta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK cad-conta 


/* ***************************  Main Block  *************************** */

for each b-banco no-lock:
    assign cBanco = cBanco + "," + b-banco.banco-cod + " - " + b-banco.descricao + "," + b-banco.banco-cod.
end.
cBanco = trim(cBanco, ",").
cb-banco:list-item-pairs = cBanco.
cb-banco = entry(2, cBanco).
for each b-titular no-lock:
   assign cTitular = cTitular + "," + b-titular.nome + " " + b-titular.sobrenome + "," + b-titular.cpf. 
end.
cTitular = trim(cTitular, ",").
cb-titular:list-item-pairs = cTitular.
cb-titular = entry(2, cTitular).

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects cad-conta  _ADM-CREATE-OBJECTS
procedure adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI cad-conta  _DEFAULT-DISABLE
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
  hide frame cad-conta.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI cad-conta  _DEFAULT-ENABLE
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
  display cb-banco fill-ag fill-conta cb-titular tg-tpConta fill-apelido 
      with frame cad-conta.
  enable IMAGE-4 RECT-4 RECT-5 cb-banco fill-ag fill-conta cb-titular 
         tg-tpConta fill-apelido brConta Btn_Novo Btn_Cancelar Btn_Excluir 
      with frame cad-conta.
  view frame cad-conta.
  {&OPEN-BROWSERS-IN-QUERY-cad-conta}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

