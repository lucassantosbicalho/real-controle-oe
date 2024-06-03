&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          rcdb             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME lancamento

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS lancamento 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS Procedure
using src.cls.MovimentoControl from propath.
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
define variable controlador as MovimentoControl     no-undo.
define variable cCCusto     as character            no-undo.
define variable cBanco      as character            no-undo.    
define variable ico-dialog  as character            no-undo.

define buffer b-conta for conta.
define buffer b-banco for banco.
define buffer b-ccusto for ccusto.

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS lancamento 
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
&Scoped-define FRAME-NAME lancamento
&Scoped-define BROWSE-NAME brItem

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES item

/* Definitions for BROWSE brItem                                        */
&Scoped-define FIELDS-IN-QUERY-brItem item.descricao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brItem 
&Scoped-define QUERY-STRING-brItem FOR EACH item NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brItem OPEN QUERY brItem FOR EACH item NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brItem item
&Scoped-define FIRST-TABLE-IN-QUERY-brItem item


/* Definitions for DIALOG-BOX lancamento                                */
&Scoped-define OPEN-BROWSERS-IN-QUERY-lancamento ~
    ~{&OPEN-QUERY-brItem}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brItem fill-data-movto fill-valor rsTpMovto ~
fill-descricao cb-ccusto cb-banco fill-narrativa l-parcelar rs-parcelar ~
fill-qtd-parc-repl fill-interval-dias Btn_Novo Btn_OK_Novo Btn_Cancelar ~
IMAGE-4 RECT-4 RECT-5 RECT-6 RECT-7 RECT-11 
&Scoped-Define DISPLAYED-OBJECTS fill-data-movto fill-valor rsTpMovto ~
fill-descricao cb-ccusto cb-banco fill-narrativa l-parcelar 

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

define button Btn_Novo auto-go 
     label "OK" 
     size 15 by 1.14 tooltip "Salvar e fechar".

define button Btn_OK_Novo 
     label "Ok e novo" 
     size 15 by 1.14 tooltip "Salvar e realizar novo lançamento".

define variable cb-banco as character format "X(256)":U 
     label "Conta bancária" 
     view-as combo-box inner-lines 5
     list-item-pairs "Item 1","Item 1"
     drop-down-list
     size 72 by 1 no-undo.

define variable cb-ccusto as character format "X(256)":U 
     label "Centro de custo" 
     view-as combo-box inner-lines 5
     list-item-pairs "Item 1","Item 1"
     drop-down-list
     size 72 by 1 no-undo.

define variable fill-narrativa as character 
     view-as editor max-chars 300
     size 72 by 3.05 no-undo.

define variable fill-data-movto as date format "99/99/9999":U 
     label "Data" 
     view-as fill-in 
     size 18 by 1 no-undo.

define variable fill-descricao as character format "X(256)":U 
     label "Descrição" 
     view-as fill-in 
     size 72 by 1 no-undo.

define variable fill-interval-dias as integer format ">>9":U initial 0 
     label "Intervalo entre parcelas (dias)" 
     view-as fill-in 
     size 14 by 1 no-undo.

define variable fill-qtd-parc-repl as integer format ">>9":U initial 0 
     label "Nº de parcelas" 
     view-as fill-in 
     size 14 by 1 no-undo.

define variable fill-valor as decimal format "->>,>>9.99":U initial 0 
     label "Valor" 
     view-as fill-in 
     size 18 by 1 no-undo.

define image IMAGE-4
     filename "images/telas.bmp":U
     size 101 by 25.71.

define variable rs-parcelar as integer 
     view-as radio-set vertical
     radio-buttons 
          "Parcelar", 1,
"Replicar", 2
     size 13 by 2.38 no-undo.

define variable rsTpMovto as integer 
     view-as radio-set horizontal
     radio-buttons 
          "Crédito", 1,
"Débito", -1
     size 28.4 by 1.19 no-undo.

define rectangle RECT-11
     edge-pixels 2 graphic-edge  no-fill   
     size 87 by .05.

define rectangle RECT-4
     edge-pixels 2 graphic-edge  no-fill   
     size 98 by 22.57.

define rectangle RECT-5
     edge-pixels 2 graphic-edge  no-fill   
     size 98 by 2.38.

define rectangle RECT-6
     edge-pixels 2 graphic-edge  no-fill   
     size 95 by 6.52.

define rectangle RECT-7
     edge-pixels 2 graphic-edge  no-fill   
     size 95 by 14.52.

define variable l-parcelar as logical initial no 
     label "Parcelar ou replicar lançamento" 
     view-as toggle-box
     size 35 by .81 no-undo.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query brItem for 
      item scrolling.
&ANALYZE-RESUME

/* Browse definitions                                                   */
define browse brItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brItem lancamento _STRUCTURED
  query brItem no-lock display
      item.descricao format "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 5.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

define frame lancamento
     brItem at row 3.05 col 5.6 widget-id 200
     fill-data-movto at row 9.67 col 20.8 colon-aligned widget-id 18
     fill-valor at row 9.62 col 46.4 colon-aligned widget-id 30
     rsTpMovto at row 9.52 col 69.6 no-label widget-id 32
     fill-descricao at row 11 col 20.8 colon-aligned widget-id 38
     cb-ccusto at row 12.38 col 20.8 colon-aligned widget-id 22
     cb-banco at row 13.81 col 20.8 colon-aligned widget-id 20
     fill-narrativa at row 15.24 col 22.8 no-label widget-id 36
     l-parcelar at row 19.33 col 22.8 widget-id 42
     rs-parcelar at row 20.67 col 22.8 no-label widget-id 50
     fill-qtd-parc-repl at row 20.86 col 66.2 colon-aligned widget-id 56
     fill-interval-dias at row 22.05 col 66.2 colon-aligned widget-id 58
     Btn_Novo at row 24.62 col 4.8 widget-id 6
     Btn_OK_Novo at row 24.57 col 21 widget-id 62
     Btn_Cancelar at row 24.62 col 82.6
     "Obs:" view-as text
          size 5 by .62 at row 15.76 col 21.4 right-aligned widget-id 40
     " Item:" view-as text
          size 5.6 by .62 at row 2 col 7.6 widget-id 26
     IMAGE-4 at row 1 col 1 widget-id 12
     RECT-4 at row 1.33 col 2.6 widget-id 14
     RECT-5 at row 24.05 col 2.6 widget-id 16
     RECT-6 at row 2.33 col 4.2 widget-id 24
     RECT-7 at row 9.1 col 4.2 widget-id 28
     RECT-11 at row 18.91 col 8 widget-id 60
     space(7.00) skip(7.75)
    with view-as dialog-box keep-tab-order no-help 
         side-labels no-underline three-d  scrollable 
         title "Lançar movimento"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB lancamento 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX lancamento
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brItem 1 lancamento */
assign 
       frame lancamento:SCROLLABLE       = false
       frame lancamento:HIDDEN           = true.

/* SETTINGS FOR FILL-IN fill-interval-dias IN FRAME lancamento
   NO-DISPLAY                                                           */
assign 
       fill-interval-dias:HIDDEN in frame lancamento           = true.

/* SETTINGS FOR FILL-IN fill-qtd-parc-repl IN FRAME lancamento
   NO-DISPLAY                                                           */
assign 
       fill-qtd-parc-repl:HIDDEN in frame lancamento           = true.

/* SETTINGS FOR RADIO-SET rs-parcelar IN FRAME lancamento
   NO-DISPLAY                                                           */
assign 
       rs-parcelar:HIDDEN in frame lancamento           = true.

/* SETTINGS FOR TEXT-LITERAL "Obs:"
          SIZE 5 BY .62 AT ROW 15.76 COL 21.4 RIGHT-ALIGNED             */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brItem
/* Query rebuild information for BROWSE brItem
     _TblList          = "rcdb.item"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > rcdb.item.descricao
"item.descricao" ? "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brItem */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX lancamento
/* Query rebuild information for DIALOG-BOX lancamento
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX lancamento */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */




&Scoped-define SELF-NAME lancamento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lancamento lancamento
on window-close of frame lancamento /* Lançar movimento */
do:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        apply "END-ERROR":U to self.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar lancamento
on choose of Btn_Cancelar in frame lancamento /* Cancelar */
do:
    
        apply "close" to this-procedure.
    
        finally:
            if valid-object (controlador) then
                delete object controlador.
        end finally.   
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Novo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Novo lancamento
on choose of Btn_Novo in frame lancamento /* OK */
do:

    run prSalvarRegistro.
    
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK_Novo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK_Novo lancamento
on choose of Btn_OK_Novo in frame lancamento /* Ok e novo */
do:
    run prSalvarRegistro.
    return no-apply.  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME l-parcelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-parcelar lancamento
on value-changed of l-parcelar in frame lancamento /* Parcelar ou replicar lançamento */
do:
    if l-parcelar:input-value then do:
        fill-qtd-parc-repl:hidden = false.
        fill-interval-dias:hidden = false.
        rs-parcelar:hidden = false. 
    end.
    else do:
        fill-qtd-parc-repl:hidden = true.
        fill-interval-dias:hidden = true.
        rs-parcelar:hidden = true.
    end.  
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-parcelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-parcelar lancamento
on value-changed of rs-parcelar in frame lancamento
do:
    define variable iPosition as integer no-undo.

    iPosition = lookup(self:SCREEN-VALUE,self:RADIO-BUTTONS).
    if iPosition = 2 then assign 
            fill-qtd-parc-repl:label = "Nº de parcelas"
            fill-interval-dias:label = "Intervalo entre parcelas (dias)".
    else assign 
            fill-qtd-parc-repl:label = "Nº de réplicas"
            fill-interval-dias:label = "Intervalo entre réplicas (dias)".
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brItem
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK lancamento 


/* ***************************  Main Block  *************************** */
for each b-conta no-lock,
    each b-banco no-lock
        where b-banco.banco-cod = b-conta.banco-cod:
    assign cBanco = cBanco + "," + substitute("&1 &2 / &3", b-banco.descricao, b-conta.ag, b-conta.conta) + "," + substitute("&1;&2;&3", b-conta.banco-cod, b-conta.ag, b-conta.conta).
end.
cBanco = trim(cBanco, ",").
cb-banco:list-item-pairs = cBanco.

for each b-ccusto no-lock:
    assign cCCusto = cCCusto + "," + b-ccusto.cc-cod + " - " + b-ccusto.descricao + "," + b-ccusto.cc-cod.
end.
cCCusto = trim(cCCusto, ",").
cb-ccusto:list-item-pairs = cCCusto.

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects lancamento  _ADM-CREATE-OBJECTS
procedure adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI lancamento  _DEFAULT-DISABLE
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
  hide frame lancamento.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI lancamento  _DEFAULT-ENABLE
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

  display fill-data-movto fill-valor rsTpMovto fill-descricao cb-ccusto cb-banco 
          fill-narrativa l-parcelar 
      with frame lancamento.
  assign
    fill-data-movto:screen-value in frame {&FRAME-NAME} = string(today, "99/99/9999")
    cb-banco = entry(2, cBanco)
    cb-ccusto = entry(2, cCCusto).
  enable brItem fill-data-movto fill-valor rsTpMovto fill-descricao cb-ccusto 
         cb-banco fill-narrativa l-parcelar rs-parcelar fill-qtd-parc-repl 
         fill-interval-dias Btn_Novo Btn_OK_Novo Btn_Cancelar IMAGE-4 RECT-4 
         RECT-5 RECT-6 RECT-7 RECT-11 
      with frame lancamento.
  view frame lancamento.
  
    
  apply "value-changed" to l-parcelar.
    
  {&OPEN-BROWSERS-IN-QUERY-lancamento}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prSalvarRegistro lancamento 
procedure prSalvarRegistro :
/*------------------------------------------------------------------------------
     Purpose: Utilizado pelos botões OK e OK e novo
     Notes:
    ------------------------------------------------------------------------------*/
    do with frame {&FRAME-NAME}:
        {include/valida-lanc-fields.i}
        if l-parcelar and fill-qtd-parc-repl:input-value = "" then 
        do:
            ico-dialog = "error".
            run smr/dialog.w (ico-dialog, "Erro ao criar registro!", fill-qtd-parc-repl:label + " obrigatória!").
            apply "entry" to fill-qtd-parc-repl.
            return no-apply.
        end.
        if l-parcelar and fill-interval-dias:input-value = "" then 
        do:
            ico-dialog = "error".
            run smr/dialog.w (ico-dialog, "Erro ao criar registro!", fill-interval-dias:label + " obrigatória!").
            apply "entry" to fill-interval-dias.
            return no-apply.
        end.
        
        if not valid-object (controlador) then
            controlador = new MovimentoControl().
            
        controlador:cadastrar(fill-data-movto:input-value,
            entry(1, cb-banco:input-value, ";"),
            entry(2, cb-banco:input-value, ";"),
            entry(3, cb-banco:input-value, ";"),
            cb-ccusto:input-value,
            item.it-cod, 
            1,
            fill-descricao:input-value,
            rsTpMovto:input-value,
            fill-narrativa:input-value,
            "",
            fill-valor:input-value).
    
        if controlador:cReturn begins "Erro" then 
        do:
            ico-dialog = "error".
            run smr/dialog.w (ico-dialog, controlador:cReturn, "").
        end.
    
        cb-banco = entry(2, cBanco).
        cb-ccusto = entry(2, cCCusto).
        fill-data-movto:clear ().   
        fill-descricao:clear ().    
        fill-valor:clear ().        
        fill-qtd-parc-repl:clear ().    
        fill-interval-dias:clear ().
        l-parcelar = false.
    
        publish "prCalculaSaldo" (today).
    end.
    finally:
        if valid-object (controlador) then
            delete object controlador.
    end finally.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

