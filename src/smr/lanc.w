&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          rcdb             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME lancamento

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS lancamento 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS Procedure
USING src.cad.cls.MovimentoControl FROM PROPATH.
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
DEFINE VARIABLE controlador AS MovimentoControl     NO-UNDO.
DEFINE VARIABLE cCCusto     AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cBanco      AS CHARACTER            NO-UNDO.    
DEFINE VARIABLE ico-dialog  AS CHARACTER            NO-UNDO.

DEFINE BUFFER b-conta FOR conta.
DEFINE BUFFER b-banco FOR banco.
DEFINE BUFFER b-ccusto FOR ccusto.

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
DEFINE BUTTON Btn_Cancelar AUTO-GO 
     LABEL "Cancelar" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Novo AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14 TOOLTIP "Salvar e fechar".

DEFINE BUTTON Btn_OK_Novo 
     LABEL "Ok e novo" 
     SIZE 15 BY 1.14 TOOLTIP "Salvar e realizar novo lançamento".

DEFINE VARIABLE cb-banco AS CHARACTER FORMAT "X(256)":U 
     LABEL "Conta bancária" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE cb-ccusto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Centro de custo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE fill-narrativa AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 300
     SIZE 72 BY 3.05 NO-UNDO.

DEFINE VARIABLE fill-data-movto AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fill-descricao AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descrição" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE fill-interval-dias AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Intervalo entre parcelas (dias)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fill-qtd-parc-repl AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Nº de parcelas" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fill-valor AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "Telas/telas.bmp":U
     SIZE 101 BY 25.71.

DEFINE VARIABLE rs-parcelar AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Parcelar", 1,
"Replicar", 2
     SIZE 13 BY 2.38 NO-UNDO.

DEFINE VARIABLE rsTpMovto AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Crédito", 1,
"Débito", -1
     SIZE 28.4 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY .05.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 22.57.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 98 BY 2.38.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 6.52.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 14.52.

DEFINE VARIABLE l-parcelar AS LOGICAL INITIAL no 
     LABEL "Parcelar ou replicar lançamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brItem FOR 
      item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brItem lancamento _STRUCTURED
  QUERY brItem NO-LOCK DISPLAY
      item.descricao FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 5.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME lancamento
     brItem AT ROW 3.05 COL 5.6 WIDGET-ID 200
     fill-data-movto AT ROW 9.67 COL 20.8 COLON-ALIGNED WIDGET-ID 18
     fill-valor AT ROW 9.62 COL 46.4 COLON-ALIGNED WIDGET-ID 30
     rsTpMovto AT ROW 9.52 COL 69.6 NO-LABEL WIDGET-ID 32
     fill-descricao AT ROW 11 COL 20.8 COLON-ALIGNED WIDGET-ID 38
     cb-ccusto AT ROW 12.38 COL 20.8 COLON-ALIGNED WIDGET-ID 22
     cb-banco AT ROW 13.81 COL 20.8 COLON-ALIGNED WIDGET-ID 20
     fill-narrativa AT ROW 15.24 COL 22.8 NO-LABEL WIDGET-ID 36
     l-parcelar AT ROW 19.33 COL 22.8 WIDGET-ID 42
     rs-parcelar AT ROW 20.67 COL 22.8 NO-LABEL WIDGET-ID 50
     fill-qtd-parc-repl AT ROW 20.86 COL 66.2 COLON-ALIGNED WIDGET-ID 56
     fill-interval-dias AT ROW 22.05 COL 66.2 COLON-ALIGNED WIDGET-ID 58
     Btn_Novo AT ROW 24.62 COL 4.8 WIDGET-ID 6
     Btn_OK_Novo AT ROW 24.57 COL 21 WIDGET-ID 62
     Btn_Cancelar AT ROW 24.62 COL 82.6
     "Obs:" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 15.76 COL 21.4 RIGHT-ALIGNED WIDGET-ID 40
     " Item:" VIEW-AS TEXT
          SIZE 5.6 BY .62 AT ROW 2 COL 7.6 WIDGET-ID 26
     IMAGE-4 AT ROW 1 COL 1 WIDGET-ID 12
     RECT-4 AT ROW 1.33 COL 2.6 WIDGET-ID 14
     RECT-5 AT ROW 24.05 COL 2.6 WIDGET-ID 16
     RECT-6 AT ROW 2.33 COL 4.2 WIDGET-ID 24
     RECT-7 AT ROW 9.1 COL 4.2 WIDGET-ID 28
     RECT-11 AT ROW 18.91 COL 8 WIDGET-ID 60
     SPACE(7.00) SKIP(7.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-HELP 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Lançar movimento"
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
ASSIGN 
       FRAME lancamento:SCROLLABLE       = FALSE
       FRAME lancamento:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fill-interval-dias IN FRAME lancamento
   NO-DISPLAY                                                           */
ASSIGN 
       fill-interval-dias:HIDDEN IN FRAME lancamento           = TRUE.

/* SETTINGS FOR FILL-IN fill-qtd-parc-repl IN FRAME lancamento
   NO-DISPLAY                                                           */
ASSIGN 
       fill-qtd-parc-repl:HIDDEN IN FRAME lancamento           = TRUE.

/* SETTINGS FOR RADIO-SET rs-parcelar IN FRAME lancamento
   NO-DISPLAY                                                           */
ASSIGN 
       rs-parcelar:HIDDEN IN FRAME lancamento           = TRUE.

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
ON WINDOW-CLOSE OF FRAME lancamento /* Lançar movimento */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar lancamento
ON CHOOSE OF Btn_Cancelar IN FRAME lancamento /* Cancelar */
DO:
    
        APPLY "close" TO THIS-PROCEDURE.
    
        FINALLY:
            IF VALID-OBJECT (controlador) THEN
                DELETE OBJECT controlador.
        END FINALLY.   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Novo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Novo lancamento
ON CHOOSE OF Btn_Novo IN FRAME lancamento /* OK */
DO:

    RUN prSalvarRegistro.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK_Novo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK_Novo lancamento
ON CHOOSE OF Btn_OK_Novo IN FRAME lancamento /* Ok e novo */
DO:
    RUN prSalvarRegistro.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME l-parcelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-parcelar lancamento
ON VALUE-CHANGED OF l-parcelar IN FRAME lancamento /* Parcelar ou replicar lançamento */
DO:
    IF l-parcelar:input-value THEN DO:
        fill-qtd-parc-repl:hidden = FALSE.
        fill-interval-dias:hidden = FALSE.
        rs-parcelar:hidden = FALSE. 
    END.
    ELSE DO:
        fill-qtd-parc-repl:hidden = TRUE.
        fill-interval-dias:hidden = TRUE.
        rs-parcelar:hidden = TRUE.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-parcelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-parcelar lancamento
ON VALUE-CHANGED OF rs-parcelar IN FRAME lancamento
DO:
    DEFINE VARIABLE iPosition AS INTEGER NO-UNDO.

    iPosition = LOOKUP(SELF:SCREEN-VALUE,SELF:RADIO-BUTTONS).
    IF iPosition = 2 THEN ASSIGN 
            fill-qtd-parc-repl:label = "Nº de parcelas"
            fill-interval-dias:label = "Intervalo entre parcelas (dias)".
    ELSE ASSIGN 
            fill-qtd-parc-repl:label = "Nº de réplicas"
            fill-interval-dias:label = "Intervalo entre réplicas (dias)".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brItem
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK lancamento 


/* ***************************  Main Block  *************************** */
FOR EACH b-conta NO-LOCK,
    EACH b-banco NO-LOCK
        WHERE b-banco.banco-cod = b-conta.banco-cod:
    ASSIGN cBanco = cBanco + "," + substitute("&1 &2 / &3", b-banco.descricao, b-conta.ag, b-conta.conta) + "," + substitute("&1;&2;&3", b-conta.banco-cod, b-conta.ag, b-conta.conta).
END.
cBanco = TRIM(cBanco, ",").
cb-banco:list-item-pairs = cBanco.
cb-banco = ENTRY(2, cBanco).

FOR EACH b-ccusto NO-LOCK:
    ASSIGN cCCusto = cCCusto + "," + b-ccusto.cc-cod + " - " + b-ccusto.descricao + "," + b-ccusto.cc-cod.
END.
cCCusto = TRIM(cCCusto, ",").
cb-ccusto:list-item-pairs = cCCusto.
cb-ccusto = ENTRY(2, cCCusto).

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects lancamento  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI lancamento  _DEFAULT-DISABLE
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
  HIDE FRAME lancamento.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI lancamento  _DEFAULT-ENABLE
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
  DISPLAY fill-data-movto fill-valor rsTpMovto fill-descricao cb-ccusto cb-banco 
          fill-narrativa l-parcelar 
      WITH FRAME lancamento.
  ENABLE brItem fill-data-movto fill-valor rsTpMovto fill-descricao cb-ccusto 
         cb-banco fill-narrativa l-parcelar rs-parcelar fill-qtd-parc-repl 
         fill-interval-dias Btn_Novo Btn_OK_Novo Btn_Cancelar IMAGE-4 RECT-4 
         RECT-5 RECT-6 RECT-7 RECT-11 
      WITH FRAME lancamento.
  VIEW FRAME lancamento.
  {&OPEN-BROWSERS-IN-QUERY-lancamento}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prSalvarRegistro lancamento 
PROCEDURE prSalvarRegistro :
/*------------------------------------------------------------------------------
     Purpose: Utilizado pelos botões OK e OK e novo
     Notes:
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF fill-data-movto:input-value = ? THEN 
        DO:
            ico-dialog = "error".
            RUN smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Data obrigatória!").
            APPLY "entry" TO fill-data-movto.
            RETURN NO-APPLY.
        END.
        IF fill-valor:input-value = 0 THEN 
        DO:
            ico-dialog = "error".
            RUN smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Valor obrigatório!").
            APPLY "entry" TO fill-valor.
            RETURN NO-APPLY.
        END.
        IF fill-descricao:input-value = "" THEN 
        DO:
            ico-dialog = "error".
            RUN smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Descrição obrigatória!").
            APPLY "entry" TO fill-descricao.
            RETURN NO-APPLY.
        END.
        IF cb-ccusto:input-value = "" THEN 
        DO:
            ico-dialog = "error".
            RUN smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Centro de custo obrigatório!").
            APPLY "entry" TO cb-ccusto.
            RETURN NO-APPLY.
        END.
        IF cb-banco:input-value = "" THEN 
        DO:
            ico-dialog = "error".
            RUN smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Conta bancária obrigatória!").
            APPLY "entry" TO cb-banco.
            RETURN NO-APPLY.
        END.
        IF l-parcelar AND fill-qtd-parc-repl:input-value = "" THEN 
        DO:
            ico-dialog = "error".
            RUN smr/dialog.w (ico-dialog, "Erro ao criar registro!", fill-qtd-parc-repl:label + " obrigatória!").
            APPLY "entry" TO fill-qtd-parc-repl.
            RETURN NO-APPLY.
        END.
        IF l-parcelar AND fill-interval-dias:input-value = "" THEN 
        DO:
            ico-dialog = "error".
            RUN smr/dialog.w (ico-dialog, "Erro ao criar registro!", fill-interval-dias:label + " obrigatória!").
            APPLY "entry" TO fill-interval-dias.
            RETURN NO-APPLY.
        END.
        
        IF NOT VALID-OBJECT (controlador) THEN
            controlador = NEW MovimentoControl().
            
        controlador:cadastrar(fill-data-movto:input-value,
            ENTRY(1, cb-banco:input-value, ";"),
            ENTRY(2, cb-banco:input-value, ";"),
            ENTRY(3, cb-banco:input-value, ";"),
            cb-ccusto:input-value,
            item.it-cod, 
            1,
            fill-descricao:input-value,
            rsTpMovto:input-value,
            fill-narrativa:input-value,
            "",
            (fill-valor:input-value * integer(rsTpMovto:input-value   ))).
    
        IF controlador:cReturn BEGINS "Erro" THEN 
        DO:
            ico-dialog = "error".
            RUN smr/dialog.w (ico-dialog, controlador:cReturn, "").
        END.
    
        cb-banco = ENTRY(2, cBanco).
        cb-ccusto = ENTRY(2, cCCusto).
        fill-data-movto:clear ().   
        fill-descricao:clear ().    
        fill-valor:clear ().        
        fill-qtd-parc-repl:clear ().    
        fill-interval-dias:clear ().
        l-parcelar = FALSE.
    
        
    END.
    FINALLY:
        IF VALID-OBJECT (controlador) THEN
            DELETE OBJECT controlador.
    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

