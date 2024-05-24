
/*------------------------------------------------------------------------
    File        : menu.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Lucas Bicalho
    Created     : Sun Sep 17 18:09:19 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE SUB-MENU mArquivo
   MENU-ITEM m1  LABEL "Importar back-up"
   MENU-ITEM m2  LABEL "Exportar back-up"
   MENU-ITEM m3  LABEL "Diretório back-up" RULE
   MENU-ITEM m4  LABEL "Sair".
   
DEFINE SUB-MENU mSobre
   MENU-ITEM m1  LABEL "Sobre o Real Controle" DISABLED.

DEFINE SUB-MENU mCadastrar
   MENU-ITEM m1  LABEL "Item (CTRL+I)"
   MENU-ITEM m2  LABEL "Conta bancária (CTRL+B)"
   MENU-ITEM m3  LABEL "Centro de custo (CTRL+C)" 
   MENU-ITEM m4  LABEL "Pessoa física (CTRL+P)" DISABLED
   MENU-ITEM m5  LABEL "Usuário (CTRL+U)" DISABLED.
   
DEFINE SUB-MENU mEditar
   MENU-ITEM m1  LABEL "Lançar movimento (CTRL+L)" 
   MENU-ITEM m2  LABEL "Visualiar movimentos (CTRL+M)".
   
DEFINE SUB-MENU mRelatorios
   MENU-ITEM m1 LABEL "Extrato consolidado (CTRL+E)" DISABLED
   MENU-ITEM m2 LABEL "Fluxo de caixa (CTRL+F)" DISABLED.
   
DEFINE MENU mBarra MENUBAR
   SUB-MENU mArquivo    LABEL "Arquivo"
   SUB-MENU mCadastrar  LABEL "Cadastrar"
   SUB-MENU mEditar     LABEL "Editar"
   SUB-MENU mRelatorios LABEL "Relatórios"
   SUB-MENU mSobre      LABEL "Ajuda" .
   
   
/* --------------------------------------------------------------------------------
         Triggers Menu Arquivo
-------------------------------------------------------------------------------- */ 
ON CHOOSE OF MENU-ITEM m1 IN MENU mArquivo
DO:
    MESSAGE "Clicou " MENU-ITEM m1:LABEL IN MENU mArquivo
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.
   
ON CHOOSE OF MENU-ITEM m2 IN MENU mArquivo
DO:
    MESSAGE "Clicou " MENU-ITEM m2:LABEL IN MENU mArquivo
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.
   
ON CHOOSE OF MENU-ITEM m3 IN MENU mArquivo
DO:
    APPLY "close":u TO THIS-PROCEDURE.
END.

/* --------------------------------------------------------------------------------
         Triggers Menu Sobre
-------------------------------------------------------------------------------- */ 
ON CHOOSE OF MENU-ITEM m1 IN MENU mSobre
DO:
    MESSAGE "Clicou " MENU-ITEM m1:LABEL IN MENU mSobre
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.

/* --------------------------------------------------------------------------------
         Triggers Menu Cadastrar
-------------------------------------------------------------------------------- */ 
ON CHOOSE OF MENU-ITEM m1 IN MENU mCadastrar
DO:
    RUN smr/cad-item.w.
    RETURN.
END.
   
ON CHOOSE OF MENU-ITEM m2 IN MENU mCadastrar
DO:
    RUN smr/cad-conta.w.
    RETURN.
END.
   
ON CHOOSE OF MENU-ITEM m3 IN MENU mCadastrar
DO:
    RUN smr/cad-ccusto.w.
    RETURN.
END.

ON CHOOSE OF MENU-ITEM m4 IN MENU mCadastrar
DO:
    MESSAGE "Clicou " MENU-ITEM m4:LABEL IN MENU mCadastrar
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.

ON CHOOSE OF MENU-ITEM m5 IN MENU mCadastrar
DO:
    MESSAGE "Clicou " MENU-ITEM m5:LABEL IN MENU mCadastrar
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
END.

/* --------------------------------------------------------------------------------
         Triggers Menu Editar
-------------------------------------------------------------------------------- */ 
ON CHOOSE OF MENU-ITEM m1 IN MENU mEditar
DO:
    RUN smr/lanc.w.
END.
ON CHOOSE OF MENU-ITEM m2 IN MENU mEditar
DO:
    RUN cad/vwrs/mov.w.
END.
