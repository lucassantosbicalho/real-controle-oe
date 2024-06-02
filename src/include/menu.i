
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
define sub-menu mArquivo
   menu-item m1  label "Importar back-up" disabled
   menu-item m2  label "Exportar back-up" disabled
   menu-item m3  label "Diretório back-up" disabled rule 
   menu-item m4  label "Sair".
   
define sub-menu mSobre
   menu-item m1  label "Sobre o Real Controle" disabled.

define sub-menu mCadastrar
   menu-item m1  label "Item"
   menu-item m2  label "Conta bancária"
   menu-item m3  label "Centro de custo" 
   menu-item m4  label "Pessoa física" disabled
   menu-item m5  label "Usuário" disabled.
   
define sub-menu mEditar
   menu-item m1  label "Lançar movimento" 
   menu-item m2  label "Visualizar movimentos".
   
define sub-menu mRelatorios
   menu-item m1 label "Extrato consolidado" disabled
   menu-item m2 label "Fluxo de caixa" disabled.
   
define menu mBarra menubar
   sub-menu mArquivo    label "Arquivo"
   sub-menu mCadastrar  label "Cadastrar"
   sub-menu mEditar     label "Editar"
   sub-menu mRelatorios label "Relatórios"
   sub-menu mSobre      label "Ajuda" .
   
   
/* --------------------------------------------------------------------------------
         Triggers Menu Arquivo
-------------------------------------------------------------------------------- */ 
on choose of menu-item m1 in menu mArquivo
do:
    message "Clicou " menu-item m1:LABEL in menu mArquivo
        view-as alert-box information buttons ok.
    return.
end.
   
on choose of menu-item m2 in menu mArquivo
do:
    message "Clicou " menu-item m2:LABEL in menu mArquivo
        view-as alert-box information buttons ok.
    return.
end.
   
on choose of menu-item m3 in menu mArquivo
do:
    apply "close":u to this-procedure.
end.

/* --------------------------------------------------------------------------------
         Triggers Menu Sobre
-------------------------------------------------------------------------------- */ 
on choose of menu-item m1 in menu mSobre
do:
    message "Clicou " menu-item m1:LABEL in menu mSobre
        view-as alert-box information buttons ok.
    return.
end.

/* --------------------------------------------------------------------------------
         Triggers Menu Cadastrar
-------------------------------------------------------------------------------- */ 
on choose of menu-item m1 in menu mCadastrar
do:
    run smr/cad-item.w.
    return.
end.
   
on choose of menu-item m2 in menu mCadastrar
do:
    run smr/cad-conta.w.
    return.
end.
   
on choose of menu-item m3 in menu mCadastrar
do:
    run smr/cad-ccusto.w.
    return.
end.

on choose of menu-item m4 in menu mCadastrar
do:
    message "Clicou " menu-item m4:LABEL in menu mCadastrar
        view-as alert-box information buttons ok.
    return.
end.

on choose of menu-item m5 in menu mCadastrar
do:
    message "Clicou " menu-item m5:LABEL in menu mCadastrar
        view-as alert-box information buttons ok.
    return.
end.

/* --------------------------------------------------------------------------------
         Triggers Menu Editar
-------------------------------------------------------------------------------- */ 
on choose of menu-item m1 in menu mEditar
do:
    run smr/lanc.w.
end.
on choose of menu-item m2 in menu mEditar
do:
    run vwrs/movto.w.
end.
