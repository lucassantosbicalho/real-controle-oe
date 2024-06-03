
/*------------------------------------------------------------------------
    File        : valida-lanc-fields.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Lucas Bicalho
    Created     : Sun Jun 02 22:20:23 BRT 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
if fill-data-movto:input-value = ? then 
do:
    ico-dialog = "error".
    run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Data obrigat�ria!").
    apply "entry" to fill-data-movto.
    return no-apply.
end.
if fill-valor:input-value = 0 then 
do:
    ico-dialog = "error".
    run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Valor obrigat�rio!").
    apply "entry" to fill-valor.
    return no-apply.
end.
if fill-descricao:input-value = "" then 
do:
    ico-dialog = "error".
    run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Descri��o obrigat�ria!").
    apply "entry" to fill-descricao.
    return no-apply.
end.
if cb-ccusto:input-value = "" then 
do:
    ico-dialog = "error".
    run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Centro de custo obrigat�rio!").
    apply "entry" to cb-ccusto.
    return no-apply.
end.
if cb-banco:input-value = "" then 
do:
    ico-dialog = "error".
    run smr/dialog.w (ico-dialog, "Erro ao criar registro!", "Conta banc�ria obrigat�ria!").
    apply "entry" to cb-banco.
    return no-apply.
end.