 
 /*------------------------------------------------------------------------
    File        : ContaBancariaControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sun Sep 26 19:07:11 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cad.cls.ContaBancariaBuilder.

block-level on error undo, throw.

class src.cad.cls.ContaBancariaControl: 
    
    define public property cReturn as character no-undo 
    get.
    private set.
    
    define private property pConta as ContaBancariaBuilder no-undo
    private get.
    private set.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character cadastrar(input pBanco       as character,
                                      input pAgencia     as character,
                                      input pContaCod    as character,
                                      input pTpConta     as character,
                                      input pDocTitular  as character,
                                      input pApelido     as character):
        
        init().
        
        
        pConta = new ContaBancariaBuilder(pBanco,
                                          pAgencia,
                                          pContaCod,
                                          pTpConta,
                                          pDocTitular,
                                          pApelido).

        if contaExistente(pConta) then do:
            cReturn = substitute("Erro! Conta existente! (Banc &1 Ag &2 Cta &3)", pConta:Banco, pConta:Agencia, pConta:Conta).
            return cReturn.
        end.

        create conta.
        assign 
            conta.ag            = pAgencia
            conta.apelido       = pApelido     
            conta.banco-cod     = pBanco      
            conta.conta         = pContaCod   
            conta.titular-conta = pDocTitular           
            conta.tp-conta      = pTpConta.
        
        cReturn = substitute("Conta cadastrada com sucesso! (Banc &1 Ag &2 Cta &3)", pConta:Banco, pConta:Agencia, pConta:Conta).
        
        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
            return cReturn.
        end finally.

    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method public character excluir(input pBanco       as character,
                                    input pAgencia     as character,
                                    input pContaCod    as character):
        
        init().
        
        
        pConta = new ContaBancariaBuilder(pBanco,
                                          pAgencia,
                                          pContaCod,
                                          "",
                                          "",
                                          "").

        if not contaExistente(pConta) then do:
            cReturn = substitute("Erro! Conta inexistente! (Banc &1 Ag &2 Cta &3)", pConta:Banco, pConta:Agencia, pConta:Conta).
            return cReturn.
        end.

        find conta exclusive-lock
            where conta.banco-cod = pBanco
              and conta.ag        = pAgencia
              and conta.conta     = pContaCod.
        delete conta.
        release conta.
                
        cReturn = substitute("Conta excluída com sucesso! (Banc &1 Ag &2 Cta &3)", pConta:Banco, pConta:Agencia, pConta:Conta).
        
        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
            return cReturn.
        end finally.

    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method public character alterar(input pBanco       as character,
                                    input pAgencia     as character,
                                    input pContaCod    as character,
                                    input pTpConta     as character,
                                    input pDocTitular  as character,
                                    input pApelido     as character):
        
        init().
        
        
        pConta = new ContaBancariaBuilder(pBanco,
                                          pAgencia,
                                          pContaCod,
                                          pTpConta,
                                          pDocTitular,
                                          pApelido).

        if not contaExistente(pConta) then do:
            cReturn = substitute("Erro! Conta inexistente! (Banc &1 Ag &2 Cta &3)", pConta:Banco, pConta:Agencia, pConta:Conta).
            return cReturn.
        end.

        find conta exclusive-lock
            where conta.banco-cod = pBanco
              and conta.ag        = pAgencia
              and conta.conta     = pContaCod.
        assign 
            conta.apelido       = (if conta.apelido       <> pApelido    then pApelido    else conta.apelido      ) 
            conta.titular-conta = (if conta.titular-conta <> pDocTitular then pDocTitular else conta.titular-conta)          
            conta.tp-conta      = (if conta.tp-conta      <> pTpConta    then pTpConta    else conta.tp-conta     ).
        release conta.
                
        cReturn = substitute("Conta alterada com sucesso! (Banc &1 Ag &2 Cta &3)", pConta:Banco, pConta:Agencia, pConta:Conta).
        
        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
            return cReturn.
        end finally.

    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private void init(  ):
        
        assign 
            cReturn             = "".

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private logical contaExistente(input pConta as ContaBancariaBuilder):
        
        if can-find(conta no-lock
                    where conta.banco-cod = pConta:Banco
                      and conta.ag        = pConta:Agencia
                      and conta.conta     = pConta:Conta)
            then return true.
        
        return false.
            
    end method.

end class.