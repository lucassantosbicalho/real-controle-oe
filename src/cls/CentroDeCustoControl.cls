 
 /*------------------------------------------------------------------------
    File        : CentroDeCustoControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sun Sep 26 19:07:11 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cls.CentroDeCustoBuilder.

block-level on error undo, throw.

class src.cls.CentroDeCustoControl: 
    
    define public property cReturn as character no-undo 
    get.
    private set.
    
    define private property pCC as CentroDeCustoBuilder no-undo
    private get.
    private set.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character cadastrar(input pCodigo       as character,
                                      input pDescricao    as character):
        
        init().
        
        
        pCC = new CentroDeCustoBuilder(pCodigo,
                                       pDescricao).

        if centroDeCustoExistente(pCC) then do:
            cReturn = substitute("Erro! Centro de custo existente! (&1)", pCC:Codigo).
            return cReturn.
        end.

        create ccusto.
        assign 
            ccusto.cc-cod    = pCodigo
            ccusto.descricao = pDescricao.     
        
        cReturn = substitute("Centro de custo cadastrado com sucesso! (&1 - &2)", pCC:Codigo, pCC:Descricao).
        
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
    method public character excluir(input pCodigo       as character):
        
        init().
        
        
        pCC = new CentroDeCustoBuilder(pCodigo, 
                                       "").

        if not centroDeCustoExistente(pCC) then do:
            cReturn = substitute("Erro! Centro de custo inexistente! (&1)", pCC:Codigo).
            return cReturn.
        end.

        find ccusto exclusive-lock
            where ccusto.cc-cod = pCodigo.
        delete ccusto.
        release ccusto.
                
        cReturn = substitute("Centro de custo &1 excluído com sucesso!", pCC:Codigo).
        
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
    method public character alterar(input pCodigo       as character,
                                    input pDescricao    as character):
        
        init().
        
        pCC = new CentroDeCustoBuilder(pCodigo,
                                       pDescricao).

        if not centroDeCustoExistente(pCC) then do:
            cReturn = substitute("Erro! Centro de custo inexistente! (&1)", pCC:Codigo).
            return cReturn.
        end.

        find ccusto exclusive-lock
            where ccusto.cc-cod = pCodigo.
        assign 
            ccusto.descricao = (if ccusto.descricao       <> pDescricao    then pDescricao    else ccusto.descricao      ).
        release ccusto.
                
        cReturn = substitute("Centro de custo alterado com sucesso! (&1)", pCC:Codigo).
        
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

    method private logical centroDeCustoExistente(input pCC as CentroDeCustoBuilder):
        
        if can-find(ccusto no-lock
                    where ccusto.cc-cod = pCC:Codigo)
            then return true.
        
        return false.
            
    end method.

end class.