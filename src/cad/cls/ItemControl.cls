 
 /*------------------------------------------------------------------------
    File        : ItemControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sun Sep 17 17:08:01 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cad.cls.ItemBuilder.

block-level on error undo, throw.

class src.cad.cls.ItemControl: 
    
    define public property cReturn as character no-undo 
    get.
    private set.
    
    define private property pItem as ItemBuilder no-undo
    private get.
    private set.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character cadastrar(input codItem as character, 
                                      input descItem as character):
        
        init().
        
        pItem = new ItemBuilder(codItem, descItem).
        
        if itemExistente(pItem) then do:
            cReturn = substitute("Erro! Item existente! (&1)", pItem:Codigo).
            return cReturn.
        end.
       
        create item.
        assign
            item.it-cod     = pItem:Codigo 
            item.descricao  = pItem:Descricao. 
        
        cReturn = substitute("Item &1 cadastrado com sucesso!", pItem:Codigo).
        
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

    method public character excluir(input codItem as character):
        
        init().
        
        pItem = new ItemBuilder(codItem, "").
        
        if not itemExistente(pItem) then do:
            cReturn = substitute("Erro! Item não encontrado! (&1)", pItem:Codigo).
            return cReturn.
        end.
            
        find item exclusive-lock
            where item.it-cod = pItem:Codigo.
            
        delete item.
        release item.
        
        cReturn = substitute("Item &1 excluído com sucesso!", pItem:Codigo).
        
        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
        end finally.

    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character alterar(input codItem as character, 
                                    input descItem as character):
        
        init().
        
        pItem = new ItemBuilder(codItem, descItem).
        
        if not itemExistente(pItem) then do:
            cReturn = substitute("Erro! Item não encontrado! (&1)", pItem:Codigo).
            return cReturn.
        end.
            
        find item exclusive-lock
            where item.it-cod = pItem:Codigo.
            
        assign item.descricao = pItem:Descricao.
        release item.
        
        cReturn = substitute("Item &1 alterado com sucesso!", pItem:Codigo).
        
        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
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

    method private logical itemExistente(input pItem as ItemBuilder):
        
        if can-find(item no-lock
                    where item.it-cod = pItem:Codigo)
            then return true.
        
        return false.
            
    end method.

end class.