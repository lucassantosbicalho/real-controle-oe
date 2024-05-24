 
 /*------------------------------------------------------------------------
    File        : ItemBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 09 14:53:23 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cad.cls.Item.

class src.cad.cls.ItemBuilder: 
    

    define public property Codigo as character no-undo 
    get.
    private set. 

    define public property Descricao as character no-undo 
    get.
    private set.     

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public ItemBuilder (input pCod as character,
                                    input pDesc as character):
        assign 
            this-object:Codigo    = pCod
            this-object:Descricao = pDesc.
        
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public Item build(  ):
        return new Item(this-object).
    end method.    

end class.