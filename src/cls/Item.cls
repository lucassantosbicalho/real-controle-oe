 
 /*------------------------------------------------------------------------
    File        : Item
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 16 20:54:23 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cls.ItemBuilder.

class src.cls.Item: 

    /* obrigatorios */
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

    constructor public Item (builder as ItemBuilder):
        assign 
            this-object:Codigo    = builder:Codigo   
            this-object:Descricao = builder:Descricao.
    end constructor.

end class.