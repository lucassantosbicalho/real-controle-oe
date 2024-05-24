 
 /*------------------------------------------------------------------------
    File        : CentroDeCustoBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 09 14:53:23 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cad.cls.CentroDeCusto.

class src.cad.cls.CentroDeCustoBuilder: 
    

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

    constructor public CentroDeCustoBuilder (input pCod as character,
                                             input pDesc as character):
        assign 
            this-object:Codigo    = pCod
            this-object:Descricao = pDesc.
        
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public CentroDeCusto build(  ):
        return new CentroDeCusto(this-object).
    end method.    

end class.