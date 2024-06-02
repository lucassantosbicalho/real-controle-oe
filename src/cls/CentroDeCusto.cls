 
 /*------------------------------------------------------------------------
    File        : CentroDeCusto
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 16 21:26:19 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cls.CentroDeCustoBuilder.

class src.cls.CentroDeCusto: 
    
    /* obrigatorios */
    define public property Codigo as character no-undo 
    get.
    private set. 

    define public property Descricao as character no-undo 
    get.
    private set.
    
    constructor public CentroDeCusto(builder as CentroDeCustoBuilder):
        assign
            this-object:Codigo    = builder:Codigo
            this-object:Descricao = builder:Descricao.
    end constructor.
end class.