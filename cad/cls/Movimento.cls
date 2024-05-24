 
 /*------------------------------------------------------------------------
    File        : Movimento
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 16 22:16:31 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cad.cls.MovimentoBuilder.

class src.cad.cls.Movimento: 
    
    /* obrigatorios */    
    define public property DataMvto as date no-undo
    get.
    private set.
    
    define public property CcCod as character no-undo
    get.
    private set.
    
    define public property Conta as character no-undo
    get.
    private set.
    
    define public property ItCod as character no-undo
    get.
    private set.
    
    define public property Seq as integer no-undo
    get.
    private set.
    
    define public property MovtoTp as integer no-undo
    get.
    private set.
    
    define public property Valor as decimal no-undo
    get.
    private set.
    
    define public property Usuario as character no-undo
    get.
    private set.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public Movimento (builder as MovimentoBuilder):
        assign 
            this-object:DataMvto = builder:DataMvto     
            this-object:CcCod    = builder:CcCod  
            this-object:Conta    = builder:Conta  
            this-object:ItCod    = builder:ItCod  
            this-object:Seq      = builder:Seq
            this-object:MovtoTp  = builder:MovtoTp    
            this-object:Valor    = builder:Valor  
            this-object:Usuario  = builder:Usuario.            
    end constructor.
    

end class.