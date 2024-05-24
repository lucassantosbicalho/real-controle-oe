 
 /*------------------------------------------------------------------------
    File        : ContaBancariaBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 09 15:00:35 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cad.cls.ContaBancaria.

class src.cad.cls.ContaBancariaBuilder: 

    define public property Banco as character no-undo 
    get.
    private set. 

    define public property Agencia as character no-undo 
    get.
    private set. 

    define public property Conta as character no-undo 
    get.
    private set.
     
    define public property TpConta as character no-undo 
    get.
    private set. 

    define public property Apelido as character no-undo 
    get.
    private set.   

    define public property DocTitular as character no-undo 
    get.
    private set.     
    
      
    
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public ContaBancariaBuilder (input pBanco       as character,
                                             input pAgencia     as character,
                                             input pConta       as character,
                                             input pTpConta     as character,
                                             input pDocTitular  as character,
                                             input pApelido     as character):
        assign 
           this-object:Banco        = pBanco    
           this-object:Agencia      = pAgencia  
           this-object:Conta        = pConta    
           this-object:TpConta      = pTpConta
           this-object:DocTitular   = pDocTitular
           this-object:Apelido      = pApelido.  
            
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public src.cad.cls.ContaBancariaBuilder setApelidoConta(input pApelido as character):
        
        assign 
            this-object:Apelido = substring (pApelido, 1 , 25).
        return this-object.

    end method.    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public ContaBancaria build(  ):
        return new ContaBancaria(this-object).
    end method.    


end class.