 
 /*------------------------------------------------------------------------
    File        : ContaBancaria
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 16 21:04:48 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cls.ContaBancariaBuilder.
using src.cls.ContaBancaria from propath.

class src.cls.ContaBancaria: 
    
    /* obrigatorios */
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

    define public property DocTitular as character no-undo 
    get.
    private set.
    
    /* opcionais */
    define public property Apelido as character no-undo 
    get.
    private set.   

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public ContaBancaria (builder as ContaBancariaBuilder):
        assign
            this-object:Banco       = builder:Banco 
            this-object:Agencia     = builder:Agencia   
            this-object:Conta       = builder:Conta 
            this-object:TpConta     = builder:TpConta   
            this-object:DocTitular  = builder:DocTitular   
            this-object:Apelido     = builder:Apelido.
    end constructor.    
    
end class.