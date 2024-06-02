 
 /*------------------------------------------------------------------------
    File        : PessoaFisicaBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 09 12:53:03 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cls.PessoaFisica.

class src.cls.PessoaFisicaBuilder: 

    define public property CPF as character no-undo 
    get.
    private set. 

    define public property Nome as character no-undo 
    get.
    private set. 

    define public property Sobrenome as character no-undo 
    get.
    private set. 

    define public property DataNascimento as date no-undo 
    get.
    private set. 

    define public property Email as character no-undo 
    get.
    private set.     

    define public property TelCelular as character no-undo 
    get.
    private set.     

    define public property TelOutro as character no-undo 
    get.
    private set.


    /*------------------------------------------------------------------------------
     Purpose: Required parameters
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public PessoaFisicaBuilder (input pCPF             as character,
                                            input pNome            as character,
                                            input pSobrenome       as character,
                                            input pDataNascimento  as date):
        assign
            this-object:CPF            = pCPF           
            this-object:Nome           = pNome          
            this-object:Sobrenome      = pSobrenome     
            this-object:DataNascimento = pDataNascimento.
        
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose: Set email
     Notes:
    ------------------------------------------------------------------------------*/

    method public src.cls.PessoaFisicaBuilder setEmail(input pEmail as character):
        
        assign
            this-object:Email = pEmail.
            
        return this-object.

    end method.    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public src.cls.PessoaFisicaBuilder setTelCelular(input pCelular as character):
        
        assign 
            this-object:TelCelular = pCelular.
            
        return this-object.

    end method.    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public src.cls.PessoaFisicaBuilder setOutroTelefone2(input pTelOutro as character):
        
        assign 
            this-object:TelOutro = pTelOutro.
        
        return this-object.

    end method.    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public src.cls.PessoaFisica build(  ):
        return new PessoaFisica(this-object).            
    end method.    


end class.