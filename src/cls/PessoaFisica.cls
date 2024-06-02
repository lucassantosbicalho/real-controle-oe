 
 /*------------------------------------------------------------------------
    File        : PessoaFisica
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 16 20:41:38 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cls.PessoaFisicaBuilder.

class src.cls.PessoaFisica: 

    /* obrigatorios */
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

    /* opcionais */
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
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public PessoaFisica (builder as PessoaFisicaBuilder):
        assign
            this-object:CPF            = CPF            
            this-object:Nome           = Nome            
            this-object:Sobrenome      = Sobrenome            
            this-object:DataNascimento = DataNascimento            
            this-object:Email          = Email            
            this-object:TelCelular     = TelCelular            
            this-object:TelOutro       = TelOutro.            
        
    end constructor.

end class.