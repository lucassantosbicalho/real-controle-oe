 
 /*------------------------------------------------------------------------
    File        : User
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 16 20:07:16 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cad.cls.UsuarioBuilder.

class src.cad.cls.Usuario: 
    
    /* obrigatorios */
    define public property Usuario as character no-undo 
    get.
    private set. 

    define public property Senha as character no-undo 
    get.
    private set. 

    define public property Nome as character no-undo 
    get.
    private set. 

    define public property Sobrenome as character no-undo 
    get.
    private set. 

    define public property Email as character no-undo 
    get.
    private set. 

    define public property Ativo as logical no-undo 
    get.
    private set. 


    /*------------------------------------------------------------------------------
     Purpose: The UserBuilder has all properties needed
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public Usuario (builder as UsuarioBuilder):
        assign
            this-object:Usuario   = builder:Usuario     
            this-object:Senha     = builder:Senha    
            this-object:Nome      = builder:Nome     
            this-object:Sobrenome = builder:Sobrenome     
            this-object:Email     = builder:Email.     
    end constructor.

end class.