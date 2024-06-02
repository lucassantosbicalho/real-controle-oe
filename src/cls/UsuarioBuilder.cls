 
 /*------------------------------------------------------------------------
    File        : UsuarioBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Mon Sep 11 17:44:53 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cls.Usuario.

class src.cls.UsuarioBuilder: 

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
    set. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public UsuarioBuilder (input pUsuario    as character,
                                       input pSenha      as character,
                                       input pNome       as character,
                                       input pSobrenome  as character,
                                       input pEmail      as character):
        assign 
            this-object:Usuario   = pUsuario  
            this-object:Senha     = pSenha    
            this-object:Nome      = pNome     
            this-object:Sobrenome = pSobrenome
            this-object:Email     = pEmail 
            this-object:Ativo     = true. 
            
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public src.cls.UsuarioBuilder alterarNome(input pNome as character):
        assign
            this-object:Nome = pNome.
        return this-object.

    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public src.cls.UsuarioBuilder alterarSobrenome(input pSobrenome as character):
        assign
            this-object:Sobrenome = pSobrenome.
        return this-object.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public Usuario build(  ):
        return new Usuario(this-object).
    end method.    
    

end class.