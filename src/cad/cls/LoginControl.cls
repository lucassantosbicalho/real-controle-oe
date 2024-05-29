 
 /*------------------------------------------------------------------------
    File        : LoginControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sun Sep 17 14:00:46 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cad.cls.UsuarioBuilder.

block-level on error undo, throw.

class src.cad.cls.LoginControl: 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public logical logar(input pUsuario as character,
                                input pSenha   as character):
        
        if not usuarioExistente(pUsuario) then return false.
        
        find usuario exclusive-lock
            where usuario.usuario = pUsuario.
        
        if (usuario.senha = pSenha) then do:
            /*message "Implementar log de ultimo login"
            view-as alert-box.*/ 
            return true.
        end.
        
        return false.
        
        finally:
            release usuario.    
        end finally.

    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private logical usuarioExistente(input pUsuario as character):
        
        if can-find(usuario no-lock 
                    where usuario.usuario = pUsuario)
            then return true.
        
        return false.

    end method.

end class.