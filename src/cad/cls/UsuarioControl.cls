 
/*------------------------------------------------------------------------
   File        : UsuarioControl
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Lucas Bicalho
   Created     : Sun Sep 17 00:40:00 BRT 2023
   Notes       : 
 ----------------------------------------------------------------------*/

using src.cad.cls.UsuarioBuilder.

block-level on error undo, throw.

class src.cad.cls.UsuarioControl: 
    
    define public property cReturn as character no-undo 
    get.
    private set. 

    define public property cUsuarioAsString as character no-undo 
    get.
    private set.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character cadastrar(input pUsuario as UsuarioBuilder):
        
        init().
        
        if usuarioExistente(pUsuario) then do:
            cReturn = substitute("Erro! Usuário existente! (&1)", pUsuario:Usuario).
            return cReturn.
        end.
       
        create usuario.
        assign
            usuario.usuario   = pUsuario:Usuario
            usuario.nome      = pUsuario:Nome
            usuario.sobrenome = pUsuario:Sobrenome
            usuario.email     = pUsuario:Email
            usuario.senha     = encode(pUsuario:Senha) //MESSAGE-DIGEST("SHA-512", mydata, hashkey)
            usuario.ativo     = pUsuario:Ativo.       
        
        cReturn = substitute("Usuário &1 cadastrado com sucesso!", pUsuario:Usuario).
        
        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
            return cReturn.
        end finally.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character excluir(input pUsuario as UsuarioBuilder):
        
        init().
        
        if not usuarioExistente(pUsuario) then do:
            cReturn = substitute("Erro! Usuário não encontrado! (&1)", pUsuario:Usuario).
            return cReturn.
        end.
            
        find usuario exclusive-lock
            where usuario.usuario = pUsuario:Usuario.
            
        delete usuario.
        release usuario.
        
        cReturn = substitute("Usuário &1 excluído com sucesso!", pUsuario:Usuario).
        
        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
        end finally.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character alterar(input pUsuario as UsuarioBuilder):
        
        init().
        
        if not usuarioExistente(pUsuario) then do:
            cReturn = substitute("Erro! Usuário não encontrado! (&1)", pUsuario:Usuario).
            return cReturn.
        end.
            
        find usuario exclusive-lock
            where usuario.usuario = pUsuario:Usuario.
            
        assign
            usuario.nome      = pUsuario:Nome
            usuario.sobrenome = pUsuario:Sobrenome
            usuario.email     = pUsuario:Email.     
        release usuario.
        
        cReturn = substitute("Usuário &1 alterado com sucesso!", pUsuario:Usuario).
        
        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
            return cReturn.
        end finally.

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character mudarSenha(input pUsuario as UsuarioBuilder,
                                  input pSenhaAtual as character,
                                  input pSenhaNova   as character):
        
        init().
        
        if not usuarioExistente(pUsuario) then do:
            cReturn = substitute("Erro! Usuário não encontrado! (&1)", pUsuario:Usuario).
            return cReturn.
        end.
        
        find usuario exclusive-lock
            where usuario.usuario = pUsuario:Usuario.
            
        if encode(pSenhaAtual) = usuario.senha
        then do:
            assign 
                usuario.senha = encode(pSenhaNova).
            cReturn = substitute("Senha do usuário &1 alterada com sucesso!", pUsuario:Usuario).
        end.        
        else do:
            cReturn = "Senha informada não condiz com senha atual!".
        end.

        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
            release usuario.
            return cReturn.
        end finally.

    end method.    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character inativar(input pUsuario as UsuarioBuilder):
        
        init().
        
        if not usuarioExistente(pUsuario) then do:
            cReturn = substitute("Erro! Usuário não encontrado! (&1)", pUsuario:Usuario).
            return cReturn.
        end.
        
        find usuario exclusive-lock
            where usuario.usuario = pUsuario:Usuario.
        
        assign 
            usuario.ativo           = false
            usuario.senha           = encode("inativo")
            usuario.data-desativado = today.
        release usuario.
        
        cReturn = substitute("Usuário &1 inativado com sucesso!", pUsuario:Usuario).
        
        catch e as Progress.Lang.Error:
            message substitute("Erro! &1 &2", 
                                e:GetMessageNum(1),
                                e:GetMessage(1))
            view-as alert-box.
        end catch.

        finally:
            return cReturn.
        end finally.

    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private logical usuarioExistente(input pUsuario as UsuarioBuilder):
        
        if can-find(usuario no-lock 
                    where usuario.usuario = pUsuario:Usuario)
            then return true.
        
        return false.

    end method.    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method private void init(  ):
        
        assign 
            cReturn             = ""
            cUsuarioAsString    = "".

    end method.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public character imprimir(input pUsuario as UsuarioBuilder):
        
        if usuarioExistente(pUsuario) then do:
            find usuario no-lock
                where usuario.usuario = pUsuario:usuario.     
                
            cUsuarioAsString = substitute("&1 &2 &3 &4 &5 &6",
                                          usuario.usuario,  
                                          usuario.nome,     
                                          usuario.sobrenome,
                                          usuario.email,    
                                          usuario.ativo,    
                                          usuario.senha).
        
        end. 
        else cUsuarioAsString = "Usuário não encontrado!".
        
        finally:
            return cUsuarioAsString.
        end finally.

    end method.
        
end class.