 
 /*------------------------------------------------------------------------
    File        : MovimentoControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Wed Sep 27 21:46:22 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cls.MovimentoBuilder.

block-level on error undo, throw.

class src.cls.MovimentoControl: 

    define public property cReturn as character no-undo 
    get.
    private set.
    
    define private property pMov as MovimentoBuilder no-undo
    private get.
    private set.

    /*------------------------------------------------------------------------------
     Purpose: 
     Notes: Para o movimento, não há duplicidade de registro, visto que 
            é natural haver registros iguais lançados várias vezes
            para uma mesma data, conta, item, etc. por isso
            não é usado movtoExistente().
    ------------------------------------------------------------------------------*/

    method public character cadastrar(input pData-mvto as date,   
                                      input pBanco-cod as character,
                                      input pAg        as character,
                                      input pConta     as character,
                                      input pCc-cod    as character,
                                      input pIt-cod    as character,
                                      input pSeq       as integer,
                                      input pDescricao as character,
                                      input pMovto-tp  as integer,
                                      input pNarrativa as character,
                                      input pUsuario   as character,
                                      input pValor     as decimal):
        
        init().
        
        create movto.
        assign 
            movto.id           = next-value(mvtoID)
            movto.data-mvto    = pData-mvto 
            movto.banco-cod    = pBanco-cod 
            movto.ag           = pAg        
            movto.conta        = pConta     
            movto.cc-cod       = pCc-cod    
            movto.it-cod       = pIt-cod    
            movto.seq          = pSeq       
            movto.descricao    = pDescricao 
            movto.movto-tp     = pMovto-tp  
            movto.narrativa    = pNarrativa 
            movto.usuario      = pUsuario   
            movto.valor        = pValor     
            movto.created-at   = datetime(today,mtime).
            
        cReturn = "Movimento cadastrado com sucesso!".
         
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

    method public character excluir(input pId as int64):
        
        if not can-find(movto no-lock
                            where movto.id = pId) then do:
            cReturn = substitute("Erro! Movto inexistente! (id &1)", pId).
            return cReturn.                        
        end.
        
        find movto exclusive-lock
            where movto.id = pId.
        delete movto.
        release movto.
        
        cReturn = "Movimento excluído com sucesso!".
         
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

    method public character alterar(input pId        as int64,
                                    input pData-mvto as date,   
                                    input pBanco-cod as character,
                                    input pAg        as character,
                                    input pConta     as character,
                                    input pCc-cod    as character,
                                    input pIt-cod    as character,
                                    input pSeq       as integer,
                                    input pDescricao as character,
                                    input pMovto-tp  as integer,
                                    input pNarrativa as character,
                                    input pUsuario   as character,
                                    input pValor     as decimal):
        
        init().
        
        if not can-find(movto no-lock
                            where movto.id = pId) then do:
            cReturn = substitute("Erro! Movto inexistente! (id &1)", pId).
            return cReturn.                        
        end.
        
        find movto exclusive-lock
            where movto.id = pId.

        assign 
            movto.data-mvto    = (if movto.data-mvto <> pData-mvto then pData-mvto else movto.data-mvto)
            movto.banco-cod    = (if movto.banco-cod <> pBanco-cod then pBanco-cod else movto.banco-cod)
            movto.ag           = (if movto.ag        <> pAg        then pAg        else movto.ag       )
            movto.conta        = (if movto.conta     <> pConta     then pConta     else movto.conta    )
            movto.cc-cod       = (if movto.cc-cod    <> pCc-cod    then pCc-cod    else movto.cc-cod   )
            movto.it-cod       = (if movto.it-cod    <> pIt-cod    then pIt-cod    else movto.it-cod   )
            movto.seq          = (if movto.seq       <> pSeq       then pSeq       else movto.seq      )
            movto.descricao    = (if movto.descricao <> pDescricao then pDescricao else movto.descricao)
            movto.movto-tp     = (if movto.movto-tp  <> pMovto-tp  then pMovto-tp  else movto.movto-tp )
            movto.narrativa    = (if movto.narrativa <> pNarrativa then pNarrativa else movto.narrativa)
            movto.usuario      = (if movto.usuario   <> pUsuario   then pUsuario   else movto.usuario  )
            movto.valor        = (if movto.valor     <> pValor     then pValor     else movto.valor    ).
        release movto.
            
        cReturn = "Movimento alterado com sucesso!".
         
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

    method private void init(  ):
        
        assign 
            cReturn             = "".

    end method.
    

end class.