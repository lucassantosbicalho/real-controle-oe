 
 /*------------------------------------------------------------------------
    File        : MovimentoBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Lucas Bicalho
    Created     : Sat Sep 09 17:49:16 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using src.cad.cls.Movimento.

class src.cad.cls.MovimentoBuilder: 
    
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

    /* opcionais */    
    define public property Narrativa as character no-undo
    get.
    private set.
    
    define public property DataCriacao as date no-undo
    get.
    private set.
    
    define public property HoraCriacao as character no-undo
    get.
    private set.
    
    define private property Descricao as character no-undo
    get.
    set.
        
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public MovimentoBuilder (input pDataMvto    as date,
                                         input pCcCod       as character,
                                         input pConta       as character,
                                         input pItCod       as character,
                                         input pSeq         as integer,
                                         input pMovtoTp     as integer,
                                         input pValor       as decimal,
                                         input pUsuario     as character):
        assign
            this-object:DataMvto = pDataMvto      
            this-object:CcCod    = pCcCod   
            this-object:Conta    = pConta   
            this-object:ItCod    = pItCod   
            this-object:Seq      = pSeq 
            this-object:MovtoTp  = pMovtoTp     
            this-object:Valor    = pValor   
            this-object:Usuario  = pUsuario.
    end constructor.
    
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    
    method public src.cad.cls.MovimentoBuilder setNarrativa(input pNarrativa as character):
        assign
            this-object:Narrativa = pNarrativa.
    
        return this-object.
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    
    method public src.cad.cls.MovimentoBuilder setDataCriacao(input pDataCriacao as date):
        assign
            this-object:DataCriacao = pDataCriacao.
    
        return this-object.
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    
    method public src.cad.cls.MovimentoBuilder setHoraCriacao(input pHoraCriacao as character):
        assign
            this-object:HoraCriacao = pHoraCriacao.
    
        return this-object.
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    
    method public src.cad.cls.MovimentoBuilder setDescricao(input pDescricao as character):
        assign
            this-object:Descricao = pDescricao.
    
        return this-object.
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public src.cad.cls.Movimento build(  ):
        return new Movimento(this-object).            
    end method.
    

end class.