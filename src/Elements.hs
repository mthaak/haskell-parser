{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Elements where

import Tokens (Token)

-- MISC ELEMENTS
data All = All -- (..)
  deriving (Eq, Show)

-- TERMINAL ELEMENTS

data Literal
  = Literal_Int LitInteger
  | Literal_Float LitFloat
  | Literal_Char LitChar
  | Literal_String LitString
  deriving (Eq, Show)

data Symbol = Symbol Token
  deriving (Eq, Show)

-- string starting with lowercase
data VarId = VarId String
  deriving (Eq, Show)

-- string starting with uppercase
data ConId = ConId String
  deriving (Eq, Show)

-- Variable symbol
data VarSym = VarSym String -- TODO
  deriving (Eq, Show)

-- Constructor symbol
data ConSym = ConSym String -- TODO
  deriving (Eq, Show)

-- Type variable
type TyVar = VarId

-- Type constructor
type TyCon = ConId

-- Type class
type TyCls = ConId

-- Module ID
type ModId = ConId

-- TODO qualified elements

-- Qualified variable ID
-- TODO qvarid	→	[ modid . ] varid
type QVarId = VarId

-- Qualified module ID
-- TODO qconid	→	[ modid . ] conid
type QConId = ConId

-- Qualified type constructor
-- TODO qtycon	→	[ modid . ] tycon
type QTyCon = TyCon

-- Qualified type class
-- TODO qtycls	→	[ modid . ] tycls
type QTyCls = TyCls

-- Qualified variable symbol
-- TODO qvarsym	→	[ modid . ] varsym
type QVarSym = VarSym

-- Qualified constructor symbol
-- TODO qconsym	→	[ modid . ] consym
type QConSym = ConSym

-- Integer literal
data LitInteger = LitInteger String
  deriving (Eq, Show)

-- Float literal
data LitFloat = LitFloat String
  deriving (Eq, Show)

-- Char literal
data LitChar = LitChar String
  deriving (Eq, Show)

-- String literal
data LitString = LitString String
  deriving (Eq, Show)

-- NON-TERMINAL ELEMENTS

data Module = Module ModId (Maybe [Export]) Body
  deriving (Eq, Show)

data Body = Body TopDecls
  deriving (Eq, Show)

type Exports = [Export]

data Export
  = Export_QVar QVar
  | Export_QTyCon QTyCon (Either All [CName])
  | Export_QTyCls QTyCls (Either All [QVar])
  | Export_Module ModId
  deriving (Eq, Show)

type CName = Either Var Con

type TopDecls = [TopDecl]

data TopDecl
  = TopDecl_Type SimpleType Type
  | TopDecl_Data (Maybe Context) SimpleType (Maybe Constrs) (Maybe Deriving)
  | TopDecl_NewType (Maybe Context) SimpleType NewConstr (Maybe Deriving)
  | TopDecl_Class (Maybe SContext) TyCls TyVar (Maybe CDecls)
  | TopDecl_Instance (Maybe SContext) QTyCls Inst (Maybe IDecls)
  | TopDecl_Default [Type]
  | --  | TopDecl_Foreign [FDecl] -- TODO
    TopDecl_Decl Decl
  deriving (Eq, Show)

type Decls = [Decl]

data Decl
  = Decl_GenDecl GenDecl
  | Decl_FunLhs (Either FunLhs Pat) Rhs
  deriving (Eq, Show)

type CDecls = [CDecl]

data CDecl
  = CDecl_GenDecl GenDecl
  | CDecl_FunLhs (Either FunLhs Var) Rhs
  deriving (Eq, Show)

type IDecls = [IDecl]

data IDecl
  = IDecl_FunLhs (Either FunLhs Var) Rhs
  deriving (Eq, Show)

data GenDecl
  = GenDecl_TypeSig Vars (Maybe Context) Type
  deriving (Eq, Show)

-- Operators
type Ops = [Op]

-- Variables
type Vars = [Var]

-- Function type
-- TODO make recursive?
data Type = Type [BType]
  deriving (Eq, Show) -- TODO

-- Function application
-- TODO make recursive?
data BType = BType [AType]
  deriving (Eq, Show)

data AType
  = AType_GTyCon GTyCon
  | AType_TyVar TyVar
  | AType_Tuple [Type]
  | AType_List Type
  | AType_ParanCon Type
  deriving (Eq, Show)

data GTyCon
  = GTyCon_QTyCon QTyCon
  | GTyCon_Unit
  | GTyCon_ListCon
  | GTyCon_FunCon
  | GTyCon_TupleCon
  deriving (Eq, Show) -- TODO

data Context = Context [Class]
  deriving (Eq, Show) -- TODO

-- Class
data Class = Class TyCls TyVar
  deriving (Eq, Show) -- TODO

data SContext = SContext [SimpleClass]
  deriving (Eq, Show) -- TODO

data SimpleClass
  = SimpleClass QTyCls TyVar
  deriving (Eq, Show)

data SimpleType = SimpleType TyCon [TyVar]
  deriving (Eq, Show)

type Constrs = [Constr]

data Constr
  = Constr_ATypes Con [AType]
  | --  | Constr_Infix -- TODO
    Constr_FieldDecls Con [FieldDecl]
  deriving (Eq, Show)

data NewConstr
  = NewConstr_AType Con AType
  | NewConstr_Type Con Var Type
  deriving (Eq, Show)

data FieldDecl = FieldDecl Vars (Either Type AType)
  deriving (Eq, Show)

data Deriving = Deriving [DClass]
  deriving (Eq, Show)

data DClass = DClass QTyCls
  deriving (Eq, Show)

data Inst
  = Inst_GTyCon GTyCon
  | Inst_GQTyConTyVars GTyCon [TyVar]
  | Inst_TyVarsTuple [TyVar]
  | Inst_TyVarsList [TyVar]
  | Inst_TyVar2 TyVar TyVar
  deriving (Eq, Show)

-- Function left hand side
data FunLhs = FunLhs_Var Var [APat] -- TODO
  deriving (Eq, Show)

-- Right hand side
data Rhs = Rhs_Exp Exp (Maybe Decls)
  deriving (Eq, Show)

-- Expression
-- TODO exp	→	infixexp :: [context =>] type	    (expression type signature)
data Exp
  = Exp_InfixExp InfixExp
  deriving (Eq, Show)

data InfixExp -- TODO
  = InfixExp_Infix LExp QOp InfixExp
  | InfixExp_Neg InfixExp
  | InfixExp_LExp LExp
  deriving (Eq, Show)

data LExp -- TODO
  = LExp_FExp FExp
  deriving (Eq, Show)

-- Function application
-- TODO turn this into the left-recursive definition fexp	→	[fexp] aexp
data FExp = FExp AExp [AExp]
  deriving (Eq, Show)

data AExp -- TODO
  = AExp_QVar QVar
  | AExp_GCon GCon
  | AExp_Lit Literal
  | AExp_ParanExp Exp
  | AExp_Tuple [Exp]
  | AExp_List [Exp]
  | -- TODO:
    --  | AExp_ArithSeq [Exp]
    --  | AExp_ListComp Exp [Qual]
    AExp_LeftSect InfixExp QOp
  | AExp_RightSect QOp InfixExp
  | AExp_LabelCon QCon [FBind]
  --  | AExp_LabelUpd AExp [FBind] -- TODO
  deriving (Eq, Show)

--  qual	→	pat <- exp	    (generator)
--  |	let decls	    (local declaration)
--  |	exp	    (guard)
--
--  alts	→	alt1 ; … ; altn	    (n ≥ 1)
--  alt	→	pat -> exp [where decls]
--  |	pat gdpat [where decls]
--  |		    (empty alternative)
--
--  gdpat	→	guards -> exp [ gdpat ]
--
--  stmts	→	stmt1 … stmtn exp [;]	    (n ≥ 0)
--  stmt	→	exp ;
--  |	pat <- exp ;
--  |	let decls ;
--  |	;	    (empty statement)
--
--  fbind	→	qvar = exp
data FBind = FBind QVar Exp
  deriving (Eq, Show)

--  pat	→	lpat qconop pat	    (infix constructor)
--  |	lpat

data Pat
  = Pat_LPat LPat -- TODO
  deriving (Eq, Show)

--  lpat	→	apat
--  |	- (integer | float)	    (negative literal)
--  |	gcon apat1 … apatk	    (arity gcon  =  k, k ≥ 1)

data LPat
  = LPat_APat APat -- TODO
  deriving (Eq, Show)

--  apat	→	var [ @ apat]	    (as pattern)
--  |	gcon	    (arity gcon  =  0)
--  |	qcon { fpat1 , … , fpatk }	    (labeled pattern, k ≥ 0)
--  |	literal
--  |	_	    (wildcard)
--  |	( pat )	    (parenthesized pattern)
--  |	( pat1 , … , patk )	    (tuple pattern, k ≥ 2)
--  |	[ pat1 , … , patk ]	    (list pattern, k ≥ 1)
--  |	~ apat	    (irrefutable pattern)
data APat -- TODO
  = APat_Var Var (Maybe APat)
  | APat_GCon GCon
  | APat_QCon QCon [FPat]
  | APat_Lit Literal
  deriving (Eq, Show)

--  fpat	→	qvar = pat
data FPat
  = FPat QVar Pat
  deriving (Eq, Show)

--  gcon	→	  ()
--        |	  []
--        |	  (,{,})
--        |	  qcon
data GCon
  = GCon_Nop
  | GCon_QCon QCon
  deriving (Eq, Show)

-- Variable
data Var = Var VarId -- TODO
  deriving (Eq, Show)

-- Qualified variable
data QVar = QVar QVarId -- TODO
  deriving (Eq, Show)

-- Constructor
data Con = Con ConId -- TODO
  deriving (Eq, Show)

-- Qualified constructor
data QCon = QCon QConId -- TODO
  deriving (Eq, Show)

-- Variable operator
data VarOp = VarOp VarSym -- TODO
  deriving (Eq, Show)

-- Qualified variable operator
data QVarOp = QVarOp QVarSym -- TODO
  deriving (Eq, Show)

-- Constructor operator
data ConOp = ConOp ConSym -- TODO
  deriving (Eq, Show)

-- Qualified constructor operator
data QConOp = QConOp QConSym -- TODO
  deriving (Eq, Show)

-- Operator
data Op
  = Op_VarOp VarOp
  | Op_ConOp ConOp
  deriving (Eq, Show)

-- Qualified operator
data QOp
  = QOp_QVarOp QVarOp
  | QOp_QConOp QConOp
  deriving (Eq, Show)

data Nop = Nop
