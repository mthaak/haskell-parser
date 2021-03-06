{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Elements where

import Tokens (Token)

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
data VarSym = VarSym String
  deriving (Eq, Show)

-- Constructor symbol
data ConSym = ConSym String
  deriving (Eq, Show)

-- Type variable
type TyVar = VarId

-- Type constructor
type TyCon = ConId

-- Type class
type TyCls = ConId

-- Module ID
data ModId = ModId String
  deriving (Eq, Show)

-- Qualified variable ID
data QVarId = QVarId (Maybe ModId) VarId deriving (Eq, Show)

-- Qualified module ID
data QConId = QConId (Maybe ModId) ConId deriving (Eq, Show)

-- Qualified type constructor
data QTyCon = QTyCon (Maybe ModId) TyCon deriving (Eq, Show)

-- Qualified type class
data QTyCls = QTyCls (Maybe ModId) TyCls deriving (Eq, Show)

-- Qualified variable symbol
data QVarSym = QVarSym (Maybe ModId) VarSym
  deriving (Eq, Show)

-- Qualified constructor symbol
data QConSym = QConSym (Maybe ModId) ConSym
  deriving (Eq, Show)

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

data Body = Body ImpDecls TopDecls
  deriving (Eq, Show)

type ImpDecls = [ImpDecl]

type Exports = [Export]

data Export
  = Export_QVar QVar
  | Export_QTyCon QTyCon (Either All [CName])
  | Export_QTyCls QTyCls (Either All [QVar])
  | Export_Module ModId
  deriving (Eq, Show)

type Qualified = Bool

data ImpDecl = ImpDecl Qualified ModId (Maybe ModId) (Maybe ImpSpec)
  deriving (Eq, Show)

data ImpSpec
  = ImpSpec [Import]
  | ImpSpec_Hiding [Import]
  deriving (Eq, Show)

data Import
  = Import_Var Var
  | Import_TyCon TyCon (Maybe (Either All [CName]))
  | Import_TyCls TyCls (Maybe (Either All [Var]))
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
  | Decl_FunLhs FunLhs Rhs
  | Decl_Pat Pat Rhs
  deriving (Eq, Show)

type CDecls = [CDecl]

data CDecl
  = CDecl_GenDecl GenDecl
  | CDecl_FunLhs FunLhs Rhs
  | CDecl_Var Var Rhs
  deriving (Eq, Show)

type IDecls = [IDecl]

data IDecl
  = IDecl_FunLhs FunLhs Rhs
  | IDecl_Var Var Rhs
  deriving (Eq, Show)

data GenDecl
  = GenDecl_TypeSig Vars (Maybe Context) Type
  | GenDecl_Fixity Fixity (Maybe LitInteger) Ops
  | GenDecl_Empty
  deriving (Eq, Show)

-- Operators
type Ops = [Op]

-- Variables
type Vars = [Var]

-- Fixity
data Fixity
  = Fixity_Infixl
  | Fixity_Infixr
  | Fixity_Infix
  deriving (Eq, Show)

-- Function type
-- TODO make recursive?
data Type = Type [BType]
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Context = Context [Class]
  deriving (Eq, Show) -- TODO single class

-- Class
data Class = Class QTyCls TyVar
  deriving (Eq, Show) -- TODO multiple atypes

data SContext = SContext [SimpleClass]
  deriving (Eq, Show) -- TODO single simple class

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

-- TODO FDecl

-- TODO CallConv

-- TODO Impent

-- TODO Expent

-- TODO Safety

-- TODO FType

-- TODO FTType

-- TODO FAType

-- Function left hand side
data FunLhs
  = FunLhs_Var Var [APat]
  | FunLhs_Pat Pat VarOp Pat
  | FunLhs_Fun FunLhs [APat]
  deriving (Eq, Show)

-- Right hand side
data Rhs
  = Rhs_Exp Exp (Maybe Decls)
  | Rhs_GdRhs GdRhs (Maybe Decls)
  deriving (Eq, Show)

data GdRhs = GdRhs Guards Exp (Maybe GdRhs)
  deriving (Eq, Show)

type Guards = [Guard]

data Guard
  = Guard_Pat Pat InfixExp
  | Guard_Decls Decls
  | Guard_Infix InfixExp
  deriving (Eq, Show)

-- Expression
data Exp
  = Exp_InfixExp InfixExp
  -- TODO expression type signature
  deriving (Eq, Show)

data InfixExp
  = InfixExp_Infix LExp QOp InfixExp
  | InfixExp_Neg InfixExp
  | InfixExp_LExp LExp
  deriving (Eq, Show)

data LExp
  = LExp_Lambda [APat] Exp
  | LExp_Let Decls Exp
  | LExp_IfElse Exp Exp Exp
  | LExp_Case Exp Alts
  | LExp_Do Stmts
  | LExp_FExp FExp
  deriving (Eq, Show)

-- Function application
-- TODO turn this into the left-recursive definition fexp	???	[fexp] aexp
data FExp = FExp AExp [AExp]
  deriving (Eq, Show)

data AExp
  = AExp_QVar QVar
  | AExp_GCon GCon
  | AExp_Lit Literal
  | AExp_ParanExp Exp
  | AExp_Tuple [Exp]
  | AExp_List [Exp]
  | AExp_ArithSeq Exp (Maybe Exp) (Maybe Exp)
  | AExp_ListComp Exp [Qual]
  | AExp_LeftSect InfixExp QOp
  | AExp_RightSect QOp InfixExp
  | AExp_LabelCon QCon [FBind]
  --  | AExp_LabelUpd AExp [FBind] -- TODO labeled update
  deriving (Eq, Show)

data Qual
  = Qual_Gen Pat Exp
  | Qual_Local Decls
  | Qual_Guard Exp
  deriving (Eq, Show)

type Alts = [Alt]

data Alt
  = Alt_Exp Pat Exp (Maybe Decls)
  | Alt_GdPat Pat GdPat (Maybe Decls)
  | Alt_Empty
  deriving (Eq, Show)

data GdPat = GdPat Guards Exp (Maybe GdPat)
  deriving (Eq, Show)

data Stmts = Stmts [Stmt] Exp
  deriving (Eq, Show)

data Stmt
  = Stmt_Exp Exp
  | Stmt_Pat Pat Exp
  | Stmt_Decls Decls
  | Stmt_Empty
  deriving (Eq, Show)

data FBind = FBind QVar Exp
  deriving (Eq, Show)

data Pat
  = Pat_Infix LPat QConOp Pat
  | Pat_LPat LPat
  deriving (Eq, Show)

data LPat
  = LPat_APat APat
  | LPat_NegLit (Either LitInteger LitFloat)
  | LPat_GCon GCon [APat]
  deriving (Eq, Show)

data APat
  = APat_Var Var (Maybe APat)
  | APat_GCon GCon
  | APat_QCon QCon [FPat]
  | APat_Lit Literal
  | APat_Wildcard
  | APat_Paran Pat
  | APat_Tuple [Pat]
  | APat_List [Pat]
  | APat_Irrefut APat
  deriving (Eq, Show)

data FPat
  = FPat QVar Pat
  deriving (Eq, Show)

data GCon
  = GCon_Unit
  | GCon_List
  | GCon_Tuple
  | GCon_QCon QCon
  deriving (Eq, Show)

-- Variable
data Var
  = Var_VarId VarId
  | Var_VarSym VarSym
  deriving (Eq, Show)

-- Qualified variable
data QVar
  = QVar_QVarId QVarId
  | QVar_QVarSym QVarSym
  deriving (Eq, Show)

-- Constructor
data Con
  = Con_ConId ConId
  | Con_ConSym ConSym
  deriving (Eq, Show)

-- Qualified constructor
data QCon
  = QCon_QCondId QConId
  | QCon_QConSym QConSym
  deriving (Eq, Show)

-- Variable operator
data VarOp
  = VarOp_VarSym VarSym
  | VarOp_VarId VarId
  deriving (Eq, Show)

-- Qualified variable operator
data QVarOp
  = QVarOp_QVarSym QVarSym
  | QVarOp_QVarId QVarId
  deriving (Eq, Show)

-- Constructor operator
data ConOp
  = ConOp_ConSym ConSym
  | ConOp_ConId ConId
  deriving (Eq, Show)

-- Qualified constructor operator
data QConOp
  = QConOp_GConSym GConSym
  | QConOp_QConId QConId
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

data GConSym
  = GConSym_Colon
  | GConSym_QConSym QConSym
  deriving (Eq, Show)

-- MISC ELEMENTS
data All = All -- (..)
  deriving (Eq, Show)

data Nop = Nop
