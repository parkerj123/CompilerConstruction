-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParCpp where
import AbsCpp
import LexCpp
import ErrM

}

%name pProgram Program
%name pFunction Function
%name pDecl Decl
%name pListFunction ListFunction
%name pListStm ListStm
%name pListDecl ListDecl
%name pListIdent ListIdent
%name pListOut ListOut
%name pListIn ListIn
%name pStm Stm
%name pName Name
%name pOut Out
%name pIn In
%name pExp Exp
%name pExp1 Exp1
%name pExp2 Exp2
%name pExp3 Exp3
%name pExp4 Exp4
%name pListExp ListExp
%name pType Type
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '&' { PT _ (TS _ 2) }
  '&&' { PT _ (TS _ 3) }
  '(' { PT _ (TS _ 4) }
  ')' { PT _ (TS _ 5) }
  '*' { PT _ (TS _ 6) }
  '+' { PT _ (TS _ 7) }
  '++' { PT _ (TS _ 8) }
  ',' { PT _ (TS _ 9) }
  '-' { PT _ (TS _ 10) }
  '->' { PT _ (TS _ 11) }
  '.' { PT _ (TS _ 12) }
  ':' { PT _ (TS _ 13) }
  '::' { PT _ (TS _ 14) }
  '::size_type' { PT _ (TS _ 15) }
  ';' { PT _ (TS _ 16) }
  '<' { PT _ (TS _ 17) }
  '<<' { PT _ (TS _ 18) }
  '<=' { PT _ (TS _ 19) }
  '=' { PT _ (TS _ 20) }
  '==' { PT _ (TS _ 21) }
  '>=' { PT _ (TS _ 22) }
  '>>' { PT _ (TS _ 23) }
  '?' { PT _ (TS _ 24) }
  '[' { PT _ (TS _ 25) }
  ']' { PT _ (TS _ 26) }
  'bool' { PT _ (TS _ 27) }
  'double' { PT _ (TS _ 28) }
  'if' { PT _ (TS _ 29) }
  'int' { PT _ (TS _ 30) }
  'return' { PT _ (TS _ 31) }
  'std::string' { PT _ (TS _ 32) }
  'string' { PT _ (TS _ 33) }
  'typedef' { PT _ (TS _ 34) }
  'using' { PT _ (TS _ 35) }
  'vec_sz' { PT _ (TS _ 36) }
  'vector<double>' { PT _ (TS _ 37) }
  'while' { PT _ (TS _ 38) }
  '{' { PT _ (TS _ 39) }
  '||' { PT _ (TS _ 40) }
  '}' { PT _ (TS _ 41) }

L_ident  { PT _ (TV $$) }
L_quoted { PT _ (TL $$) }
L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
String  :: { String }  : L_quoted {  $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }

Program :: { Program }
Program : ListFunction { AbsCpp.Prog (reverse $1) }
Function :: { Function }
Function : Type Ident '(' ListDecl ')' '{' ListStm '}' { AbsCpp.Func $1 $2 $4 (reverse $7) }
Decl :: { Decl }
Decl : Type ListIdent { AbsCpp.Decl $1 $2 }
ListFunction :: { [Function] }
ListFunction : {- empty -} { [] }
             | ListFunction Function { flip (:) $1 $2 }
ListStm :: { [Stm] }
ListStm : {- empty -} { [] } | ListStm Stm { flip (:) $1 $2 }
ListDecl :: { [Decl] }
ListDecl : {- empty -} { [] }
         | Decl { (:[]) $1 }
         | Decl ',' ListDecl { (:) $1 $3 }
ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } | Ident ',' ListIdent { (:) $1 $3 }
ListOut :: { [Out] }
ListOut : Out { (:[]) $1 } | Out '<<' ListOut { (:) $1 $3 }
ListIn :: { [In] }
ListIn : In { (:[]) $1 } | In '>>' ListIn { (:) $1 $3 }
Stm :: { Stm }
Stm : Decl ';' { AbsCpp.SDecl $1 }
    | Exp ';' { AbsCpp.SExp $1 }
    | '{' ListStm '}' { AbsCpp.SBlock (reverse $2) }
    | 'while' '(' ListExp ')' ListStm { AbsCpp.SWhile $3 (reverse $5) }
    | 'return' Exp ';' { AbsCpp.SReturn $2 }
    | 'using' Stm ';' { AbsCpp.SUsing $2 }
    | Name ';' { AbsCpp.SConst $1 }
    | ListOut ';' { AbsCpp.SOut $1 }
    | ListIn ';' { AbsCpp.SIn $1 }
    | 'if' '(' Exp ')' '{' ListStm '}' { AbsCpp.SIf $3 (reverse $6) }
Name :: { Name }
Name : Exp '::' Ident { AbsCpp.Name $1 $3 }
Out :: { Out }
Out : Name { AbsCpp.Out $1 } | Exp { AbsCpp.OutExp $1 }
In :: { In }
In : Name { AbsCpp.In $1 } | Exp { AbsCpp.InExp $1 }
Exp :: { Exp }
Exp : Ident '.' Ident '(' ListExp ')' ';' { AbsCpp.EFunc $1 $3 $5 }
    | Ident '=' Exp { AbsCpp.EAss $1 $3 }
    | Exp '>>' Exp { AbsCpp.ESR $1 $3 }
    | Exp '<<' Exp { AbsCpp.ESL $1 $3 }
    | Type ListExp '::' Type '::size_type' Ident { AbsCpp.ETStd $1 $2 $4 $6 }
    | Exp '?' Exp ':' Exp { AbsCpp.EQues $1 $3 $5 }
    | Exp '==' Exp { AbsCpp.EEquiv $1 $3 }
    | Ident '[' Exp ']' { AbsCpp.EArr $1 $3 }
    | Ident '&' { AbsCpp.EAmp $1 }
    | '!' Exp { AbsCpp.EExcl $2 }
    | Exp '<=' Exp { AbsCpp.ELesEq $1 $3 }
    | Exp '>=' Exp { AbsCpp.EGreEq $1 $3 }
    | '*' Ident { AbsCpp.EPoint $2 }
    | Exp '&&' Exp { AbsCpp.EAnd $1 $3 }
    | Exp '->' Exp { AbsCpp.EArrow $1 $3 }
    | '++' Exp { AbsCpp.EPreInc $2 }
    | Exp '||' Exp { AbsCpp.EOr $1 $3 }
    | Exp1 { $1 }
    | Type Exp { AbsCpp.ETy $1 $2 }
Exp1 :: { Exp }
Exp1 : Exp2 '<' Exp2 { AbsCpp.ELess $1 $3 } | Exp2 { $1 }
Exp2 :: { Exp }
Exp2 : Exp2 '+' Exp3 { AbsCpp.EAdd $1 $3 }
     | Exp2 '-' Exp3 { AbsCpp.ESub $1 $3 }
     | Exp3 { $1 }
Exp3 :: { Exp }
Exp3 : Exp3 '*' Exp4 { AbsCpp.EMul $1 $3 } | Exp4 { $1 }
Exp4 :: { Exp }
Exp4 : Ident '(' ListExp ')' { AbsCpp.EType $1 $3 }
     | Ident { AbsCpp.EIdent $1 }
     | String { AbsCpp.EStr $1 }
     | Integer { AbsCpp.EInt $1 }
     | Double { AbsCpp.EDoub $1 }
     | '(' Exp ')' { $2 }
ListExp :: { [Exp] }
ListExp : {- empty -} { [] }
        | Exp { (:[]) $1 }
        | Exp ',' ListExp { (:) $1 $3 }
Type :: { Type }
Type : 'int' { AbsCpp.Type }
     | 'double' { AbsCpp.Type }
     | 'string' { AbsCpp.Type }
     | 'vec_sz' { AbsCpp.Type }
     | 'std::string' { AbsCpp.Type }
     | 'vector<double>' { AbsCpp.Type }
     | 'typedef' { AbsCpp.Type }
     | 'bool' { AbsCpp.Type }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}
