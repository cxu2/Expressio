(*
 * File: exceptions.ml
 * Date: 2017-03-13
 *
 * PLT Spring 2018
 * Expressio Project
 * Ian Treyball
 * Lalka Rieger
 * Chengtian Xu
 * David Han
 *)


(* Analyzer Exceptions *)
exception AllVoidFunctionsMustNotReturn of string
exception AllNonVoidFunctionsMustEndWithReturn of string
exception AssignmentTypeMismatch of string * string
exception CannotUseRowsOnNonMatrix of string
exception CannotUseTransposeOnNonMatrix of string
exception CannotUseColsOnNonMatrix of string
exception CannotUseLenOnNonVector of string
exception DuplicateGlobal of string
exception DuplicateFuncOrLocal of string 
exception FunctionNotFound of string
exception IncorrectNumberOfArguments of string * int * int
exception InvalidBinopExpression of string
exception InvalidUnaryOperation
exception MalformedMatrixLit

exception MismatchedMatricesForAddSub of string
exception MismatchedMatricesForMult of string
exception MismatchedVectorsForBinop of string
exception ReturnTypeMismatch of string * string
exception UndefinedID of string
exception UnsupportedRegexpBinop of string
exception VoidFunctionLocal of string
exception VoidFunc of string

(* Codegen Exceptions *)
exception AssignLHSMustBeAssignable
exception DecMustBeCalledOnID
exception IllegalBoolBinop
exception IllegalBoolUnop
exception IllegalIntUnop
exception IllegalCast
exception IllegalFloatBinop
exception IllegalFloatUnop
exception IllegalIntBinop
exception IllegalMatrixBinop
exception IllegalVectorBinop
exception IncMustBeCalledOnID
exception InvalidMatrixDimension
exception InvalidUnopType
exception InvalidVectorDimension
exception UnsupportedBinopType
exception UnsupportedMatrixType
exception UnsupportedVectorType
exception VectorOutOfBoundsAccess of string
exception UnsupportedType
exception LocalNotFound of string
exception GlobalVarNotFound of string
exception StatementNotSuuported
exception IllegalArgument of string
