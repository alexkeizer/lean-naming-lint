import Std

open Lean (Name FVarId getConstInfo MessageData MonadEnv)
open Lean.Meta
open Std.Tactic.Lint


namespace NamingConvention
  open Lean

  /-- Checks that `name` is in snake_case -/
  def snakeCaseTest (name : String) (reason : String) : MetaM (Option MessageData) := do
    let chars := name.toList
    let parts := (chars.splitOn '_').filter (fun x => !x.isEmpty)

    if parts.length == 1 then
      -- There are no `_`, so there should not be any upper case characters
      if chars.any Char.isUpper then
        return m!"`{name}` should be in snake_case ({reason})"
      else
        return none
    else
      let errors := List.reduceOption <| parts.map fun p =>
        if (p.get! 0).isUpper then
          some p
        else
          none

      if errors.isEmpty then
        return none
      else
        let errors := String.join <| errors.map fun error => 
          "\n * " ++ String.mk error ++ " should be in lowerCamelCase"
        return m!
          "When something named with UpperCamelCase is part of something named with snake_case,"
           ++ m!"it is referenced in lowerCamelCase ({reason})\n{errors}"


  /-- Checks that `name` is in lowerCamelCase, i.e., the first character is lowercase, and there
      are no `_` characters
   -/
  def lowerCamelCaseTest (name : String) (reason : String) : MetaM (Option MessageData) := do
    let chars := name.toList

    if (chars.get! 0).isUpper || chars.contains '_' then
      return m!"`{name}` should be in lowerCamelCase ({reason})"
    else
      return none
 

  /-- Checks that `name` is in UpperCamelCase, i.e., the first character is Uppercase, and there
      are no `_` characters 
    -/
  def upperCamelCaseTest (name : String) (reason : String) : MetaM (Option MessageData) := do
    let chars := name.toList

    if (chars.get! 0).isLower || chars.contains '_' then
      return m!"`{name}` should be in UpperCamelCase ({reason})"
    else
      return none

  /--
    Checks that a `def` or `theorem` conforms to the naming convention

    https://github.com/leanprover-community/mathlib4/wiki/Porting-wiki#naming-convention
  -/
  def definitionTest (name : Name) (value : Expr) : MetaM (Option MessageData) := do
    if name.isInternal then
      return none

    -- Extract only the last element of the name
    let name ← match name with
      | .str _ name => pure name
      | _ => return none

    let type ← try 
      inferType value
    catch _ =>
      return none
    let type ← whnf type

    -- Functions are named the same way as their return values 
    -- (e.g. a function of type A → B → C is named as though it is a term of type C
    forallTelescopeReducing type fun _ type => do
      if type.isSort then
        upperCamelCaseTest name "Props and Types (or Sort) (inductive types, structures, classes) are in UpperCamelCase"
      else
        let typeOfType ← inferType type

        if typeOfType.isProp then 
          snakeCaseTest name "Terms of Props (e.g. proofs, theorem names) are in snake_case"
        else if typeOfType.isType then
          lowerCamelCaseTest name "Terms of Types (most definitions) are in lowerCamelCase"
        else
          pure none

end NamingConvention

open NamingConvention

/--
  Check whether the definition of the given name is consistent with the mathlib4 naming convention
-/
def namingConventionTest (name : Name) : MetaM (Option MessageData) := do
  let decl ← match (← MonadEnv.getEnv).find? name with
    | some decl => pure decl
    | none => return none

  match decl with
    | .defnInfo val => definitionTest name val.value
    | .thmInfo val  => definitionTest name val.value
    | _ => pure none


@[std_linter]
def namingConvention : Linter where
  test := namingConventionTest
  noErrorsFound := "No naming convention violations found"
  errorsFound := "Some definitions seem to be inconsistent with the naming convention"


#lint only namingConvention