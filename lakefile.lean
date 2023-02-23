import Lake
open Lake DSL

package «lean-naming-linter» {
  -- add package configuration options here
}

lean_lib «LeanNamingLinter» {
  -- add library configuration options here
}

@[default_target]
lean_exe «lean-naming-linter» {
  root := `Main
}


require std from git "https://github.com/leanprover/std4" @ "main"

-- require Unicode from git
--   "https://github.com/xubaiw/Unicode.lean" @ "main" 