import Lake
open Lake DSL

package «namingLint» {
  -- add package configuration options here
}

@[default_target]
lean_lib «NamingLint» 



require std from git "https://github.com/leanprover/std4" @ "main"

-- require Unicode from git
--   "https://github.com/xubaiw/Unicode.lean" @ "main" 