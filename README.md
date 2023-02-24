# NamingLint

A linter for Lean 4 that checks whether definitions conform with Mathlib4's naming convention 

## Usage

To import, add to your lakefile:
```lean
require namingLint from git "https://github.com/alexkeizer/lean-naming-lint" @ "master"
```

The lint is added to the standard set of lints, so you only have to 

```lean
import NamingLint
```

Before linting as usual
```lean
#lint
```
