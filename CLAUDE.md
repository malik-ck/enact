# CLAUDE.md — enact development guide

## Status
This package is mid-refactor. Assume nothing is final without reading the code.
- Several exported functions may still return NULL or have incomplete implementations
- Test coverage is growing but not exhaustive

## For AIs
Read every file in `R/` before making changes.

Place user-facing functions at the top of files, helpers below them, helpers-of-helpers further down. Do not define functions inside functions unless they are very brief anonymous functions.

It is preferrable for the code base to remain manageable in size. If the user requests the addition of a feature, it is unavoidable that you will add lines of code to the code base.
In such cases, please make sure you implement solutions that are economical regarding line counts.
If you do refactoring, ensure that the changes do not introduce unnecessary complexity or reduce code clarity. If it seems a new feature or refactor requires a lot of complexity to
implement, spell out to the user where these complexities lie and ask them to opt in explicitly. A refactor should be followed by a brief review regarding dead code.

## R-specific practices
- Generics: define with `UseMethod()`; register every method in NAMESPACE via `devtools::document()`
- Isolated closure envs: `new.env(parent = emptyenv())`; use `lockEnvironment()` to signal immutability
- Quoting: `alist()` for formal lists with missing defaults; `bquote()` for splicing values into expressions
- Validated string args: `match.arg()` at the top of the function
- R is copy-on-modify — never assume in-place mutation; always return modified objects
- Pre-allocate lists in loops: `vector("list", n)` rather than growing with `[[]]`

## What NOT to do
- Do not add type checks on learner prediction outputs
- Do not use `<<-`; use return values or explicit `assign(nm, val, envir = e)`
- Do not call `as.vector()` on data frame columns; use `df[[col]]`
- Do not use `pairlist()` with `quote(expr=)` for function formals; use `alist()`
- Do not inherit classes speculatively — only add a class if at least one method dispatches on it differently
