# gdtoolkit CLI

Welcome to the `gdtoolkit` CLI documentation!

## `gdtoolkit fmt`

The formatting command: formats a single file, or a path recursively.

```
Auto-format GDScript code

Usage: gdtoolkit.exe fmt [OPTIONS] <INPUT>

Arguments:
  <INPUT>  Input path

Options:
  -c, --check            Only check formatting without showing output
  -w, --write            Write formatting in each input file
  -i, --ignore <IGNORE>  File pattern to ignore
  -h, --help             Print help
```

## `gdtoolkit debug`

Debug commands to evaluate the tool.

### `gdtoolkit debug code-to-lex`

Convert code to lexer JSON output.

```
Convert GDScript code to lex output

Usage: gdtoolkit.exe debug code-to-lex <INPUT> [OUTPUT]

Arguments:
  <INPUT>   Input path
  [OUTPUT]  Output path (or stdout if missing)

Options:
  -h, --help  Print help
```

### `gdtoolkit debug lex-to-code`

Convert lexer JSON to code output.

```
Convert lex output to GDScript code

Usage: gdtoolkit.exe debug lex-to-code <INPUT> [OUTPUT]

Arguments:
  <INPUT>   Input path
  [OUTPUT]  Output path (or stdout if missing)

Options:
  -h, --help  Print help
```

### `gdtoolkit debug code-to-ast`

Convert code to AST JSON output.

```
Convert GDScript code to AST output

Usage: gdtoolkit.exe debug code-to-ast <INPUT> [OUTPUT]

Arguments:
  <INPUT>   Input path
  [OUTPUT]  Output path (or stdout if missing)

Options:
  -h, --help  Print help
```
