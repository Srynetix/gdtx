# gdtoolkit - Command-line utility for Godot Engine 3.x

Welcome on the `gdtoolkit` project!

- [Documentation](./docs)

## Motivation

While developing games with [Godot], I use [GDScript], since it's the most recommended language, and it integrates really well in the game development workflow.  
I was mostly using C# with Godot in the past, but performance problems on the Web target made me reconsider the language to use.

As I am also a professional [Python] developer (and a decent [Rust] developer), I want a great tooling experience around the language, which, for me, is:

1. An opinionated automatic code formatter (like [black] for Python, or [rustfmt] for Rust)
2. A sensible and extensible code linting tool (like [flake8] for Python, or [clippy] for Rust)

Other features that could be great in this project are:

3. Add the possibility to manage / download engine versions (already possible with my [gdpm] utility)
4. Add scaffolding utilities to bootstrap a project using a template (or a built-in project with sensible defaults)
5. Manage dependencies / addons (also somewhat possible with [gdpm])
6. Compatibility with the all new [Godot] engine 4.x

(I think I will merge [gdpm] in the `gdtoolkit` project, but that's something for future me.)

One last thing, I want to do everything in [Rust].

## How ?

To do what I want, I need a [GDScript] parser and a "Godot project file" parser.

In the past, I tried to modify the [Godot] game engine a little so it could expose code lint capabilities through the command-line, but it was harder than I expected, and made me go back in time seeing C++ everywhere (I was a big fan before, in the C++11/14 era.).  

So like in a "Compilers 101" course, I need to divide the work in two steps for each target language:

- The **lexer**: reads source code, emits "tokens"
- The **parser**: reads "tokens", emits nodes in an AST (Abstract Source Tree)

When that's done, I need two more steps:

- The **formatter**: use "tokens" from the lexer to write back code, but with enforced rules
- The **linter**: use everything available to emit warnings, tips and errors about the input code

Then, everything can be glued together in a command-line utility and tada :tada:!

To configure the tools, instead of using a specific configuration file, I will use the Godot project `project.godot` file (hence the lexer/parser/formatter).  
I already did this in the [gdpm] project.

## Project structure

Based on what I just said in the previous section, the project will be a [Cargo workspace] with several crates:

- `gdtoolkit`: The main command-line utility
- `gdtoolkit-gdscript-lexer`: The GDScript lexer code
- `gdtoolkit-gdscript-linter`: The GDScript linter code
- `gdtoolkit-gdscript-formatter`: The GDScript formatter code
- `gdtoolkit-gdscript-parser`: The GDScript parser code
- `gdtoolkit-gdproject-lexer`: The "Godot project file" lexer code
- `gdtoolkit-gdproject-formatter`: The "Godot project file" formatter code
- `gdtoolkit-gdproject-parser`: The "Godot project file" parser code

That's a straightforward design, and there is work to do!

## Current project state

Here is a quick view of the project implementation state, by order of priority:

- **GDScript**
    - **GDScript Lexer** _(85%)_: Works great on my projects, code need to be more clearly written, capable of tokenizing a GDScript file of 77k lines in 1 second on my machine,
    - **GDScript Formatter** _(50%)_: Know how to write back tokens to code, when applying the tool on my code base (mostly all my projects in a folder), it writes back exactly the same code, so only the rules are missing, 
    - **GDScript Parser** _(2%)_: Most of the work will be here, many things to do (and more things to do to improve the developer experience with nice error reports),
    - **GDScript Linter** _(0%)_: As it depends on the other crates, it's not even created yet :smile:
- **Godot project** _(0%)_: Not started yet.

So yep, mandatory warning: :warning: **this is a work in progress**, use at your own risks :warning:

## Contributing

If you like the project, do not hesitate to contribute by raising issues and opening Pull Requests ! :smile:

[black]: https://github.com/psf/black
[Cargo workspace]: https://doc.rust-lang.org/book/ch14-03-cargo-workspaces.html
[clippy]: https://github.com/rust-lang/rust-clippy
[flake8]: https://flake8.pycqa.org/en/latest/
[gdpm]: https://github.com/Srynetix/gdpm
[GDScript]: https://docs.godotengine.org/en/3.5/tutorials/scripting/gdscript/index.html
[Godot]: https://godotengine.org/
[Python]: https://www.python.org/
[Rust]: https://www.rust-lang.org/learn
[rustfmt]: https://github.com/rust-lang/rustfmt
