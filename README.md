# LitX

Execute a Literate Markdown program.

## Motivation

At $JOB, we occasionally need to perform operations on our infrastructure that
cannot be completed (in whole or in part) by our usual [IaC][] tooling. For such
cases, we will still script the steps and commit that script to our
infrastructure repository by way of Pull Request -- this allows code review,
satisfies the two-person rule, and creates an audit trail.

[iac]: https://en.wikipedia.org/wiki/Infrastructure_as_code

[Literate Programming][literate] is a perfect fit for such scripts, which should
ideally be documented with context, warnings, and other details -- in a
maximally-readable format. A markdown file with the script embedded as
code-blocks throughout works well, but we still want to be able to execute those
code-blocks exactly as written without error-prone copy-pasting into a terminal.

[literate]: https://en.wikipedia.org/wiki/Literate_programming

LitX does exactly that.

## Example

Given [this input][input],

[input]: ./files/example.md

![](./files/example-md.png)

LitX will generate (and execute) [this output][output],

[output]: ./files/example.bash

![](./files/example-bash.png)

## Basic Usage

Build and execute the code-blocks in `op.md`:

```console
litx <./op.md

litx -i - <./op.md

litx -i ./op.md
```

Build the code-blocks in `op.md` into `op.bash`, do not execute it:

```console
litx --exec cat <./.op.md >./op.bash
```

## Complete Usage

```console
% litx --help
Usage: litx [-i|--input PATH|-] [--bash] [--python] [--filter-tag TEXT] 
            [--comment-chars TEXT] [--exec CMD] [--arg ARG] [--no-env]
  Execute Literate Markdown programs

Available options:
  -i,--input PATH|-        Read Markdown from PATH (default: -)
  --bash                   Parse and execute bash code blocks
  --python                 Parse and execute python code blocks
  --filter-tag TEXT        Filter to code blocks fo the given tag
  --comment-chars TEXT     Set the characters used for line comments
  --exec CMD               Execute script using CMD
  --arg ARG                Pass additional arguments when executing
  --no-env                 Don't inherit ENV in the executed process
  -h,--help                Show this help text

See litx(1) for more details.
```

## Installation

### Binary Release

1. Head over to [Releases](/releases) and choose a Release
1. Copy the URL to the appropriate archive for your OS
1. Download, extract, and install:

   ```console
   curl -L "$(xclip -o -s clipboard) | tar xvzf -       # On Linux
   curl -L "$(pbpaste)" | tar xvzf -                    # On OSX
   cd ./litx
   sudo make install                                    # for /usr/bin/litx
   make install PREFIX=$HOME/.local                     # for ~/.local/bin/lix
   rm -r ./litx                                         # optionally
   ```

### From Source

1. Have a Haskell setup, including Stack
1. Clone the repository
1. Build and install in `~/.local`

   ```console
   make install.check
   ```

You can also run `stack build ... --copy-bins` of course, but you'll be missing
completions and documentation.

## Documentation

Once installed,

```
man 1 litx
```

Documentation is always available in Markdown form [in-repository](./doc/), but
note it will be as of `main` and not necessarily your installed version.

## Roadmap

- [ ] Add `--check` for (e.g.) running ShellCheck on output
- [ ] Add `--{offset,skip}=[-]N` to target specific code blocks by index
- [ ] `<!-- litx: ignore -->` pragmas

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
