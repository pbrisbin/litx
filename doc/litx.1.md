% LITX(1) User Manual
%
% November 2022

# NAME

litx - execute Literate Markdown programs

# SYNOPSIS

*litx* \[options]

# OPTIONS

**\-i**, **\--input** *\<PATH|->*\

> Read Markdown from *PATH* (default: *-*)

## Language Presets

If not specified (see below), LitX uses the most common supported language
across all code blocks in the source document. If there are none, LitX will be a
complicated no-op (and empty file executed with *cat*).

**\--bash**

> Equivalent to:
>
> **\--filter-tag**=*\"bash\"*\
> **\--comment-chars**=*\"#\"*\
> **\--exec**=*\"bash\"*\
> **\--arg**=*\"-s\"*\
> **\--arg**=*\"-\"*

Additional languages coming soon.

## Rendering Options

**\--filter-tag** *\<TEXT>*\

> Only process code blocks with the given tag.

**\--comment-chars** *\<TEXT>*\

> Set the characters used to prefix line comments.

## Execution Options

**\--exec** *\<COMMAND>*\

> Set the command to use to execute the script. Default is language-specific.
> Passing this option will clear any arguments.
>
> For example,
>
> ```
> --bash
> ```
>
> will invoke
>
> ```
> bash -s -
> ```
>
> but
>
> ```
> --bash --exec my-bash
> ```
>
> will only invoke
>
> ```
> my-bash
> ```
>
> You will need to pass
>
> ```
> --exec my-bash --arg -s --arg -
> ```
>
> to achieve what (probably) you want.
>
> **NOTE**: The script is always passed to the command as its *stdin*.

**\--arg** *\<ARG>*\

> Pass an additional argument during execution. Can be specified many times.
>
> This will append to any language-specific arguments. To avoid this, you can
> re-specify the default *\--exec* first.

**\--no-env**\

> Don't inherit the current process's environment in the executed process.

# PRAGMAS

LitX reads directives from HTML comment in the source documents.

## Options

The first comment that begins with the word *litx*, if present, will be parsed
for command-line flags:

```
<!-- litx --bash -->

<!-- litx
     --bash
     --arg="--foo bar"
     --no-env
-->
```

These options are applied after the inferred language preset, but before any
other command-line options (including an explicit language preset). *All* words
will be considered options, so ensure there is not extra content in the comment.

# ENVIRONMENT

**LOG_LEVEL**=*error|warn|info|debug|trace*\

> Default is *info*.

**LOG_FORMAT**=*tty|json*\

> Default is *tty*.

**LOG_COLOR**=*always|never|auto*\

> Default is *auto*.

**LOG_DESTINATION**=*stdout|stderr|@\<path\>*\

> Default is *stdout*.

# AUTHOR

Patrick Brisbin <pbrisbin@gmail.com>
