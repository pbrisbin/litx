<!-- litx --bash

We can declare options by in-file pragma. --bash is a synonym for

  --shebang="/usr/bin/env bash"
  --preamble="set -euo pipfail"
  --codeblock-tag=bash

-->

This is a markdown file documenting some manual operation your team will do
off-hours, like a databse upgrade or something. It requires a few steps that you
would like to:

1. Script
2. Get review on
3. Commit to the repository
4. Execute without error-prone copy/paste

So we put it in a markdown file.

## Setup

We'll accept the enviroment to upgrade as the first and only argument, and
assume AWS credentials are active. In order to talk to the DB, we have to use a
special psql, which expects `$ENV`.

```bash
export ENV=$1
export AWS_PROFILE=freckle

psql_() {
  ../tools/run-in-ssh psql "$@"
}
```

## Pre-upgrade

We need to drop some indexes for the upgrade to proceed.

```bash
psql_ -f ./scripts/upgrade/drop-reg-types.sql
```

## Upgrade

```bash
stackctl --filter "..." deploy --no-confirm
```

## Post-upgrade

```bash
psql_ -f ./scripts/upgrade/add-reg-types.sql
```