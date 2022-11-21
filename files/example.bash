# source=files/example.md:37
set -euo pipefail

# source=files/example.md:45
export ENV=$1
export AWS_PROFILE=freckle

psql_() {
  ../tools/run-in-ssh psql "$@"
}

# source=files/example.md:58
psql_ -f ./scripts/upgrade/drop-reg-types.sql

# source=files/example.md:64
stackctl --filter "..." deploy --no-confirm

# source=files/example.md:70
psql_ -f ./scripts/upgrade/add-reg-types.sql
