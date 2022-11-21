# source=files/example.md:39
set -euo pipefail

# source=files/example.md:47
export ENV=$1
export AWS_PROFILE=freckle

psql_() {
  ../tools/run-in-ssh psql "$@"
}

# source=files/example.md:60
psql_ -f ./scripts/upgrade/drop-reg-types.sql

# source=files/example.md:66
stackctl --filter "..." deploy --no-confirm

# source=files/example.md:72
psql_ -f ./scripts/upgrade/add-reg-types.sql
