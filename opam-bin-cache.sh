#!/bin/sh -uex

COMMAND=$1; shift
ID=$1; shift

if [ -z "$ID" ]; then
    if [ $COMMAND = wrap ]; then exec "$@"
    else exit 0
    fi
fi

SUCCESS_CACHE_DIR=~/.cache/opam-bin-cache/success/$ID
FAILURE_CACHE_DIR=~/.cache/opam-bin-cache/failure
FAILURE_CACHE=$FAILURE_CACHE_DIR/$ID

case $COMMAND in
    restore)
        NAME=$1; shift
        if [ -d "$SUCCESS_CACHE_DIR" ]; then
            rm -f "$NAME.install"
            cp -aT "$SUCCESS_CACHE_DIR/" "$OPAM_SWITCH_PREFIX/"
        else
            # This is the first time we built this package.
            # Continue without error: restore is followed by install.
            exit 0
        fi;;
    wrap)
        if [ -d "$SUCCESS_CACHE_DIR" ]; then
            # Already in the cache: exit without executing the command
            exit 0
        else
            if [ -f "$FAILURE_CACHE" ]; then
                # We already know this package fails
                printf "Package in failure cache; exiting" >&2
                exit 1
            else
                if "$@" ; then exit 0
                else
                    mkdir -p "$FAILURE_CACHE_DIR"
                    touch "$FAILURE_CACHE"
                    exit 1
                fi
            fi
        fi;;
    store)
        if [ -d "$SUCCESS_CACHE_DIR" ]; then exit 0
        else
            for f in "$@"; do
                echo "STORING FILE: $f"
                if [ -d "$OPAM_SWITCH_PREFIX/$f" ]; then
                    mkdir -p "$SUCCESS_CACHE_DIR/$f"
                else
                    mkdir -p "$(dirname "$SUCCESS_CACHE_DIR/$f")"
                    cp -aT "$OPAM_SWITCH_PREFIX/$f" "$SUCCESS_CACHE_DIR/$f"
                fi
            done
        fi;;
    *)
        echo "Invalid command '$COMMAND'. Valid commands:" >&2
        echo "    restore ID NAME" >&2
        echo "    wrap ID COMMAND [ARGS]..." >&2
        echo "    store ID [FILES]..." >&2
        exit 2
esac

# Use as:
# pre-install-commands: ["opam-bin-cache.sh" "restore" build-id name] {?build-id}
# wrap-build-commands: ["opam-bin-cache.sh" "wrap" build-id] {?build-id}
# wrap-install-commands: ["opam-bin-cache.sh" "wrap" build-id] {?build-id}
# post-install-commands: ["opam-bin-cache.sh" "store" build-id installed-files] {?build-id & error-code = 0}
