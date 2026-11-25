#!/usr/bin/env bash
# Sourced before creating the temporary test home.
rvf_check_dependencies() {
    local mode="$1" requested_emacs="$2" failed=0 cmd package version
    local -a commands=(git timeout)
    if [[ "$mode" != unit ]]; then
        commands+=(xvfb-run Xvfb xauth latex pdflatex dvisvgm kpsewhich python jupyter)
    fi
    for cmd in "$requested_emacs" "${commands[@]}"; do
        if command -v -- "$cmd" >/dev/null 2>&1; then
            printf 'OK: %s -> %s\n' "$cmd" "$(command -v -- "$cmd")"
        else
            printf 'ERROR: Required executable not found: %s\n' "$cmd" >&2
            failed=1
        fi
    done
    if command -v -- "$requested_emacs" >/dev/null 2>&1; then
        if version=$("$requested_emacs" --version 2>&1); then
            printf 'Emacs: %s\n' "$(printf '%s\n' "$version" | sed -n '1p')"
        else
            printf 'ERROR: Cannot execute %s --version\n%s\n' "$requested_emacs" "$version" >&2
            failed=1
        fi
    fi
    if [[ "$mode" != unit ]]; then
        if command -v kpsewhich >/dev/null 2>&1; then
            for package in lxfonts.sty amsmath.sty amssymb.sty mylatexformat.ltx; do
                if ! kpsewhich "$package"; then
                    printf 'ERROR: TeX file not found: %s\n' "$package" >&2
                    failed=1
                fi
            done
        fi
        if command -v python >/dev/null 2>&1; then
            if ! python - <<'PY'
import importlib
import sys

print("Python:", sys.executable)
failed = False
for name in ("ipykernel", "jupyter_client", "networkx", "numpy", "matplotlib"):
    try:
        importlib.import_module(name)
    except Exception as exc:
        print(f"ERROR: Python dependency {name}: {exc}", file=sys.stderr)
        failed = True
    else:
        print(f"OK: Python dependency {name}")
sys.exit(int(failed))
PY
            then
                printf 'Use the intended Python environment (e.g. uv run), with the required dependencies installed.\n' >&2
                failed=1
            fi
        fi
        if [[ -n "${RVF_EPDFINFO:-}" && ! -x "$RVF_EPDFINFO" ]]; then
            printf 'ERROR: RVF_EPDFINFO is not executable: %s\n' "$RVF_EPDFINFO" >&2
            failed=1
        fi
    fi
    if ((failed)); then
        printf 'Dependency check failed. See README.org and Makefile.local.example.\n' >&2
        return 1
    fi
    printf 'Dependency check passed (actual workflows are verified by make test).\n'
}
