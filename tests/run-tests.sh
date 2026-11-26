#!/usr/bin/env bash
set -euo pipefail
if [[ $# -lt 2 || $# -gt 3 || ("${3:-}" != "" && "${3:-}" != --doctor) ]]; then
    echo 'Usage: run-tests.sh {all|unit|latex|jupyter|workflows} EMACS [--doctor]' >&2
    exit 2
fi
mode="$1"
case "$mode" in all|unit|latex|jupyter|workflows) ;; *) echo "Unknown test mode: $mode" >&2; exit 2;; esac
root="$(cd "$(dirname "$0")/.." && pwd)"
source "$root/tests/check-dependencies.sh"
rvf_check_dependencies "$mode" "$2"
if [[ "${3:-}" == --doctor ]]; then exit 0; fi
emacs="$(command -v -- "$2")"
if [[ "$emacs" != /* ]]; then emacs="$(realpath "$emacs")"; fi
tmp="$(mktemp -d)"
cleanup() {
    status=$?
    for log in "$tmp/emacs.log" "$tmp/emacs-no-personal.log"; do
        if [[ -f "$log" ]]; then cat "$log"; fi
    done
    rm -rf "$tmp"
    exit "$status"
}
trap cleanup EXIT
export HOME="$tmp/home"
export XDG_CACHE_HOME="$HOME/.cache" XDG_CONFIG_HOME="$HOME/.config" XDG_DATA_HOME="$HOME/.local/share"
export JUPYTER_CONFIG_DIR="$HOME/.jupyter" JUPYTER_DATA_DIR="$HOME/.local/share/jupyter"
export RVF_TEST_LOG="$tmp/emacs.log" RVF_TEST_MODE="$mode"
export RVF_TEST_DIR="$HOME/.emacs.d"
export MPLBACKEND=Agg
mkdir -p "$RVF_TEST_DIR/straight/versions" "$RVF_TEST_DIR/straight/repos" "$HOME/notes" "$HOME/library" "$HOME/Documents/org-latex-previews"
touch "$HOME/library/test.bib"
cp "$root/init.el" "$root/emacs-config.org" "$RVF_TEST_DIR/"
if [[ -f "$root/early-init.el" ]]; then cp "$root/early-init.el" "$RVF_TEST_DIR/"; fi
cp -R "$root/lisp" "$root/snippets" "$root/auctex" "$root/tests" "$root/samples" "$RVF_TEST_DIR/"
cp "$root/straight/versions/default.el" "$RVF_TEST_DIR/straight/versions/"
cat > "$RVF_TEST_DIR/personal.org" <<'EOF'
#+begin_src emacs-lisp
(setq my-notes-directory (expand-file-name "notes/" "~")
      my-library-path (expand-file-name "library/" "~")
      my-bibliography (expand-file-name "library/test.bib" "~"))
#+end_src
EOF
# Bootstrap the exact recorded straight revision.
pin="$(sed -n 's/.*("straight\.el" \. "\([0-9a-f]*\)").*/\1/p' "$RVF_TEST_DIR/straight/versions/default.el")"
if [[ -z "$pin" ]]; then
    echo "ERROR: straight.el pin not found in straight/versions/default.el" >&2
    exit 1
fi
git clone --quiet https://github.com/radian-software/straight.el.git "$RVF_TEST_DIR/straight/repos/straight.el"
git -C "$RVF_TEST_DIR/straight/repos/straight.el" checkout --quiet "$pin"
if [[ "$mode" != unit ]]; then
    command -v jupyter
    python -m ipykernel install --user --name rvf-regression --display-name "Emacs regression"
fi
cd "$RVF_TEST_DIR"
status=0
if [[ "$mode" != unit ]]; then
    echo "Running graphical Org workflow tests..."
    timeout --kill-after=20s 20m xvfb-run -a "$emacs" -Q --no-splash --load tests/run-workflows.el || status=1
    if [[ "$mode" == all || "$mode" == workflows ]]; then
        echo "Running graphical startup and workflows without personal.org..."
        # A second process reuses the pinned packages, not the configuration
        # variables or database connection from the first process.
        mv "$RVF_TEST_DIR/personal.org" "$tmp/personal.org"
        rm -f "$RVF_TEST_DIR/personal.el"
        RVF_TEST_PERSONAL=absent RVF_TEST_LOG="$tmp/emacs-no-personal.log" \
            timeout --kill-after=20s 20m xvfb-run -a "$emacs" -Q --no-splash --load tests/run-workflows.el || status=1
    fi
fi
if [[ "$mode" == all || "$mode" == unit ]]; then
    echo "Running isolated ERT unit tests..."
    timeout --kill-after=20s 20m "$emacs" -Q --batch --load tests/run-unit.el || status=1
fi
exit "$status"
