#!/usr/bin/env bash
set -euo pipefail
mode="$1"
emacs="$(command -v "$2")"
case "$mode" in all|unit|latex|jupyter|workflows) ;; *) exit 2;; esac
root="$(cd "$(dirname "$0")/.." && pwd)"
tmp="$(mktemp -d)"
cleanup() {
    status=$?
    if [[ -f "$tmp/emacs.log" ]]; then cat "$tmp/emacs.log"; fi
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
mkdir -p "$RVF_TEST_DIR/straight/versions" "$HOME/notes" "$HOME/library" "$HOME/Documents/org-latex-previews"
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
(when-let ((program (getenv "RVF_EPDFINFO")))
  (setq pdf-info-epdfinfo-program program))
#+end_src
EOF
# Bootstrap the exact recorded straight revision.
pin="$(sed -n 's/.*("straight\.el" \. "\([0-9a-f]*\)").*/\1/p' "$RVF_TEST_DIR/straight/versions/default.el")"
test -n "$pin"
git clone --quiet https://github.com/radian-software/straight.el.git "$RVF_TEST_DIR/straight/repos/straight.el"
git -C "$RVF_TEST_DIR/straight/repos/straight.el" checkout --quiet "$pin"
if [[ "$mode" != unit ]]; then
    command -v jupyter
    python -m ipykernel install --user --name rvf-regression --display-name "Emacs regression"
fi
cd "$RVF_TEST_DIR"
if [[ "$mode" != unit ]]; then
    timeout --kill-after=20s 20m xvfb-run -a "$emacs" -Q --no-splash --load tests/run-workflows.el
fi
if [[ "$mode" == all || "$mode" == unit ]]; then
    timeout --kill-after=20s 20m "$emacs" -Q --batch --load tests/run-unit.el
fi
