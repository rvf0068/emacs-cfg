# Configuration maintenance

- Keep straight.el, explicit external package recipes, and the literate emacs-config.org configuration. Do not introduce another package manager.
- Keep current package revisions during refactoring; make only explicitly justified compatibility pin corrections (such as the Org revision required for native previews). Retain elfeed-org, org-pomodoro, minions and modus-themes; org-fragtog is superseded and may be removed in a separate verified change.
- Preserve working functionality and existing keybindings, particularly Org, LaTeX previews, AUCTeX, Jupyter, Citar, Org Roam, and uv integration.
- Keep substantial implementations and compatibility fixes in lisp/ with lexical binding and focused ERT tests. Prefer extending existing implementations; document upstream issues.
- Use \( ... \) for inline math and \[ ... \] for display math in Org documents. Preserve existing label conventions.
- Run make test after each configuration change. Real Org LaTeX preview generation and real Jupyter kernel execution with inline images must pass; never substitute mocked package-loading tests for these integration checks.
- Use samples/workflow-test.org to check interactive preview editing and PDF export when changing these behaviors.
- Do not claim a test passed if dependencies were unavailable or a test was skipped. Fix or explicitly report regressions before merging.
- Never commit personal.org, custom.el, Makefile.local, OAuth tokens, credentials, caches or generated files. Add unit-suite dependency names to tests/run-unit.el when needed; derive external straight recipes from emacs-config.org using the test helper.
