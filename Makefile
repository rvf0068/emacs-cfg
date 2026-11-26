.DEFAULT_GOAL := test
# Optional machine-specific settings; command-line overrides still win.
EMACS ?= emacs
UV_PROJECT ?=
-include Makefile.local
RUN_TESTS = $(if $(strip $(UV_PROJECT)),uv run --project "$(UV_PROJECT)" --,) bash tests/run-tests.sh

.PHONY: doctor doctor-unit test test-unit test-latex test-jupyter test-workflows benchmark-startup
doctor:
	$(RUN_TESTS) all "$(EMACS)" --doctor
doctor-unit:
	$(RUN_TESTS) unit "$(EMACS)" --doctor
test:
	$(RUN_TESTS) all "$(EMACS)"
test-unit:
	$(RUN_TESTS) unit "$(EMACS)"
test-latex:
	$(RUN_TESTS) latex "$(EMACS)"
test-jupyter:
	$(RUN_TESTS) jupyter "$(EMACS)"
test-workflows:
	$(RUN_TESTS) workflows "$(EMACS)"

benchmark-startup:
	$(RUN_TESTS) benchmark "$(EMACS)"
