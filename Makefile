EMACS ?= emacs
.PHONY: test test-unit test-latex test-jupyter test-workflows
test:
	bash tests/run-tests.sh all "$(EMACS)"
test-unit:
	bash tests/run-tests.sh unit "$(EMACS)"
test-latex:
	bash tests/run-tests.sh latex "$(EMACS)"
test-jupyter:
	bash tests/run-tests.sh jupyter "$(EMACS)"
test-workflows:
	bash tests/run-tests.sh workflows "$(EMACS)"
