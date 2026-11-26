# Keep the dependency closure validated by PR #16.  In particular, do not
# disable APT recommends: doing so broke the actual LaTeX preview tests.
FROM ubuntu:24.04

ENV LANG=C.UTF-8
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y \
       ca-certificates git curl python3 python3-venv \
       emacs-gtk xvfb xauth libzmq3-dev build-essential pkg-config \
       autoconf automake libtool \
       libpoppler-glib-dev libpng-dev zlib1g-dev elpa-pdf-tools-server \
       texlive-latex-base texlive-latex-extra texlive-fonts-recommended \
       texlive-fonts-extra texlive-science dvisvgm latexmk biber \
       aspell-en ripgrep \
    && rm -rf /var/lib/apt/lists/*

RUN python3 -m venv /opt/workflow-venv \
    && /opt/workflow-venv/bin/python -m pip install --no-cache-dir \
       jupyter-core jupyter-client ipykernel matplotlib numpy networkx
ENV PATH="/opt/workflow-venv/bin:${PATH}"
ENV RVF_EPDFINFO=/usr/bin/epdfinfo

# Fail while building rather than halfway through the regression suite.
RUN emacs --version \
    && kpsewhich lxfonts.sty \
    && kpsewhich mylatexformat.ltx \
    && test -x "$RVF_EPDFINFO" \
    && jupyter --version \
    && jupyter-kernelspec --version

# The checkout is mounted read-only; only the temporary test home is writable.
RUN useradd --create-home --shell /bin/bash ci
USER ci
WORKDIR /workspace
