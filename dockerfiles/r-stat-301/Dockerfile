# Copyright (c) UBC-DSCI Development Team. 
# Distributed under the terms of the Modified BSD License.
FROM jupyter/minimal-notebook

LABEL maintainer="Brian Kim <brian.kim@stat.ubc.ca>"

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

USER root

# R pre-requisites
RUN apt-get update --yes && \
    apt-get install --yes --no-install-recommends \
    fonts-dejavu \
    unixodbc \
    unixodbc-dev \
    r-cran-rodbc \
    gfortran \
    gcc \
    vim && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

USER ${NB_UID}

# Change workdir to $HOME/work so config files are preserved when bind mounting
WORKDIR "${HOME}/work"

# disable warnings that pollute build logs; seems to be related to the update to python 3.11
# https://discourse.jupyter.org/t/debugger-warning-it-seems-that-frozen-modules-are-being-used-python-3-11-0/16544/12
ENV PYDEVD_DISABLE_FILE_VALIDATION=1

COPY conda-linux-64.lock /tmp/conda-linux-64.lock

RUN mamba update --quiet --file /tmp/conda-linux-64.lock && \
  mamba clean --all -y -f && \
  fix-permissions "${CONDA_DIR}" && \
  fix-permissions "/home/${NB_USER}" && \
  Rscript -e "lib <- 'faux'; install.packages(lib, repos='http://cran.us.r-project.org'); if ( ! library(lib,character.only=TRUE,logical.return=TRUE)){quit(status=1,save='no')}" && \
  Rscript -e "lib <- 'moderndive'; install.packages(lib, repos='http://cran.us.r-project.org'); if ( ! library(lib,character.only=TRUE,logical.return=TRUE)){quit(status=1,save='no')}"

# Install latest pexpect to fix asyncio issue with jupyterlab_git
# Install setuptools==69.0.0 to fix issue with nbgrader and "from backports import tarfile"
RUN pip install --no-cache pexpect==4.9.0 setuptools==69.0.0

# Disable the cell toolbar (which ignores metadata and students often accidentally click + delete grading cells)
RUN jupyter labextension disable @jupyterlab/cell-toolbar-extension

# Disable announcement pop-up on start
RUN jupyter labextension disable "@jupyterlab/apputils-extension:announcements"

# Configure shortcuts-extension to remove Shift-M cell merge shortcut
COPY config/shortcuts.jupyterlab-settings /home/${NB_USER}/.jupyter/lab/user-settings/\@jupyterlab/shortcuts-extension/shortcuts.jupyterlab-settings

# Copy jupyter_server_config.py which allows students to see and delete hidden files
COPY config/jupyter_server_config.py /home/${NB_USER}/.jupyter

# Copy jupyter_notebook_config.py with automatic backup script
COPY config/jupyter_notebook_config.py /home/${NB_USER}/.jupyter

# Copy gitconfig that sets default pull strategy to rebase
COPY config/.gitconfig /home/${NB_USER}/

# install rise from fork wheel with fixed CSS
COPY jupyterlab_rise-0.42.0-py3-none-any.whl ./
RUN pip install wheel && pip install jupyterlab_rise-0.42.0-py3-none-any.whl && rm -f jupyterlab_rise-0.42.0-py3-none-any.whl

# Make sure everything in the home folder is owned by NB_USER for running docker image locally.
USER root
RUN chown -R ${NB_USER} /home/${NB_USER}
USER ${NB_UID}

# Disable HEALTHCHECK for performance reasons
HEALTHCHECK NONE