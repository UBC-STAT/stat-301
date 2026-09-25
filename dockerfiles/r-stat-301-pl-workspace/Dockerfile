FROM prairielearn/workspace-jupyterlab-r:2ba3e7fcb3b38e102f1706ad61fb8f0e7fc15950



USER root

# Install R packages
COPY conda-linux-64.lock /tmp/conda-linux-64.lock

RUN mamba install --yes --file /tmp/conda-linux-64.lock && \
    mamba clean --all -f -y 

# Install CRAN packages
RUN Rscript -e "lib <- 'faux'; install.packages(lib, repos='http://cran.us.r-project.org'); if ( ! library(lib,character.only=TRUE,logical.return=TRUE)){quit(status=1,save='no')}" 

RUN Rscript -e "lib <- 'moderndive'; install.packages(lib, repos='http://cran.us.r-project.org'); if ( ! library(lib,character.only=TRUE,logical.return=TRUE)){quit(status=1,save='no')}"

USER ${NB_UID}