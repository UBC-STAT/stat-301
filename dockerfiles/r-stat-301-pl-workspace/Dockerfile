FROM prairielearn/workspace-jupyterlab-r:523b1a9d375f8c8e14b917eaa7195921e5ae388a

USER root

# Install R packages
COPY conda-linux-64.lock /tmp/conda-linux-64.lock

RUN mamba install --yes --file /tmp/conda-linux-64.lock && \
    mamba clean --all -f -y 

# Install CRAN packages
RUN Rscript -e "lib <- 'faux'; install.packages(lib, repos='http://cran.us.r-project.org'); if ( ! library(lib,character.only=TRUE,logical.return=TRUE)){quit(status=1,save='no')}" 

RUN Rscript -e "lib <- 'moderndive'; install.packages(lib, repos='http://cran.us.r-project.org'); if ( ! library(lib,character.only=TRUE,logical.return=TRUE)){quit(status=1,save='no')}"

USER ${NB_UID}
