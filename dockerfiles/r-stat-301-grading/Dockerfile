# Copyright (c) UBC-DSCI Development Team.
# Distributed under the terms of the Modified BSD License.
ARG BASE_CONTAINER=lourenzutti/r-stat-301:latest
FROM $BASE_CONTAINER

LABEL maintainer="Tiffany Timbers <tiffany.timbers@gmail.com>"

USER root

# install Rise slide extension
RUN conda install --quiet --yes -c conda-forge rise

# install nbgrader 
RUN pip install nbgrader==0.6.1
RUN jupyter nbextension install --sys-prefix --py nbgrader --overwrite 
RUN jupyter nbextension enable --sys-prefix --py nbgrader
RUN jupyter serverextension enable --sys-prefix --py nbgrader

# downgrade nbgrader to 5.6.1 so that feedback forms can be generated
RUN pip install "nbconvert<6"

# switch user
USER $NB_UID
