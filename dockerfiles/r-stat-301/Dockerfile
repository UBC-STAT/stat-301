# Copyright (c) UBC-DSCI Development Team.
# Distributed under the terms of the Modified BSD License.
ARG BASE_CONTAINER=ubcdsci/r-dsci-100:latest
FROM $BASE_CONTAINER

LABEL maintainer="Tiffany Timbers <tiffany.timbers@gmail.com>"

RUN pip install SQLAlchemy==1.3
RUN pip install jupyter-client==6.1.7

# Install testthat version 2.3
RUN Rscript -e "devtools::install_version('testthat', version = '2.3.2', repos = 'http://cran.us.r-project.org')"

# Install the palmerpenguins dataset
RUN Rscript -e "devtools::install_github('allisonhorst/palmerpenguins@v0.1.0')" 

# Install ISLR package for the Credit data set
RUN Rscript -e "install.packages('ISLR', repos='http://cran.us.r-project.org')"

# Install contextual package for multi-armed bandit
RUN Rscript -e "install.packages('contextual', repos='http://cran.us.r-project.org')"

# Install contextual package for multi-armed bandit
RUN Rscript -e "install.packages('GGally', repos='http://cran.us.r-project.org')"

# Install janitor package
RUN Rscript -e "install.packages('janitor', repos='http://cran.us.r-project.org')"

# Install mltools package
RUN Rscript -e "install.packages('mltools', repos='http://cran.us.r-project.org')"

# Install leaps package
RUN Rscript -e "install.packages('leaps', repos='http://cran.us.r-project.org')"

# Install faraway package
RUN Rscript -e "install.packages('faraway', repos='http://cran.us.r-project.org')"

# Install glmnet package
RUN Rscript -e "install.packages('glmnet', repos='http://cran.us.r-project.org')"

# Install titanic package
RUN Rscript -e "install.packages('titanic', repos='http://cran.us.r-project.org')"

# Install performance package
RUN Rscript -e "install.packages('performance', repos='http://cran.us.r-project.org')"

# Install binom package
RUN Rscript -e "install.packages('binom', repos='http://cran.us.r-project.org')"

# Install gridGraphics package
RUN Rscript -e "install.packages('gridGraphics', repos='http://cran.us.r-project.org')"

# Install AER package
RUN Rscript -e "install.packages('AER', repos='http://cran.us.r-project.org')"

# Install faux package
RUN Rscript -e "install.packages('faux', repos='http://cran.us.r-project.org')"

# Install faux package
RUN Rscript -e "install.packages('latex2exp', repos='http://cran.us.r-project.org')"