FROM rocker/binder:3.6.0
LABEL maintainer='Qingqing Chen'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}


RUN wget https://github.com/spatialnetworkslab/identifying-meaningful-locations/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-10-26/')); devtools::install_deps(); devtools::install()"

RUN rm DESCRIPTION.1; exit 0

