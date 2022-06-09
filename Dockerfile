FROM rocker/tidyverse:3.5.1 as base

ENV RENV_VERSION 0.9.3

RUN apt-get update \
  && apt-get install -y --no-install-recommends \  
    default-jre \ 
    default-jdk \ 
    libbz2-dev \
    libpcre3-dev \
    liblzma-dev \
    zlib1g-dev \
    libxml2-dev \
    libgit2-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libpng-dev \
    libv8-dev \
    libudunits2-dev \
    libgdal-dev \
    libjq-dev \
    r-cran-rjava \
    ca-certificates \
    curl \
  && R CMD javareconf

RUN export ADD=shiny && bash /etc/cont-init.d/add && \
    R -e 'install.packages("renv")' && \
    echo "RENV_PATHS_LIBRARY=/usr/local/lib/R/library" >> /usr/local/lib/R/etc/Renviron && \
    echo "RENV_PATHS_CACHE=/usr/local/lib/R/library" >> /usr/local/lib/R/etc/Renviron && \
    chown -R rstudio:rstudio /usr/local/lib/R 

WORKDIR /home/rstudio
COPY . .
RUN R -e 'renv::restore(repos=c(CRAN="https://mirror.las.iastate.edu/CRAN"))'

EXPOSE 8787 