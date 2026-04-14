FROM rocker/r-ver:4.4.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    unzip \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libproj-dev \
    libgdal-dev \
    libmagick++-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    texlive \
    texlive-latex-extra \
    texlive-fonts-extra \
    && rm -rf /var/lib/apt/lists/*

# Install AWS CLI v2
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "/tmp/awscliv2.zip" && \
    unzip /tmp/awscliv2.zip -d /tmp && \
    /tmp/aws/install && \
    rm -rf /tmp/awscliv2.zip /tmp/aws

RUN mkdir -p /home/epic
WORKDIR /home/epic

# Install CRAN packages and clean cache in the SAME layer
RUN install2.r --error --skipinstalled \
    s2 \
    sf \
    tidyverse \
    arrow \
    attempt \
    collapse \
    config \
    data.table \
    DBI \
    desc \
    doSNOW \
    dplyr \
    DT \
    foreach \
    ggplot2 \
    ggridges \
    glue \
    golem \
    htmltools \
    leaflet \
    leaflet.extras2 \
    leaflet.extras \
    magrittr \
    methods \
    openxlsx \
    pdist \
    pins \
    piggyback \
    pkgload \
    readxl \
    rhandsontable \
    rmarkdown \
    RMySQL \
    SearchTrees \
    shinydisconnect \
    shiny \
    shinycssloaders \
    shinyjs \
    sp \
    tidyr \
    viridis \
    webshot \
    knitr \
    spelling \
    testthat \
    beepr \
    datasets \
    fipio \
    htmlwidgets \
    jsonlite \
    mapview \
    rnaturalearth \
    rvest \
    terra \
    tidygeocoder \
    units \
    remotes \
    && rm -rf /tmp/downloaded_packages /tmp/*.rds \
    && rm -rf /usr/local/lib/R/site-library/*/doc \
    && rm -rf /usr/local/lib/R/site-library/*/help \
    && rm -rf /usr/local/lib/R/site-library/*/html

# GitHub packages
RUN R -e "remotes::install_github('mikejohnson51/AOI')" && \
    R -e "remotes::install_github('hrbrmstr/hrbrthemes')" && \
    rm -rf /tmp/downloaded_packages /tmp/*.rds

# Copy app files
ADD . /home/epic/

# Install local package
RUN R -e "remotes::install_local('/home/epic/', dependencies = TRUE)" && \
    rm -rf /tmp/downloaded_packages /tmp/*.rds

EXPOSE 2000 2001

WORKDIR /home/epic
CMD ["R", "-e", "httpuv::startServer('0.0.0.0', 2001, list(call = function(req) { list(status = 200, body = 'OK', headers = list('Content-Type' = 'text/plain')) })); library(EJAM); EJAM::run_app(isPublic = TRUE, options = list(host = '0.0.0.0', port = 2000))"]