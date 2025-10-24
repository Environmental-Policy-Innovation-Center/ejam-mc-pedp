FROM rocker/rstudio:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
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
    libgeos-dev \
    libgeos++-dev \
    libsqlite3-dev \
    libmagick++-dev \
    libfreetype6-dev \
    libfontconfig1-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libtiff5-dev \
    libpng-dev \
    libjpeg-dev \
    libgit2-dev \
    libjq-dev \
    libv8-dev \
    cmake \
    git \
    && rm -rf /var/lib/apt/lists/*
    
# Install AWS CLI v2
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "/tmp/awscliv2.zip" && \
    unzip /tmp/awscliv2.zip -d /tmp && \
    /tmp/aws/install && \
    rm -rf /tmp/awscliv2.zip /tmp/aws

RUN mkdir -p /home/epic
WORKDIR /home/epic

# Install core spatial dependencies first
RUN install2.r --error --skipinstalled \
    sf \
    units \
    && rm -rf /tmp/downloaded_packages

# Install all EJAM dependencies
RUN install2.r --error --skipinstalled \
    arrow \
    collapse \
    colorspace \
    config \
    curl \
    data.table \
    digest \
    dplyr \
    DT \
    fs \
    ggplot2 \
    gh \
    glue \
    golem \
    gt \
    httr \
    httr2 \
    leaflet \
    leaflet.extras2 \
    magrittr \
    methods \
    openxlsx \
    pdist \
    piggyback \
    pkgdown \
    plotly \
    purrr \
    readr \
    readxl \
    rlang \
    rmarkdown \
    rstudioapi \
    scales \
    SearchTrees \
    shiny \
    shinycssloaders \
    shinydisconnect \
    shinyjs \
    stringr \
    tibble \
    tidycensus \
    tidyr \
    webshot \
    writexl \
    XML \
    && rm -rf /tmp/downloaded_packages

# Install geojsonio (has tricky dependencies)
RUN install2.r --error --skipinstalled \
    V8 \
    jqr \
    geojson \
    geojsonio \
    && rm -rf /tmp/downloaded_packages
    
RUN install2.r --error --skipinstalled \
    remotes \
    && rm -rf /tmp/downloaded_packages

# Copy folder contents 
ADD . /home/epic/

RUN R -e "remotes::install_github('mikejohnson51/AOI')"

# Install EJAM package
RUN R -e "remotes::install_local('/home/epic/', dependencies = TRUE)"

# Verify EJAM installed successfully
RUN R -e "library(EJAM); cat('EJAM loaded successfully\n')"

# Expose ports
EXPOSE 2000 2001

# Set the working directory and command to run the app
WORKDIR /home/epic
CMD ["R", "-e", "httpuv::startServer('0.0.0.0', 2001, list(call = function(req) { list(status = 200, body = 'OK', headers = list('Content-Type' = 'text/plain')) })); shiny::runApp('/home/epic/app.R', port = 2000, host = '0.0.0.0')"]