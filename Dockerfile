FROM rocker/verse:4.3.1

USER root

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    wget \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    locales \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('tidytext','stringi', 'stopwords', 'DBI', 'plotly'))"
