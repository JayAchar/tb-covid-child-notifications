FROM rocker/tidyverse:4.0.4@sha256:1f27b4587932fe7ab2b9a8cb2b5e735c64548aa77edf1d3cf17c81061a95ff90
# define download date for all R package dependencies
ARG R_PACKAGE_DATE=2022-06-30

ENV DISABLE_AUTH=true

RUN apt-get -y update && \
    apt-get install -y  libudunits2-dev \
        libgdal-dev \
        libgeos-dev \
        libproj-dev

WORKDIR /usr/src
COPY . .
RUN R CMD build .

# define date for R package deps
RUN echo "options(repos = \
    c(CRAN = 'https://packagemanager.rstudio.com/cran/__linux__/focal/${R_PACKAGE_DATE}'), \
    download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

# install required packagess
RUN Rscript -e "source('/usr/src/packages.R')"

EXPOSE 8787

CMD ["/init"]
