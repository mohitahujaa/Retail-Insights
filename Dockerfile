# RetailPulse: R/Shiny dashboard in a container.
#
# Build:  docker build -t retailpulse .
# Run:    docker run --rm -p 3838:3838 retailpulse
# Open:   http://localhost:3838

# R 4.4.1 with CRAN packages pinned to a fixed snapshot date by the rocker
# image, so builds are reproducible.
FROM rocker/r-ver:4.4.1

RUN install2.r --error --skipinstalled --ncpus -1 \
      readxl dplyr lubridate ggplot2 shiny bslib DT scales testthat \
    && rm -rf /tmp/downloaded_packages

RUN useradd --create-home --shell /bin/bash retailpulse
WORKDIR /app
COPY --chown=retailpulse:retailpulse . /app
USER retailpulse

# Run the full analysis once at build time: verifies the pipeline against the
# bundled workbook and caches results so the app starts in seconds.
RUN Rscript scripts/run_analysis.R

EXPOSE 3838
CMD ["R", "--no-save", "-e", "shiny::runApp('.', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
