FROM rocker/r-ver:4.4.1

RUN apt-get update -y && apt-get upgrade -y

# Install system dependencies for R packages
RUN apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libgdal-dev \
    libudunits2-dev \
    pandoc \
    libv8-dev \
    libjavascriptcoregtk-4.1-dev \ 
    libfontconfig1-dev \
    libcairo2-dev \
    libgeos-dev \
    libproj-dev \
    libharfbuzz-dev \
    libfribidi-dev \ 
    libtiff-dev \
    cmake \ 
    g++ \
    make \
    && rm -rf /var/lib/apt/lists/*

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Set working directory
WORKDIR /app

# Copy renv.lock and R files
COPY renv.lock .
COPY Ground_water_project.R .

# Restore R packages using renv
RUN R -e "renv::restore()"

# Expose port for Shiny app
EXPOSE 3838

# Run Shiny app
CMD ["R", "-e", "shiny::runApp('/app/Ground_water_project.R', host='0.0.0.0', port=3838)"]
