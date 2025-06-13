FROM rocker/r-ver:4.4.1

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libgdal-dev \
    libudunits2-dev \
    pandoc \
    libv8-dev \
    libjavascriptcoregtk-4.0-dev \ 
    libfontconfig1-dev \
    libcairo2-dev \
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
