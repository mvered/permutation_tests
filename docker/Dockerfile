# SET UP DOCKERFILE FOR RUNNING SIMULATIONS ON AWS AS BATCH JOBS

# Use a stable R version as the base
FROM rocker/r-ver:4.3.0

# 1. Install System Dependencies
# libarmadillo-dev is for RcppArmadillo
# curl/unzip are for installing the AWS CLI
RUN apt-get update && apt-get install -y \
    libarmadillo-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    curl \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# 2. Install AWS CLI v2
# This allows the entrypoint.sh to run 'aws s3 mv'
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
    && unzip awscliv2.zip && ./aws/install \
    && rm -rf awscliv2.zip ./aws

# 3. Install R Packages
# We install only the essentials to keep the image size down
RUN R -e "install.packages(c('Rcpp', 'RcppArmadillo', 'optparse'), repos='https://cloud.r-project.org/')"

# 4. Setup Application Directory and copy things over
WORKDIR /app

# Copy the source code (helpers, worker, and data pools)
# This assumes your build context is the project root
COPY src/ /app/src/

# copy over data as well
COPY data/ /app/data/

# Copy the entrypoint script from the docker folder
COPY docker/entrypoint.sh /app/entrypoint.sh

# 5. Pre-compile C++ Logic
# Doing this during 'build' means the 1,000 workers don't waste 
# time (and money) compiling the same code simultaneously
RUN R -e "Rcpp::sourceCpp('/app/src/stats_functions.cpp')"

# 6. Set Permissions and Entrypoint
RUN chmod +x /app/entrypoint.sh

# Change working directory to src so R can find data easily
WORKDIR /app/src

ENTRYPOINT ["/app/entrypoint.sh"]