FROM --platform=linux/amd64 debian:bookworm

RUN echo "Acquire::http::Pipeline-Depth 0;" > /etc/apt/apt.conf.d/99custom && \
    echo "Acquire::http::No-Cache true;" >> /etc/apt/apt.conf.d/99custom && \
    echo "Acquire::BrokenProxy    true;" >> /etc/apt/apt.conf.d/99custom
RUN rm -rf /var/lib/apt/lists/*
RUN apt-get clean
RUN apt-get update -o Acquire::CompressionTypes::Order::=gz
RUN apt-get upgrade -y

# Install dependencies for building and using GCC
RUN apt-get update && \
    apt-get install -y \
    build-essential \
    gcc-12 \
    curl \
    wget 


# Verify GCC version
RUN gcc --version

# Set the working directory
WORKDIR /app

# Copy any C source files into the container
COPY . .
