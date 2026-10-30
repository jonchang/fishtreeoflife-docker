FROM rocker/tidyverse:4.6.1@sha256:6b8523ae3b6a948b5d71c9ac3994026aeac2d79d7f1e7421102207e84534ceae AS build

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libglpk40 \
  && rm -rf /var/lib/apt/lists/*

RUN install2.r --error \
    --ncpus -1 \
    ape \
    future \
    phangorn \
    && rm -rf /tmp/*

COPY downloads/* downloads/

COPY R/lib.R R/

COPY R/generate_taxonomy.R R/
RUN Rscript R/generate_taxonomy.R

COPY R/generate_monophyly.R R/monophy_minimal.R R/
RUN Rscript R/generate_monophyly.R family \
    && Rscript R/generate_monophyly.R order

COPY R/generate_fossils.R R/
RUN Rscript R/generate_fossils.R

RUN mkdir /out && mv assets _fossils _data downloads /out/

# Data only: one layer, no base image.
FROM scratch

LABEL org.opencontainers.image.source="https://github.com/jonchang/fishtreeoflife-docker" \
      org.opencontainers.image.description="Build data for fishtreeoflife.org"

COPY --from=build /out/ /
