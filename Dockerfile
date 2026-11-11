FROM rocker/tidyverse:4.6.2@sha256:bdf8e2bd8aa43758e0d5663e0df04fa32bcf6394edc5862439e02a195772e9f0 AS r

# The manifest and schemas. `make manifest` builds only these.
FROM r AS compile-manifest

COPY downloads/* downloads/
COPY bundles/ bundles/
COPY R/generate_manifest.R R/
RUN Rscript R/generate_manifest.R

COPY schema/ schema/
RUN mkdir /out && mv data schema /out/

FROM scratch AS manifest
COPY --from=compile-manifest /out/ /

FROM r AS build

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
COPY --from=manifest / /out/

# Data only: one layer, no base image.
FROM scratch

LABEL org.opencontainers.image.source="https://github.com/jonchang/fishtreeoflife-docker" \
      org.opencontainers.image.description="Build data for fishtreeoflife.org"

COPY --from=build /out/ /
