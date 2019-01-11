FROM rocker/tidyverse:3.5.2

RUN install2.r --error --deps TRUE ape future MonoPhy

COPY downloads/* downloads/

COPY scripts/* scripts/

RUN Rscript scripts/generate_taxonomy.R family \
    && Rscript scripts/generate_taxonomy.R order \
    && Rscript scripts/generate_monophyly.R family \
    && Rscript scripts/generate_monophyly.R order \
    && Rscript scripts/generate_fossils.R \
    && rm -rf scripts/*

CMD ["R"]
