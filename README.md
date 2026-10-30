# fishtreeoflife-docker

Builds the data for [fishtreeoflife.org](https://github.com/jonchang/fishtreeoflife.org) and publishes it as a single-layer image at `ghcr.io/jonchang/fishtreeoflife-docker`. The image holds only data files, with no base image or shell, so it can't be run.

The build is reproducible: the base image is pinned by digest and file timestamps are fixed, so the image digest changes only when the data does. GitHub Actions builds every push and compares the result with the published tag:

- On a push, it publishes the branch and `sha-` tags only if the data changed.
- On a pull request, the job summary says whether the data differs from `master`.
- The weekly build fails if rebuilding `master` changes the data.

## Building locally

Write the data files to `out/`, for the website's `scripts/fetch-data.sh --from out`:

```
make files
```

Or build the image itself:

```
make build
```

## Copying a file from the published image

```
docker create --name fishtree-data --platform linux/amd64 ghcr.io/jonchang/fishtreeoflife-docker:master /bin/true
docker cp fishtree-data:/downloads/tiprates.csv.xz .
docker rm fishtree-data
```
