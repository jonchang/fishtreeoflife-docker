# fishtreeoflife-docker

GitHub Actions builds this image on every push and weekly, and publishes it to `ghcr.io/jonchang/fishtreeoflife-docker`, tagged with the branch name and commit SHA. The [website build](https://github.com/jonchang/fishtreeoflife.org) pulls `ghcr.io/jonchang/fishtreeoflife-docker:master`.

## Building the Docker image locally

```
make build
```

## Inspecting the image interactively

```
make run
```

## Copying a file

```
docker run --rm --entrypoint cat ghcr.io/jonchang/fishtreeoflife-docker:master /path/to/file > path/to/destination
```

## Building without cache

```
docker build --no-cache -t ghcr.io/jonchang/fishtreeoflife-docker:master .
```

## Pushing manually

CI normally publishes the image. To push a local build, log in with `docker login ghcr.io` first, then run `make push`.
