IMAGE ?= ghcr.io/jonchang/fishtreeoflife-docker:master
OUT ?= out

build:
	docker build -t $(IMAGE) .

files:
	docker buildx build --output type=local,dest=$(OUT) .
