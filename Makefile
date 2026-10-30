IMAGE ?= ghcr.io/jonchang/fishtreeoflife-docker:master

run:
	docker run -it $(IMAGE)

build:
	docker build -t $(IMAGE) .

push:
	docker push $(IMAGE)
