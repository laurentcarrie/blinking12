#!/usr/bin/env bash

docker rm $(docker ps -a -q)

docker rmi -f $(docker images -q)

docker build --tag=toto x-toto

docker run -it --rm --name toto toto



