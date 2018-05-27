#!/bin/sh
docker pull foamspace/cliquebait:latest
docker kill watchy-cb
docker rm watchy-cb
docker run --name watchy-cb -d --rm -p 8545:8545 -v `pwd`/cliquebait-generated.json:/cliquebait/cliquebait.json -e ACCOUNTS_TO_CREATE=5 foamspace/cliquebait:latest
