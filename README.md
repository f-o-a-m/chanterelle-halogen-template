# chanterelle-halogen-template

[![Build Status](https://travis-ci.org/f-o-a-m/chanterelle-halogen-template.svg?branch=master)](https://travis-ci.org/f-o-a-m/chanterelle-halogen-template)

[hosted here](https://f-o-a-m.github.io/chanterelle-halogen-template/)

## Purpose
This is a template chanterelle project + halogen frontend. Or maybe it's more like an example. The point is to show how to make a simple application written in purescript utilizing

1. chanterelle
2. purescript-web3
3. halogen

The application is fairly simple. It is an event processesor for the [SuperRare](https://superrare.co/) contracts -- it starts polling for the creation events for all tokenized art pieces starting from the deployment block and continuing into the present.


## Instructions

### Build contracts.
```bash
> make install
> make compile-contracts
```


### Run a local ethereum node (in a separate terminal)
```bash
docker run --rm -it -p 8545:8545 -e ACCOUNTS_TO_CREATE=10 foamspace/cliquebait:latest
```

### Test and Deploy contracts
```bash
> make test-purs-dapp
> make deploy-contracts
```

### Build, Serve frontend
```bash
> make build-purs
> make parcel-build
> make parcel-start
```
