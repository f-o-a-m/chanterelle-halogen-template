# chanterelle-halogen-template

[hosted here](https://f-o-a-m.github.io/chanterelle-halogen-template/)

## Purpose
This is a template chanterelle project + halogen frontend. It comes with boiler plate for deploying an ERC20 token (NavelCoin).
It also has a lot of configuration for runing a Halogen app to support this contract.

## Instructions

### Build contracts.
```bash
> make install
> make compile-contracts
```


### Run a local ethereum node (in a separate terminal)
```bash
docker run --rm -p 8545:8545 -e ACCOUNTS_TO_CREATE=3 foamspace/cliquebait:latest
```

### Test and Deploy contracts
```bash
> make test-purs-dapp
> make deploy-contracts
```

### Build, Serve frontend
```bash
> make build-purs
> make parcel-start
```
