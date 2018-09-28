# Functional Ethereum

1. [Intro](#introduction)
2. [FOAM architecture](#foam-architecture)
3. [Chanterelle](#chanterelle)
4. [Cliquebait](#cliquebait)

## Introduction


.

.

.


### Personal
- Technical Background:
  - Mathematics
  - Functional Programming


.

.

.

 
- Open source software
  - PureScript (ethereum, data processing)
  - Haskell (ethereum, web)


.

.

.


### My Work
- Senior Blockchain Engineer at FOAM
- Mainnet application at [map.foam.space](map.foam.space)
- Proof of Location [Beta application](https://beta.foam.space) (rinkeby)
- Previously enterprise Ethereum development and consulting


.
.
.


### Topics
- FOAM architecture
- chanterelle
- purescript


.
.
.


## FOAM Architecture


.
.
.


### Languages
- Statically typed, purely functional stack -- all software written in Haskell or PureScript
- High degree of certainty about how the software behaves.
- Offload work to the compiler.


.
.
.


### Ethereum App
- The main net application consists of 4 parts
  - indexer -- driven by ethereum log activity, indexes smart contract data in various dbs
  - server -- REST API, offers global views, aggregation, search, and websockets
  - ui -- browser application, visualizations, discovery, account management
  - ethereum blockchian (parity)


.
.
.



### Diagram


.
.
.


![FOAM architecture](https://github.com/f-o-a-m/mayday-presentation/blob/master/images/foam-architecture.png)


.
.
.


## Chanterelle


.
.
.


### What is it?
- Chanterelle was originally developed to replace Truffle.js at FOAM
  - At the time we had no good js developers
  - Makes a lot of assumptions about how you want to use it
- It offers
  1. Deployment and Migrations
  2. Testing setup
  3. Code generation
  

.
.
.

