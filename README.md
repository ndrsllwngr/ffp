# ffp
![Haskell CI](https://github.com/ndrsllwngr/ffp/workflows/Haskell%20CI/badge.svg?branch=develop)

Fortgeschrittene Funktionale Programmierung - Group project (2020)

- Minesweepskell - a fun(ctional) Web-Minesweeper Client

## Description

- Minesweepskell - a fun(ctional) Web-Minesweeper Client
- Usage of monads (incl. monad-transformer, lifting, advanced usage of state monads)
- Server with REST-Api, manages game state and provides game logic (YESOD)
- database connection to persist data of scores as well as game states to resume the game at a later time
- Add simple KI which assists the gamer (Automated Problem Solving)
- (Unit) test logic (Testing)

## Installation
Run in the project folder `stack build`.

### Requirements
```bash
# install haskell-stack (includes ghc, package manager, cli), if it fails run `xcode-select --install before`
brew install haskell-stack

# install yesod cli
stack install yesod-bin --install-ghc
```

## Usage
### Development
Start a server with
```
export JWT_SECRET="some secret value"
stack exec -- yesod devel
```
it watches for changes and recompiles the project automatically. 

## Dependencies
```
yesod
```

For more details take a look at the [package.yaml](package.yaml).

## Contributors

- Andreas Ellwanger
- Timo Erdelt
- Andreas Griesbeck

### Individual contributions

Due too the small group size of 3 it is impossible for us to properly distinguish what of our project has been done by whom. We all worked on all parts of our application, especially since we mostly did “pair-programming” (with two or often all three of us working together). So all of us were equally involved in all parts of our application.
We would be happy to answer questions about our development process, aswell as our individual/collective contributions at the examination.
