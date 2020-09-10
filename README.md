# ffp
![Haskell CI](https://github.com/ndrsllwngr/ffp/workflows/Haskell%20CI/badge.svg?branch=develop)

Fortgeschrittene Funktionale Programmierung - Group project (2020)

- Minesweepskell - a fun(ctional) Web-Minesweeper Client

## Description

- Minesweepskell - a fun(ctional) Web-Minesweeper Client
- ~Usage of monads (incl. monad-transformer, lifting, advanced usage of state monads)~
- Server with REST-Api, manages game state and provides game logic (YESOD)
- database connection to persist data of scores as well as game states to resume the game at a later time
- ~Add simple KI which assists the gamer (Automated Problem Solving)~
- (Unit) test logic (Testing)

## Installation
Run in the project folder `stack build`.

### Requirements
```bash
$ stack --version
Version 2.3.3 x86_64

$ yesod version
yesod-bin version: 1.6.0.6
```

#### Install Haskell and Yesod
1. Install [haskell-stack](http://haskellstack.org/) and [yesod](https://www.yesodweb.com/)
```bash
# - if it fails run `xcode-select --install before`
brew install haskell-stack
stack install yesod-bin --install-ghc
```
2. Add yesod installation path to your shell
```
# ~/.zshrc
export PATH=$HOME/.local/bin:$PATH
```
If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

#### Install MongoDB
1. Install MongoDB
```sh
brew tap mongodb/brew
brew install mongodb-community@4.4
```
2. Run MongoDB
```sh
brew services start mongodb-community@4.4
brew services stop mongodb-community@4.4
```
3. Verify that it is running `ps aux | grep -v grep | grep mongod`, logs can be found here `/usr/local/var/log/mongodb/mongo.log.` and settings here `/usr/local/etc/mongod.conf`

For more information take a look at https://docs.mongodb.com/manual/tutorial/install-mongodb-on-os-x/

##### Use MongoDB Compass as GUI
- https://www.mongodb.com/products/compass

##### Use DataGrip as GUI
- https://www.jetbrains.com/de-de/datagrip/ 
- 
![Screenshot 2020-08-30 at 16 33 59](https://user-images.githubusercontent.com/10077258/91661825-aa5bbe80-eade-11ea-98ce-4a0572999590.png)

## Usage
### Development
- Build libraries: `stack build`
- Start a development server with `stack exec -- yesod devel`, it watches for changes and recompiles the project automatically. 
- Run `stack clean --full && stack build` to clean the project and to rebuild it.

##### ADJUST GHC PROMPT
- add the following to your `~/.ghc/ghci.conf`
```haskell
:set prompt      λ: 
:set prompt-cont λ| 
```
https://stackoverflow.com/a/47694134

### Tests
```
stack test --flag minesweepskell:library-only --flag minesweepskell:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).


## Dependencies
```
yesod
mongodb
persistent
lens
...
```

For more details take a look at the [package.yaml](package.yaml).

## Contributors

- Andreas Ellwanger
- Timo Erdelt
- Andreas Griesbeck

### Individual contributions

Due too the small group size of 3 it is impossible for us to properly distinguish what of our project has been done by whom. We all worked on all parts of our application, especially since we mostly did “pair-programming” (with two or often all three of us working together). So all of us were equally involved in all parts of our application.
We would be happy to answer questions about our development process, aswell as our individual/collective contributions at the examination.
