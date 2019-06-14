FROM haskell

RUN apt update && apt install -y freeglut3 libgl-dev libglu-dev

RUN cabal update 

COPY . /usr/app

WORKDIR /usr/app

RUN cabal install gloss

RUN cabal install

CMD ["haskell-game"]