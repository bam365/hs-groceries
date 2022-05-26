FROM haskell:8.10.7
WORKDIR /opt/app
RUN cabal update
COPY ./hs-groceries.cabal /opt/app/
RUN cabal build --only-dependencies -j4
COPY . /opt/app/
RUN cabal install
RUN cabal build
EXPOSE 8080 
CMD cabal exec hs-groceries
