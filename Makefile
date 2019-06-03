default : src/main.elm 
	elm make src/main.elm --optimize --output=build/party-status.js  

debug : src/main.elm
	elm make src/main.elm --output=build/party-status.js  

server : src/Main.hs
	cabal v1-build

hconfig : habitica-party-dashboard.cabal
	cabal v1-configure

clean :
	rm -rf elm-stuff/
