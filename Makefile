default : src/main.elm 
	elm make src/main.elm --optimize --output=build/party-status.js  

debug : src/main.elm
	elm make src/main.elm --output=build/party-status.js  



clean :
	rm -rf elm-stuff/
