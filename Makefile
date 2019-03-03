default : src/main.elm 
	elm make src/main.elm --optimize --output=build/party-status.js  

clean :
	rm output/party-status.js 
