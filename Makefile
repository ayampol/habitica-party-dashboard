default : src/main.elm 
	elm make src/main.elm --optimize --output=output/party-status.js  

clean :
	rm output/party-status.js 
