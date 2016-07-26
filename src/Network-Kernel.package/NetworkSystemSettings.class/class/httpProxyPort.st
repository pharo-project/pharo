settings
httpProxyPort  
	"Return the HTTP proxy port to use, an Integer"
	
	^ HTTPProxyPort ifNil: [ HTTPProxyPort := 80 ]