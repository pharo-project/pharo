accessing
mailToAddress
	"Assuming my scheme is #mailto, return the address."
	
	^ self username, '@', self host 