accessing
prefix
	"The path prefix collection under which I am serving files"
	
	^ prefix ifNil: [ prefix := #() ]