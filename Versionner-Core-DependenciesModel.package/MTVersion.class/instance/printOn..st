printing
printOn: aStream
	"Pretty print for a version"
	aStream "nextPutAll: name ;
			nextPut: $- ;
			nextPutAll: author ;
			nextPut: $. ;"
			nextPutAll: versionString asString .