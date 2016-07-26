printing
printOn: aStream
	"Print a description of the complement rather than self.
	Rationale: self would be too long to print."
	
	aStream nextPut: $(.
	absent printOn: aStream.
	aStream nextPut: $); space; nextPutAll: #complement.