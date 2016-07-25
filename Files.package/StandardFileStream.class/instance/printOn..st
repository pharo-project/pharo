printing
printOn: aStream
	"Put a printed version of the receiver onto aStream."

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name