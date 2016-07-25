printing
printOn: aStream 
	"The receiver prints on aStream in terms of infix notation."
	
	aStream nextPut: $(.
	x printOn: aStream.
	aStream nextPut: $@.
	(y notNil and: [y negative])
		ifTrue: [
			"Avoid ambiguous @- construct"
			aStream space].
	y printOn: aStream.
	aStream nextPut: $).