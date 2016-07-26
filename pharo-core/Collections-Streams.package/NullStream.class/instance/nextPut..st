writing
nextPut: anObject 
	"Insert the argument, anObject, as the next object accessible by the 
	receiver. Answer anObject."

	position := position + 1.
	lastElement := anObject.
	^anObject