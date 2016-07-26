read, write, position
nextPut: anObject 
	"Insert the argument, anObject, as the next object accessible by the 
	receiver. Answer anObject."

	^ writer nextPut: anObject