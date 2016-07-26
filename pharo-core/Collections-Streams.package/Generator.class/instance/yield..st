public
yield: anObject 
	"Yield the next value to the consumer of the generator.
	A synonym for #nextPut:"
	^ self nextPut: anObject