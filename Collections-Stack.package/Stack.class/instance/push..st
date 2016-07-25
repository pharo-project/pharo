adding
push: anObject 
	"Adds a new object of any kind on top of the stack."
	self addFirst: anObject.
	^ anObject.