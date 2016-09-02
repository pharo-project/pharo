fields accessing
indexableFieldOf: anObject at: anIndex 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	indexable element in the receiver. Fail if the argument index is not an 
	Integer or is out of bounds. Essential. See Object documentation 
	whatIsAPrimitive. Read the class comment for a discussion about that the fact
	that the index can be a float."

	<primitive: 60>
	anIndex isInteger ifTrue:
		[(self classOf: anObject) isVariable
			ifTrue: [self errorSubscriptBounds: anIndex]
			ifFalse: [self errorNotIndexableFor: anObject]].
		
	self errorNonIntegerIndex