fields accessing
indexableFieldOf: anObject at: anIndex put: newObject
	"Primitive. Assumes receiver is indexable. Store the argument value in 
	the indexable element of the receiver indicated by index. Fail if the 
	index is not an Integer or is out of bounds. Or fail if the value is not of 
	the right type for this kind of collection. Answer the value that was 
	stored"

	<primitive: 61>
	anIndex isInteger ifTrue:
		[(self classOf: anObject) isVariable
			ifTrue: [(anIndex between: 1 and: (self indexableSizeOf: anObject))
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: anIndex]]
			ifFalse: [self errorNotIndexableFor: anObject]].
	
	self errorNonIntegerIndex