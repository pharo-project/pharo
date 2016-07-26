internal
add: newObject to: collectionOwner

	self checkValue: newObject.
	self hasInverse
		ifTrue: [ self addAssociationFrom: collectionOwner to: newObject ]
