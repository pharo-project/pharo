internal
remove: oldObject from: collectionOwner

	self hasInverse
		ifTrue: [ self removeAssociationFrom: collectionOwner to: oldObject ]
