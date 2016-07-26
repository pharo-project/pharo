internal
addAssociationFrom: ownerObject to: otherObject
	"A new reference from <ownerObject> to <otherObject> is created. Here we update the other 
	side of the association. If the other side is a ToOne association this means that an other
	association may have to be removed first."

	self inverseSlot isToOneSlot
		ifTrue: [ 
			| oldObject |
			oldObject := inverseSlot read: otherObject.
			oldObject notNil
				ifTrue: [ inverseSlot removeAssociationFrom: otherObject to: oldObject ].
			inverseSlot writeInverse: ownerObject to: otherObject ]
		ifFalse: [ 
			(inverseSlot read: otherObject) inverseAdd: ownerObject ]