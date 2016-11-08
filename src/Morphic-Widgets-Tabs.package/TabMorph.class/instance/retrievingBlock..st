accessing
retrievingBlock: anObject

	retrievingBlock := anObject.
	retrievingBlock ifNotNil: [ self softRetrieveMorph ].