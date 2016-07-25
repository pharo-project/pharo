accessing
associationAt: key ifPresent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	^(array at: (self findElementOrNil: key))
		ifNotNil: [:assoc | aBlock cull: assoc]