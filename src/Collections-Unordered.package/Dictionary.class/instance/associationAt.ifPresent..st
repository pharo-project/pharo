accessing
associationAt: key ifPresent: aBlock
	"Answer the value of evaluating aBlock with the association for the given key.
	If key is not found, return nil."

	^(array at: (self findElementOrNil: key))
		ifNotNil: [:assoc | aBlock cull: assoc]