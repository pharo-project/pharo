accessing
associationAt: key ifPresent: aBlock
	"Answer the value of evaluating aBlock optionally with the association
	for the given key.
	If the key is not found, return nil."

	^(array at: (self findElementOrNil: key))
		ifNotNil: [:assoc | aBlock cull: assoc]