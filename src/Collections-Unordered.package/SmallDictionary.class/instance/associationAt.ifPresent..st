accessing - associations
associationAt: key ifPresent: aBlock
	"Answer the value of evaluating aBlock with an association for the given key.
	If key is not found, return nil."

	| index |
	(index := keys indexOf: key) = 0
		ifTrue: [ ^ nil ].
	^ aBlock cull: key -> (values at: index)