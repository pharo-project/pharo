accessing
associationAt: key ifPresent: aBlock
	"Answer the value of evaluating aBlock optionally with an association
	for the given key.
	If the key is not found, return nil.

	Modifying the association won't affect the receiver because it
	isn't implemented with associations like Dictionary."

	| index |
	(index := keys indexOf: key) = 0
		ifTrue: [ ^ nil ].
	^ aBlock cull: key -> (values at: index)