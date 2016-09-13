accessing
associationAt: key ifAbsent: aBlock 
	"Answer an association for the given key.
	If the key is not found, return the result of evaluating aBlock.

	Modifying the association won't affect the receiver because it
	isn't implemented with associations like Dictionary."

	| index value |
	index := keys indexOf: key.
	index = 0 ifTrue: [ ^ aBlock value].
	
	value := values at: index.
	^ key->value.