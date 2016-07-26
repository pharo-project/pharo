accessing - associations
associationAt: key ifAbsent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	| index value |
	index := keys indexOf: key.
	index = 0 ifTrue: [ ^ aBlock value].
	
	value := values at: index.
	^ key->value.