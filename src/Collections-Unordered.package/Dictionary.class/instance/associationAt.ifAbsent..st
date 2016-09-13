accessing
associationAt: key ifAbsent: aBlock
	"Answer the association with the given key.
	If the key is not found, return the result of evaluating aBlock."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	nil == assoc ifTrue: [ ^ aBlock value ].
	^ assoc