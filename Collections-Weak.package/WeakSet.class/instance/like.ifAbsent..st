accessing
like: anObject ifAbsent: aBlock
	"Answer an object in the receiver that is equal to anObject,
	or evaluate the block if not found. Relies heavily on hash properties"

	| element |
	((element  := array at: (self scanFor: anObject)) == flag or: [ element == nil ])
		ifTrue: [ ^aBlock value ]
		ifFalse: [ ^element enclosedSetElement ]