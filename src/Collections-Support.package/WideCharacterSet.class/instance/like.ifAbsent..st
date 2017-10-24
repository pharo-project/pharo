accessing
like: aCharacter ifAbsent: aBlock
	"Answer an object in the receiver that is equal to anObject,
	or evaluate the block if not found. Relies heavily on hash properties"

	^ (self includes: aCharacter)
		ifTrue: [ aCharacter ]
		ifFalse: [ aBlock value ]