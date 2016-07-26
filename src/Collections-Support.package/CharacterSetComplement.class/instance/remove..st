collection ops
remove: aCharacter
	"This means aCharacter is now absent from myself.
	It must be added to my absent."
	
	byteArrayMapCache := nil.
	^absent add: aCharacter