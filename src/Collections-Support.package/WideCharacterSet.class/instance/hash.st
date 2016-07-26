comparing
hash
	"Answer a hash code aimed at storing and retrieving the receiver in a Set or Dictionary.
	Two equal objects should have equal hash.
	Note: as the receiver can be equal to an ordinary CharacterSet,
	the hash code must reflect this"
	
	^self hasWideCharacters
		ifTrue: [map hash]
		ifFalse: [self asCharacterSet hash]