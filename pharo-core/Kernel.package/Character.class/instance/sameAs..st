comparing
sameAs: aCharacter 
	"Answer whether the receiver is equal to aCharacter, ignoring case"
	^ (self asLowercase = aCharacter asLowercase)	