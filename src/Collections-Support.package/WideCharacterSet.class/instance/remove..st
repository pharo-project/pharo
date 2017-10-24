removing
remove: aCharacter 
	^ self remove: aCharacter ifAbsent: [ self errorNotFound: aCharacter ]