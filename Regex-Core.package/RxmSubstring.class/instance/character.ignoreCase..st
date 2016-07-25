initialize-release
character: aCharacter ignoreCase: aBoolean
	"Match exactly this character."

	sample := String with: aCharacter.
	aBoolean ifTrue: [self beCaseInsensitive]