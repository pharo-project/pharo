tests
testNonBMPCharacterDecoding
	"Characters not in the Basic Multilingual Plane are encoded as a UTF-16 surrogate pair"

	| string object |
	string := String with: 16r1D11E asCharacter. "MUSICAL SYMBOL G CLEF"
	object := (STON fromString: '''\uD834\uDD1E''').
	self assert: object equals: string