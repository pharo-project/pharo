tests
testNonBMPCharacterEncoding
	"Characters not in the Basic Multilingual Plane are encoded as a UTF-16 surrogate pair"

	| string json |
	string := String with: 16r1D11E asCharacter. "MUSICAL SYMBOL G CLEF"
	json := String streamContents: [ :out |
		(STON writer on: out) asciiOnly: true; nextPut: string ].
	self assert: json equals: '''\uD834\uDD1E'''