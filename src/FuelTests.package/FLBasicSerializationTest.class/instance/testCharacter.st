tests
testCharacter
	"Test character serialization. If the code is less than 255 the same instance is used. But if it is bigger, new ones are created. For more info read Character class comment and Character class >> value:"

	self assertSerializationIdentityOf: $a.
	
	FLPlatform current isSpur 
		ifTrue: [
			self assertSerializationIdentityOf: (Character value: 12345). "Japanese Hiragana 'A' " ]
		ifFalse: [
			self assertSerializationEqualityOf: (Character value: 12345). "Japanese Hiragana 'A' " ].
		
	self assertSerializationEqualityOf: Character allCharacters.
	self assertSerializationEqualityOf: (Array with: $a with: (Character value: 12345)).