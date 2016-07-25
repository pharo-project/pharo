testing
testPeek
	| string bytes readStream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	readStream := ZnCharacterReadStream on: bytes readStream.
	self assert: readStream peek equals: 'é' first.
	self assert: readStream peek equals: 'é' first.
	self assert: readStream next equals: 'é' first.
	readStream skip: 'lève ' size.
	self assert: readStream peek equals: $e.
	self assert: (readStream next: 'en Français' size) equals: 'en Français'