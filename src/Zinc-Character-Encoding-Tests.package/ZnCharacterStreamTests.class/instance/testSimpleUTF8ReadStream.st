testing
testSimpleUTF8ReadStream
	| string bytes |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	self 
		assert: (ZnCharacterReadStream on: bytes readStream) upToEnd
		equals: string