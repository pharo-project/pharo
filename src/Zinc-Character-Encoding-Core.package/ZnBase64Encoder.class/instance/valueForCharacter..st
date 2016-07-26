private
valueForCharacter: char
	| code |
	(code := char asciiValue) < 128
		ifTrue: [ 
			(inverse at: code + 1)
				ifNotNil: [ :byteValue | ^ byteValue ] ].
	ZnCharacterEncodingError signal: 'Illegal Base64 input'