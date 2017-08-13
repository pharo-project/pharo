conversion
nextFromStream: aStream

	| encodedCharacter character1 character2 offset value1 value2 codePoint |
	aStream isBinary ifTrue: [^ aStream basicNext].
	(character1 := aStream basicNext) ifNil: [^ nil].
	character1 asciiValue <= 127 ifTrue: [^ character1].
	(character2 := aStream basicNext) ifNil: [^ nil].
	offset := 16rA1.
	value1 := character1 asciiValue - offset.
	value2 := character2 asciiValue - offset.
	(value1 < 0 or: [value1 > 93]) ifTrue: [^ nil].
	(value2 < 0 or: [value2 > 93]) ifTrue: [^ nil].

	encodedCharacter := (Character leadingChar: self leadingChar code: value1 * 94 + value2) charCode + 1.
	codePoint := (EncodedCharSet charsetAt: self leadingChar) ucsTable at: encodedCharacter.
	^ Character codePoint: codePoint