conversion
nextPut: aCharacter toStream: aStream 
	| value leadingChar nonUnicodeChar value1 value2 |
	aStream isBinary ifTrue: [^aCharacter storeBinaryOn: aStream].
	value := aCharacter charCode.
	leadingChar := aCharacter leadingChar.
	(leadingChar = 0 and: [value < 128]) ifTrue: [
		aStream basicNextPut: (Character value: value).
		^ aStream
	].

	(128 <= value and: [value < 256]) ifTrue: [^ aStream].
	nonUnicodeChar := self nonUnicodeClass charFromUnicode: value.
	nonUnicodeChar ifNotNil: [
		value := nonUnicodeChar charCode.
		value1 := value // 94 + 161.
		value2 := value \\ 94 + 161.
		aStream basicNextPut: (Character value: value1).
		aStream basicNextPut: (Character value: value2).
		^ aStream
	]
