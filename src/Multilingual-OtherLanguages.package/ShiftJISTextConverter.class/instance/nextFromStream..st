conversion
nextFromStream: aStream
	| character1 character2 value1 value2 char1Value result |
	aStream isBinary
		ifTrue: [ ^ aStream basicNext ].
	character1 := aStream basicNext.
	character1 isNil
		ifTrue: [ ^ nil ].
	char1Value := character1 asciiValue.
	char1Value < 16r81
		ifTrue: [ ^ character1 ].
	(char1Value > 16rA0 and: [ char1Value < 16rE0 ])
		ifTrue: [ ^ self katakanaValue: char1Value ].
	character2 := aStream basicNext.
	character2 isNil
		ifTrue: [ ^ nil	"self errorMalformedInput" ].
	value1 := character1 asciiValue.
	character1 asciiValue >= 224
		ifTrue: [ value1 := value1 - 64 ].
	value1 := value1 - 129 bitShift: 1.
	value2 := character2 asciiValue.
	character2 asciiValue >= 128
		ifTrue: [ value2 := value2 - 1 ].
	character2 asciiValue >= 158
		ifTrue: [ 
			value1 := value1 + 1.
			value2 := value2 - 158 ]
		ifFalse: [ value2 := value2 - 64 ].
	result := Character leadingChar: self leadingChar code: value1 * 94 + value2.
	^ self toUnicode: result