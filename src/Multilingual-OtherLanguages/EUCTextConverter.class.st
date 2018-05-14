"
Text converter for Extended Unix Character.  This is an abstract class.  The CJK variations are implemented as subclasses.
"
Class {
	#name : #EUCTextConverter,
	#superclass : #TextConverter,
	#category : #'Multilingual-OtherLanguages-TextConverter'
}

{ #category : #private }
EUCTextConverter >> languageEnvironment [

	self subclassResponsibility

]

{ #category : #private }
EUCTextConverter >> leadingChar [

	^ self subclassResponsibility

]

{ #category : #conversion }
EUCTextConverter >> nextFromStream: aStream [

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
]

{ #category : #conversion }
EUCTextConverter >> nextPut: aCharacter toStream: aStream [ 
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

]

{ #category : #private }
EUCTextConverter >> nonUnicodeClass [

	^ (EncodedCharSet charsetAt: self leadingChar).

]
