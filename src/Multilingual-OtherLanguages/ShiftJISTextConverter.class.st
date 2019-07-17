"
Text converter for Shift-JIS.  Mac and Windows in Japanese mode use this encoding.
"
Class {
	#name : #ShiftJISTextConverter,
	#superclass : #TextConverter,
	#category : #'Multilingual-OtherLanguages-TextConverter'
}

{ #category : #utilities }
ShiftJISTextConverter class >> encodingNames [ 

	^ #('shift-jis' 'shift_jis' 'sjis') copy

]

{ #category : #private }
ShiftJISTextConverter >> katakanaValue: code [

	^ Character leadingChar: JapaneseEnvironment leadingChar code: (#(
	16rFFFD 16rFF61 16rFF62 16rFF63 16rFF64 16rFF65 16rFF66 16rFF67
	16rFF68 16rFF69 16rFF6A 16rFF6B 16rFF6C 16rFF6D 16rFF6E 16rFF6F
	16rFF70 16rFF71 16rFF72 16rFF73 16rFF74 16rFF75 16rFF76 16rFF77
	16rFF78 16rFF79 16rFF7A 16rFF7B 16rFF7C 16rFF7D 16rFF7E 16rFF7F
	16rFF80 16rFF81 16rFF82 16rFF83 16rFF84 16rFF85 16rFF86 16rFF87
	16rFF88 16rFF89 16rFF8A 16rFF8B 16rFF8C 16rFF8D 16rFF8E 16rFF8F
	16rFF90 16rFF91 16rFF92 16rFF93 16rFF94 16rFF95 16rFF96 16rFF97
	16rFF98 16rFF99 16rFF9A 16rFF9B 16rFF9C 16rFF9D 16rFF9E 16rFF9F
) at: (code - 16r9F)).

]

{ #category : #friend }
ShiftJISTextConverter >> leadingChar [

	^ JISX0208 leadingChar

]

{ #category : #conversion }
ShiftJISTextConverter >> nextFromStream: aStream [

	| character1 character2 value1 value2 char1Value result |

	aStream isBinary
		ifTrue: [ ^ aStream basicNext ].
	character1 := aStream basicNext.
	character1 ifNil: [ ^ nil ].
	char1Value := character1 asciiValue.
	char1Value < 16r81
		ifTrue: [ ^ character1 ].
	( char1Value > 16rA0 and: [ char1Value < 16rE0 ] )
		ifTrue: [ ^ self katakanaValue: char1Value ].
	character2 := aStream basicNext.
	character2 ifNil: [ ^ nil	"self errorMalformedInput" ].
	value1 := character1 asciiValue.
	character1 asciiValue >= 224
		ifTrue: [ value1 := value1 - 64 ].
	value1 := value1 - 129 bitShift: 1.
	value2 := character2 asciiValue.
	character2 asciiValue >= 128
		ifTrue: [ value2 := value2 - 1 ].
	character2 asciiValue >= 158
		ifTrue: [ value1 := value1 + 1.
			value2 := value2 - 158
			]
		ifFalse: [ value2 := value2 - 64 ].
	result := Character leadingChar: self leadingChar code: value1 * 94 + value2.
	^ self toUnicode: result
]

{ #category : #conversion }
ShiftJISTextConverter >> nextPut: aCharacter toStream: aStream [ 
	| value leadingChar aChar |
	aStream isBinary ifTrue: [^aCharacter storeBinaryOn: aStream].
	value := aCharacter charCode.
	(value between: 16rFF61 and: 16rFF9F) ifTrue: [
		aStream basicNextPut: (self sjisKatakanaFor: value).
		^ aStream].
	aChar := JISX0208 charFromUnicode: value.
	aChar ifNil: [^ aStream].
	value := aChar charCode.
	leadingChar := aChar leadingChar.
	leadingChar = 0 ifTrue: [
		aStream basicNextPut: (Character value: value).
		^ aStream.
	].
	leadingChar == self leadingChar ifTrue: [
		| upper lower | 
		upper := value // 94 + 33.
		lower := value \\ 94 + 33.
		upper \\ 2 = 1 ifTrue: [
			upper := upper + 1 / 2 + 112.
			lower := lower + 31
		] ifFalse: [
			upper := upper / 2 + 112.
			lower := lower + 125
		].
		upper >= 160 ifTrue: [upper := upper + 64].
		lower >= 127 ifTrue: [lower := lower + 1].
		aStream basicNextPut: (Character value: upper).
		aStream basicNextPut: (Character value: lower).
		^ aStream
	].

]

{ #category : #private }
ShiftJISTextConverter >> sjisKatakanaFor: value [

	^ Character value: (#(
		16rA0 16rA1 16rA2 16rA3 16rA4 16rA5 16rA6 16rA7
		16rA8 16rA9 16rAA 16rAB 16rAC 16rAD 16rAE 16rAF
		16rB0 16rB1 16rB2 16rB3 16rB4 16rB5 16rB6 16rB7
		16rB8 16rB9 16rBA 16rBB 16rBC 16rBD 16rBE 16rBF
		16rC0 16rC1 16rC2 16rC3 16rC4 16rC5 16rC6 16rC7
		16rC8 16rC9 16rCA 16rCB 16rCC 16rCD 16rCE 16rCF
		16rD0 16rD1 16rD2 16rD3 16rD4 16rD5 16rD6 16rD7
		16rD8 16rD9 16rDA 16rDB 16rDC 16rDD 16rDE 16rDF
	) at: value - 16rFF5F).


]

{ #category : #private }
ShiftJISTextConverter >> toUnicode: aChar [

	^ Character leadingChar: JapaneseEnvironment leadingChar code: aChar asUnicode.

]
