"
Text converter for UTF-8.  Since the BOM is used to distinguish the MacRoman code and UTF-8 code, BOM is written for UTF-8 by #writeBOMOn: which is called by client.
"
Class {
	#name : #UTF8TextConverter,
	#superclass : #TextConverter,
	#category : #Deprecated10
}

{ #category : #converting }
UTF8TextConverter >> errorMalformedInput [
	^UTF8InvalidText new signal: 'Invalid utf8 input detected'
]

{ #category : #friend }
UTF8TextConverter >> leadingChar [

	^ self shouldNotImplement

]

{ #category : #converting }
UTF8TextConverter >> nextFromStream: aStream [

	| character1 value1 character2 value2 unicode character3 value3 character4 value4 |

	aStream isBinary
		ifTrue: [ ^ aStream basicNext ].
	character1 := aStream basicNext.
	character1 ifNil: [ ^ nil ].
	value1 := character1 asciiValue.
	value1 <= 127
		ifTrue: [ "1-byte character" ^ character1 ].	"at least 2-byte character"
	character2 := aStream basicNext.
	character2 ifNil: [ ^ self errorMalformedInput ].
	value2 := character2 asciiValue.
	( value1 bitAnd: 16rE0 ) = 192
		ifTrue: [ ^ Unicode value: ( ( value1 bitAnd: 31 ) bitShift: 6 ) + ( value2 bitAnd: 63 ) ].	"at least 3-byte character"
	character3 := aStream basicNext.
	character3 ifNil: [ ^ self errorMalformedInput ].
	value3 := character3 asciiValue.
	( value1 bitAnd: 16rF0 ) = 224
		ifTrue: [ unicode := ( ( value1 bitAnd: 15 ) bitShift: 12 ) + ( ( value2 bitAnd: 63 ) bitShift: 6 )
				+ ( value3 bitAnd: 63 )
			].
	( value1 bitAnd: 16rF8 ) = 240
		ifTrue: [ "4-byte character"
			character4 := aStream basicNext.
			character4 ifNil: [ ^ self errorMalformedInput ].
			value4 := character4 asciiValue.
			unicode := ( ( value1 bitAnd: 16r7 ) bitShift: 18 ) + ( ( value2 bitAnd: 63 ) bitShift: 12 )
				+ ( ( value3 bitAnd: 63 ) bitShift: 6 ) + ( value4 bitAnd: 63 )
			].
	unicode ifNil: [ ^ self errorMalformedInput ].
	unicode > 16r10FFFD
		ifTrue: [ ^ self errorMalformedInput ].
	unicode = 16rFEFF
		ifTrue: [ ^ self nextFromStream: aStream ].
	^ Unicode value: unicode
]
