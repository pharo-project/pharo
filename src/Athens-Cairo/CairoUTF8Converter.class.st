"
i use persistent buffer to convert strings to utf-8 strings (an input neede by cairo)
"
Class {
	#name : #CairoUTF8Converter,
	#superclass : #Object,
	#instVars : [
		'buffer',
		'encoder',
		'bs'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #accessing }
CairoUTF8Converter >> buffer [
	^ buffer
]

{ #category : #converting }
CairoUTF8Converter >> checkBufferSpace: minimalSize [
	| size |

	size := buffer ifNil: [ 0 ] ifNotNil: [ buffer size ].

	size < minimalSize ifTrue: [
		buffer := ByteArray new: minimalSize.
		bs := buffer writeStream.

		]
]

{ #category : #converting }
CairoUTF8Converter >> convert: aString from: start to: end [

	self checkBufferSpace: 4*(end-start + 2).
	bs reset.

	encoder next: end-start+1
		putAll: aString
		startingAt: start
		toStream: bs .

	"DO NOT NULL-Terminate because its not used"

	^ bs position
]

{ #category : #converting }
CairoUTF8Converter >> convertChar: aCharacter [

	self checkBufferSpace: 8.

	bs reset.
	encoder nextPut: aCharacter toStream: bs.
	bs nextPut: 0.
	^ buffer
]

{ #category : #initialization }
CairoUTF8Converter >> initialize [
	encoder := ZnUTF8Encoder new
]
