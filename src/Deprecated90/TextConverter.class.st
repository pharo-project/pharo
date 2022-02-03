"
The abstract class for all different type of text converters.  nextFromStream: and nextPut:toStream: are the public accessible methods.  If you are going to make a subclass for a stateful text conversion, you should override restoreStateOf:with: and saveStateOf: along the line of CompoundTextConverter.

"
Class {
	#name : #TextConverter,
	#superclass : #Object,
	#instVars : [
		'latin1Map',
		'latin1Encodings'
	],
	#classInstVars : [
		'latin1Map',
		'latin1Encodings'
	],
	#category : #Deprecated90
}

{ #category : #'instance creation' }
TextConverter class >> default [

	^ UTF8TextConverter new.

]

{ #category : #accessing }
TextConverter class >> initializeLatin1MapAndEncodings [
	"Initialize the latin1Map and latin1Encodings.
	These variables ensure that conversions from latin1 ByteString is reasonably fast"
	
	| latin1 utf8 |
	latin1Map := ByteArray new: 256.
	latin1Encodings := Array new: 256.
	0 to: 255 do:[:i|
		utf8 := (String new: 8) writeStream.
		latin1 := String with: (Character value: i).
		self new nextPut: latin1 first toStream: utf8.
		utf8 := utf8 contents.
		latin1 = utf8 ifTrue:[
			latin1Map at: i+1 put: 0. "no translation needed"
		] ifFalse:[
			latin1Map at: i+1 put: 1. "translation needed"
			latin1Encodings at: i+1 put: utf8.
		].
	].
]

{ #category : #accessing }
TextConverter class >> latin1Encodings [
	"Answer an Array mapping latin1 characters to conversion string"

	^latin1Encodings ifNil:
		[self initializeLatin1MapAndEncodings.
		latin1Encodings]
]

{ #category : #accessing }
TextConverter class >> latin1Map [
	"Answer a ByteArray map telling if latin1 characters needs conversion or not"

	^latin1Map ifNil:
		[self initializeLatin1MapAndEncodings.
		latin1Map]
]

{ #category : #initialization }
TextConverter >> initialize [
	latin1Map := self class latin1Map.
	latin1Encodings := self class latin1Encodings.
]

{ #category : #converting }
TextConverter >> next: anInteger putAll: aString startingAt: startIndex toStream: aStream [
	"Handle fast conversion if ByteString"
             
   aString class == ByteString ifFalse: [
		startIndex to: startIndex + anInteger - 1 do: [ :index | 
			self nextPut: (aString at: index) toStream: aStream ].
      ^aString ].
	aStream isBinary ifTrue: [
		aStream basicNext: anInteger putAll: aString startingAt: startIndex.
		^aString ].
        
	^self next: anInteger putByteString: aString startingAt: startIndex toStream: aStream
]

{ #category : #private }
TextConverter >> next: anAmount putByteString: aString startingAt: startIndex toStream: aStream [

	"This is a reasonable default which speeds up conversion of encodings which have a partial match with latin1 encoding (for instance ascii supersets)"
	| lastIndex nextIndex |
	lastIndex := startIndex.
	[nextIndex := ByteString findFirstInString: aString inSet: latin1Map startingAt: lastIndex.
	nextIndex = 0 or: [nextIndex >= (startIndex + anAmount) ]] whileFalse:
		[aStream next: nextIndex-lastIndex putAll: aString startingAt: lastIndex.
		aStream basicNextPutAll: (latin1Encodings at: (aString byteAt: nextIndex)+1).
		lastIndex := nextIndex + 1].
	aStream basicNext: anAmount - lastIndex + startIndex putAll: aString startingAt: lastIndex.
]
