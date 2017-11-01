"
This class implements the MD5 128-bit one-way hash function. It uses the MD5Plugin for better performance. Some methods are taken from the original version of MD5NonPrimitive.
"
Class {
	#name : #MD5,
	#superclass : #HashFunction,
	#instVars : [
		'state'
	],
	#category : 'System-Hashing-MD5'
}

{ #category : #accessing }
MD5 class >> blockSize [
	^ 64
]

{ #category : #accessing }
MD5 class >> hashSize [
	^ 16
]

{ #category : #testing }
MD5 class >> isPluginAvailable [

	<primitive: 'primitivePluginAvailable' module: 'MD5Plugin'>
	^false
]

{ #category : #'instance creation' }
MD5 class >> new [

	self isPluginAvailable 
		ifTrue: [ ^self basicNew ]
		ifFalse: [ ^MD5NonPrimitive basicNew ]
]

{ #category : #private-buffers }
MD5 >> finalValue [

	^state
]

{ #category : #accessing }
MD5 >> hashStream: aPositionableStream [

	| startPosition buf bitLength |
	self initialize.

	aPositionableStream atEnd ifTrue: [
		buf := ByteArray new: 64.
		buf at: 1 put: 128.
		self processBuffer: buf.
		^self finalValue ].

	startPosition := aPositionableStream position.
	[aPositionableStream atEnd] whileFalse: [
		buf := aPositionableStream next: 64 into: (ByteArray new: 64).
		(aPositionableStream atEnd not and: [buf size = 64])
			ifTrue: [self processBuffer: buf]
			ifFalse: [
				bitLength := (aPositionableStream position - startPosition) * 8.
				self processFinalBuffer: buf bitLength: bitLength]].

	^ self finalValue
]

{ #category : #initialization }
MD5 >> initialize [
	"Some magic numbers to get the process started"

	state :=  #[1 35 69 103 137 171 205 239 254 220 186 152 118 84 50 16]


]

{ #category : #private-buffers }
MD5 >> primProcessBuffer: aByteArray withState: s [

	<primitive: 'primitiveProcessBufferWithState' module: 'MD5Plugin'>
	self primitiveFailed
]

{ #category : #private-buffers }
MD5 >> processBuffer: aByteArray [

	self primProcessBuffer: aByteArray withState: state.
	
]

{ #category : #private-buffers }
MD5 >> processFinalBuffer: aByteArray bitLength: bitLength [ 
	"Pad the buffer until we have an even 64 bytes, then transform"
	| out |
	out := ByteArray new: 64.
	out 
		replaceFrom: 1
		to: aByteArray size
		with: aByteArray
		startingAt: 1.
	aByteArray size < 56 ifTrue: 
		[ out 
			at: aByteArray size + 1
			put: 128.	"trailing bit"
		self 
			storeLength: bitLength
			in: out.
		self processBuffer: out.
		^ self ].

	"not enough room for the length, so just pad this one, then..."
	aByteArray size < 64 ifTrue: 
		[ out 
			at: aByteArray size + 1
			put: 128 ].
	self processBuffer: out.

	"process one additional block of padding ending with the length"
	out := ByteArray new: 64.	"filled with zeros"
	aByteArray size = 64 ifTrue: 
		[ out 
			at: 1
			put: 128 ].
	self 
		storeLength: bitLength
		in: out.
	self processBuffer: out
]

{ #category : #private-buffers }
MD5 >> storeLength: bitLength in: aByteArray [ 
	"Fill in the final 8 bytes of the given ByteArray with a 64-bit
	little-endian representation of the original message length in bits."
	| n i |
	n := bitLength.
	i := aByteArray size - 8 + 1.
	[ n > 0 ] whileTrue: 
		[ aByteArray 
			at: i
			put: (n bitAnd: 255).
		n := n bitShift: -8.
		i := i + 1 ]
]
