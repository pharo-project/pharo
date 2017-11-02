"
I represent a 32-bit register. An instance of me can hold any non-negative integer in the range [0..(2^32 - 1)]. Operations are performed on my contents in place, like a hardware register, and results are always modulo 2^32.

This class is primarily meant for use by the SecureHashAlgorithm class.

"
Class {
	#name : #ThirtyTwoBitRegister,
	#superclass : #Object,
	#instVars : [
		'hi',
		'low'
	],
	#category : 'System-Hashing-Core'
}

{ #category : #'instance creation' }
ThirtyTwoBitRegister class >> new [
	"Answer a new instance whose initial contents is zero."

	^ super new load: 0

]

{ #category : #'accumulator ops' }
ThirtyTwoBitRegister >> += aThirtTwoBitRegister [
	"Replace my contents with the sum of the given register and my current contents."

	| lowSum |
	lowSum := low + aThirtTwoBitRegister low.
	hi := (hi + aThirtTwoBitRegister hi + (lowSum bitShift: -16)) bitAnd: 16rFFFF.
	low := lowSum bitAnd: 16rFFFF.

]

{ #category : #converting }
ThirtyTwoBitRegister >> asByteArray [
	^ ByteArray with: (low bitAnd: 16rFF) with: (low bitShift: -8) with: (hi bitAnd: 16rFF) with: (hi bitShift: -8)
]

{ #category : #accessing }
ThirtyTwoBitRegister >> asInteger [
	"Answer the integer value of my current contents."

	^ (hi bitShift: 16) + low

]

{ #category : #converting }
ThirtyTwoBitRegister >> asReverseInteger [
	"Answer the byte-swapped integer value of my current contents."

	^ ((low bitAnd: 16rFF) bitShift: 24) +
       ((low bitAnd: 16rFF00) bitShift: 8) +
	  ((hi bitAnd: 16rFF) bitShift: 8) +
       (hi bitShift: -8)

]

{ #category : #'accumulator ops' }
ThirtyTwoBitRegister >> bitAnd: aThirtTwoBitRegister [
	"Replace my contents with the bitwise AND of the given register and my current contents."

	hi := hi bitAnd: aThirtTwoBitRegister hi.
	low := low bitAnd: aThirtTwoBitRegister low.

]

{ #category : #'accumulator ops' }
ThirtyTwoBitRegister >> bitInvert [
	"Replace my contents with the bitwise inverse my current contents."

	hi := hi bitXor: 16rFFFF.
	low := low bitXor: 16rFFFF.

]

{ #category : #'accumulator ops' }
ThirtyTwoBitRegister >> bitOr: aThirtTwoBitRegister [
	"Replace my contents with the bitwise OR of the given register and my current contents."

	hi := hi bitOr: aThirtTwoBitRegister hi.
	low := low bitOr: aThirtTwoBitRegister low.

]

{ #category : #'accumulator ops' }
ThirtyTwoBitRegister >> bitShift: anInteger [
	"Replace my contents with the bitShift of anInteger."
	self load: (self asInteger bitShift: anInteger). 
]

{ #category : #'accumulator ops' }
ThirtyTwoBitRegister >> bitXor: aThirtTwoBitRegister [
	"Replace my contents with the bitwise exclusive OR of the given register and my current contents."

	hi := hi bitXor: aThirtTwoBitRegister hi.
	low := low bitXor: aThirtTwoBitRegister low.

]

{ #category : #accessing }
ThirtyTwoBitRegister >> byte1: hi1 byte2: hi2 byte3: low1 byte4: low2 [
	hi := (hi1 bitShift: 8) + hi2.
	low := (low1 bitShift: 8) + low2.
]

{ #category : #accessing }
ThirtyTwoBitRegister >> byteAt: anInteger [
	anInteger = 1 ifTrue: [^ hi bitShift: -8].
	anInteger = 2 ifTrue: [^ hi bitAnd: 16rFF].
	anInteger = 3 ifTrue: [^ low bitShift: -8].
	anInteger = 4 ifTrue: [^ low bitAnd: 16rFF]
]

{ #category : #copying }
ThirtyTwoBitRegister >> copy [
	"Use the clone primitive for speed."

	<primitive: 148>
	^ super copy

]

{ #category : #accessing }
ThirtyTwoBitRegister >> hi [

	^ hi

]

{ #category : #'accumulator ops' }
ThirtyTwoBitRegister >> leftRotateBy: bits [
	"Rotate my contents left by the given number of bits, retaining exactly 32 bits."
	"Details: Perform this operation with as little LargeInteger arithmetic as possible."

	| bitCount s1 s2 newHi |
	"ensure bitCount is in range [0..32]"
	bitCount := bits \\ 32.
	bitCount < 0 ifTrue: [bitCount := bitCount + 32].

	bitCount > 16
		ifTrue: [
			s1 := bitCount - 16.
			s2 := s1 - 16.
			newHi := ((low bitShift: s1) bitAnd: 16rFFFF) bitOr: (hi bitShift: s2).
			low := ((hi bitShift: s1) bitAnd: 16rFFFF) bitOr: (low bitShift: s2).
			hi := newHi]
		ifFalse: [
			s1 := bitCount.
			s2 := s1 - 16.
			newHi := ((hi bitShift: s1) bitAnd: 16rFFFF) bitOr: (low bitShift: s2).
			low := ((low bitShift: s1) bitAnd: 16rFFFF) bitOr: (hi bitShift: s2).
			hi := newHi]

]

{ #category : #accessing }
ThirtyTwoBitRegister >> load: anInteger [
	"Set my contents to the value of given integer."

	low := anInteger bitAnd: 16rFFFF.
	hi := (anInteger bitShift: -16) bitAnd: 16rFFFF.
	self asInteger = anInteger
		ifFalse: [self error: 'out of range: ', anInteger printString].

]

{ #category : #accessing }
ThirtyTwoBitRegister >> loadFrom: aByteArray at: index [
	"Load my 32-bit value from the four bytes of the given ByteArray starting at the given index. Consider the first byte to contain the most significant bits of the word (i.e., use big-endian byte ordering)."

	hi := ((aByteArray at: index) bitShift: 8) + ( aByteArray at: index + 1).
	low := ((aByteArray at: index + 2) bitShift: 8) + ( aByteArray at: index + 3).

]

{ #category : #accessing }
ThirtyTwoBitRegister >> low [

	^ low
]

{ #category : #printing }
ThirtyTwoBitRegister >> printOn: aStream [
	"Print my contents in hex with a leading 'R' to show that it is a register object being printed."

	aStream nextPutAll: 'R:'.
	self asInteger storeOn: aStream base: 16.

]

{ #category : #accessing }
ThirtyTwoBitRegister >> reverseLoadFrom: aByteArray at: index [
	"Load my 32-bit value from the four bytes of the given ByteArray
starting at the given index. Consider the first byte to contain the most
significant bits of the word (i.e., use big-endian byte ordering)."

	hi := ((aByteArray at: index + 3) bitShift: 8) + ( aByteArray at: index + 2).
	low := ((aByteArray at: index + 1) bitShift: 8) + ( aByteArray at: index).

]

{ #category : #accessing }
ThirtyTwoBitRegister >> storeInto: aByteArray at: index [
	"Store my 32-bit value into the four bytes of the given ByteArray starting at the given index. Consider the first byte to contain the most significant bits of the word (i.e., use big-endian byte ordering)."

	aByteArray at: index put: (hi bitShift: -8).
	aByteArray at: index + 1 put: (hi bitAnd: 16rFF).
	aByteArray at: index + 2 put: (low bitShift: -8).
	aByteArray at: index + 3 put: (low bitAnd: 16rFF)
]
