"
I represent a 64-bit register. An instance of me can hold any non-negative integer in the range [0..(2^64 - 1)]. Operations are performed on my contents in place, like a hardware register, and results are always modulo 2^64.

This class is similar to the ThirtyTwoBitRegister class but extended to 64 bits.

"
Class {
	#name : 'SixtyFourBitRegister',
	#superclass : 'FixedBitWidthRegister',
	#category : 'System-Hashing-Core',
	#package : 'System-Hashing',
	#tag : 'Core'
}

{ #category : 'accumulator ops' }
SixtyFourBitRegister >> += aSixtyFourBitRegister [
    "Replace my contents with the sum of the given register and my current contents."

    | lowSum |
    lowSum := low + aSixtyFourBitRegister low.
    hi := (hi + aSixtyFourBitRegister hi + (lowSum bitShift: -32)) bitAnd: 16rFFFFFFFF.
    low := lowSum bitAnd: 16rFFFFFFFF
]

{ #category : 'converting' }
SixtyFourBitRegister >> asByteArray [
	"Convert the 64-bit register contents to a byte array.
	The resulting byte_array will be a bytes object, which is an immutable sequence of integers in the range 0-255."

	^ (ByteArray new: 8)
		  at: 1 put: ((hi bitShift: -24) bitAnd: 16rFF);
		  at: 2 put: ((hi bitShift: -16) bitAnd: 16rFF);
		  at: 3 put: ((hi bitShift: -8) bitAnd: 16rFF);
		  at: 4 put: ((hi bitShift: 0) bitAnd: 16rFF);
		  at: 5 put: ((low bitShift: -24) bitAnd: 16rFF);
		  at: 6 put: ((low bitShift: -16) bitAnd: 16rFF);
		  at: 7 put: ((low bitShift: -8) bitAnd: 16rFF);
		  at: 8 put: ((low bitShift: 0) bitAnd: 16rFF);
		  yourself
]

{ #category : 'converting' }
SixtyFourBitRegister >> asInteger [
    "Answer the integer value of my current contents."

    ^ (hi bitShift: 32) + low
]

{ #category : 'converting' }
SixtyFourBitRegister >> asReverseInteger [
    "Answer the byte-swapped integer value of my current contents."

    ^ ((low bitAnd: 16rFF) bitShift: 56) +
       ((low bitAnd: 16rFF00) bitShift: 40) +
       ((low bitAnd: 16rFF0000) bitShift: 24) +
       ((low bitAnd: 16rFF000000) bitShift: 8) +
       ((hi bitAnd: 16rFF) bitShift: -8) +
       ((hi bitAnd: 16rFF00) bitShift: -24) +
       ((hi bitAnd: 16rFF0000) bitShift: -40) +
       ((hi bitAnd: 16rFF000000) bitShift: -56)
]

{ #category : 'accumulator ops' }
SixtyFourBitRegister >> bitInvert [
    "Replace my contents with the bitwise inverse of my current contents."

    hi := hi bitXor: 16rFFFFFFFF.
    low := low bitXor: 16rFFFFFFFF
]

{ #category : 'accumulator ops' }
SixtyFourBitRegister >> bitShift: anInteger [
    "Replace my contents with the bitShift of anInteger."

    | result |
    result := (self asInteger bitShift: anInteger) bitAnd: 16rFFFFFFFF.
    self load: result
]

{ #category : 'accessing' }
SixtyFourBitRegister >> byteAt: anInteger [

	anInteger = 1 ifTrue: [ ^ hi bitShift: -24  ].
	anInteger = 2 ifTrue: [ ^ (hi bitShift: -16) bitAnd: 16rFF ].
	anInteger = 3 ifTrue: [ ^ (hi bitShift: -8) bitAnd: 16rFF ].
	anInteger = 4 ifTrue: [ ^ (hi bitShift: 0) bitAnd: 16rFF ].
	anInteger = 5 ifTrue: [ ^ low bitShift: -24 ].
	anInteger = 6 ifTrue: [ ^ (low bitShift: -16) bitAnd: 16rFF ].
	anInteger = 7 ifTrue: [ ^ (low bitShift: -8) bitAnd: 16rFF].
	anInteger = 8 ifTrue: [ ^ (low bitShift: 0) bitAnd: 16rFF ].
	self errorSubscriptBounds: anInteger 
]

{ #category : 'accumulator ops' }
SixtyFourBitRegister >> leftRotateBy: bits [
	"Rotate my contents left by the given number of bits, retaining exactly 64 bits."

	| bitCount s1 s2 newHi newLow |
	"ensure bitCount is in range [0..64]"
	bitCount := bits \\ 64.
	bitCount < 0 ifTrue: [ bitCount := bitCount + 64 ].
	newHi := hi.
	newLow := low.

	bitCount > 32
		ifTrue: [
			bitCount := bitCount - 32.
			s1 := bitCount - 32.
			s2 := s1 - 32.
			newHi := ((low bitShift: s1) bitAnd: 16rFFFFFFFF) bitOr: (hi bitShift: s2).
			newLow := ((hi bitShift: s1) bitAnd: 16rFFFFFFFF) bitOr: (low bitShift: s2) ]
		ifFalse: [
			s1 := bitCount.
			s2 := s1 - 32.
			newHi := ((hi bitShift: s1) bitAnd: 16rFFFFFFFF) bitOr: (low bitShift: s2).
			newLow := ((low bitShift: s1) bitAnd: 16rFFFFFFFF) bitOr: (hi bitShift: s2) ].

	hi := newHi.
	low := newLow
]

{ #category : 'accessing' }
SixtyFourBitRegister >> load: anInteger [
    "Set my contents to the value of the given 64-bit integer."

    low := anInteger bitAnd: 16rFFFFFFFF.
    hi := (anInteger bitShift: -32) bitAnd: 16rFFFFFFFF.
    self asInteger = anInteger
        ifFalse: [ self error: 'out of range: ', anInteger printString ]
]

{ #category : 'accessing' }
SixtyFourBitRegister >> loadFrom: aByteArray at: index [
	"Load my 64-bit value from the eight bytes of the given ByteArray starting at the given index. 
	Consider the first byte to contain the most significant bits of the word (i.e., use big-endian byte ordering)."

	hi := ((aByteArray at: index) bitShift: 24)
	      + ((aByteArray at: index + 1) bitShift: 16)
	      + ((aByteArray at: index + 2) bitShift: 8)
	      + ((aByteArray at: index + 3) bitShift: 0).
	low := ((aByteArray at: index + 4) bitShift: 24)
	       + ((aByteArray at: index + 5) bitShift: 16)
	       + ((aByteArray at: index + 6) bitShift: 8)
	       + ((aByteArray at: index + 7) bitShift: 0).
]

{ #category : 'accessing' }
SixtyFourBitRegister >> reverseLoadFrom: aByteArray at: index [
    "Load my 64-bit value from the eight bytes of the given ByteArray starting at the given index. 
	Consider the first byte to contain the least significant bits of the word (i.e., use little-endian byte ordering)."

	self loadFrom: aByteArray reversed at: index
]
