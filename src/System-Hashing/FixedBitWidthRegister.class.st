"
Superclass for fixed bit width registers

Subclass implementations use big-endian byte order, where the most significant bytes are stored at lower indices in the ByteArray. This is the standard byte order for representing integers in network protocols and many file formats.
"
Class {
	#name : 'FixedBitWidthRegister',
	#superclass : 'Object',
	#instVars : [
		'low',
		'hi'
	],
	#category : 'System-Hashing-Core',
	#package : 'System-Hashing',
	#tag : 'Core'
}

{ #category : 'instance creation' }
FixedBitWidthRegister class >> new [
	"Answer a new instance whose initial contents is zero."

	^ super new load: 0
]

{ #category : 'converting' }
FixedBitWidthRegister >> asInteger [
	"Answer the integer value of my current contents."
	
	self subclassResponsibility
]

{ #category : 'accumulator ops' }
FixedBitWidthRegister >> bitAnd: aBitRegister [
    "Replace my contents with the bitwise AND of the given register and my current contents."

    hi := hi bitAnd: aBitRegister hi.
    low := low bitAnd: aBitRegister low
]

{ #category : 'accumulator ops' }
FixedBitWidthRegister >> bitOr: aBitRegister [
    "Replace my contents with the bitwise OR of the given register and my current contents."

    hi := hi bitOr: aBitRegister hi.
    low := low bitOr: aBitRegister low
]

{ #category : 'accumulator ops' }
FixedBitWidthRegister >> bitXor: aBitRegister [
    "Replace my contents with the bitwise exclusive OR of the given register and my current contents."

    hi := hi bitXor: aBitRegister hi.
    low := low bitXor: aBitRegister low
]

{ #category : 'copying' }
FixedBitWidthRegister >> copy [
	"Use the clone primitive for speed."

	<primitive: 148>
	^ super copy
]

{ #category : 'accessing' }
FixedBitWidthRegister >> hi [

	^ hi
]

{ #category : 'accessing' }
FixedBitWidthRegister >> low [

	^ low
]

{ #category : 'printing' }
FixedBitWidthRegister >> printOn: aStream [
	"Print my contents in hex with a leading 'R' to show that it is a register object being printed."

	aStream nextPutAll: 'R:'.
	self asInteger storeOn: aStream base: 16
]
