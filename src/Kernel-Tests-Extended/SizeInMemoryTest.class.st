"
SUnit tests for the size of objects in memory
"
Class {
	#name : #SizeInMemoryTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Extended-Objects'
}

{ #category : #utilities }
SizeInMemoryTest >> align64Bits: size [
	size % 8 = 0
		ifTrue: [ ^ size ]
		ifFalse:[ ^ size + 8 - (size % 8) ].
]

{ #category : #utilities }
SizeInMemoryTest >> headerSize [
	" The header is 64 bits"
	^ 8
]

{ #category : #utilities }
SizeInMemoryTest >> paddedByteStringSize: numberOfChars [ 
	"64 bits for the header"
	| originalSize  |
	originalSize := self headerSize + (numberOfChars = 0 ifTrue:[1] ifFalse:[numberOfChars]).
		
	^ self align64Bits: originalSize.
]

{ #category : #tests }
SizeInMemoryTest >> testSizeInMemoryCompactClasses [
	self skip.
	"One word for the header + one word per instance variable"
	self assert: (Association key: 'aKey' value: 23) sizeInMemory equals: 12.

	"One word for the header + one word per instance variable"
	self assert: Rectangle new sizeInMemory equals: 12
]

{ #category : #tests }
SizeInMemoryTest >> testSizeInMemoryLargeInstances [
	"64 bits for the object header + a world per instance variable"

	self
		assert: Smalltalk allClasses sizeInMemory equals: (self align64Bits: self headerSize + (3 * Smalltalk wordSize));
		assert: Smalltalk allClasses asArray sizeInMemory
			equals: (self align64Bits: self headerSize + (Smalltalk allClasses asArray size * Smalltalk wordSize) + self headerSize)
]

{ #category : #tests }
SizeInMemoryTest >> testSizeInMemoryNormalClasses [
	"Two word for the header + one word per instance variable"

	self assert: Date today sizeInMemory equals: (self align64Bits: 2 * Smalltalk wordSize + self headerSize).

	"Two word for the header + one word per instance variable"
	self assert: TestCase new sizeInMemory equals: (self align64Bits: 2 * Smalltalk wordSize + self headerSize)
]

{ #category : #tests }
SizeInMemoryTest >> testSizeInMemoryOfByteObjects [
	"Byte objects should be padded to words"
	self assert: 'a' sizeInMemory equals: (self paddedByteStringSize: 1).
	self assert: 'abcd' sizeInMemory equals: (self paddedByteStringSize: 4).
	self assert: '' sizeInMemory equals: (self paddedByteStringSize: 0).
	self assert: 'abcde' sizeInMemory equals: (self paddedByteStringSize: 5).
	self assert: 'abcdefghi' sizeInMemory equals: (self paddedByteStringSize: 9).
]

{ #category : #tests }
SizeInMemoryTest >> testSizeInMemoryOfCharacters [
	self assert: 0 asCharacter sizeInMemory isZero.
	self assert: 255 asCharacter sizeInMemory isZero.
	self assert: 256 asCharacter sizeInMemory isZero.
	self assert: 16r10FFFF asCharacter sizeInMemory isZero
]

{ #category : #tests }
SizeInMemoryTest >> testSizeInMemorySmallIntegers [
	self assert: SmallInteger minVal sizeInMemory isZero.
	self assert: 0 sizeInMemory isZero.
	self assert: SmallInteger maxVal sizeInMemory isZero.
	self assert: (SmallInteger minVal - 1) sizeInMemory > 0.
	self assert: (SmallInteger maxVal + 1) sizeInMemory > 0
]
