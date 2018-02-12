"
SUnit tests for ByteArray
"
Class {
	#name : #ByteArrayTest,
	#superclass : #TestCase,
	#category : #'Collections-Tests-Arrayed'
}

{ #category : #tests }
ByteArrayTest >> testBooleanAt [
	"Checking the result of ByteArray>>#booleanAt on a few instances of ByteArray. It should return false on 0-valued bytes and true on all other bytes (according to the integer to boolean C convention)."

	self deny: (#[0] booleanAt: 1).
	self assert: (#[1] booleanAt: 1).
	self assert: (#[255 2 3] booleanAt: 1).
	self deny: (#[0 25] booleanAt: 1).
	self assert: (#[255] booleanAt: 1).
]

{ #category : #tests }
ByteArrayTest >> testFallbackReplaceFromToWithForString [
	| testString byteArray stringByteSize |
	testString := 'Test string'.
	stringByteSize := 'Test string' byteSize.
	byteArray := ByteArray new: stringByteSize.
	
	self
		shouldnt: [
			byteArray
				replaceFrom: 1
				to: stringByteSize
				with: testString
				startingAt: 1 ]
		raise: Exception
		description: 'Primitive 105 should be optional for ByteArray'

]

{ #category : #tests }
ByteArrayTest >> testFallbackReplaceFromToWithForWideString [
	| testString byteArray stringByteSize |
	testString := 'Test string' asWideString.
	stringByteSize := 'Test string' byteSize.
	byteArray := ByteArray new: stringByteSize.
	
	self
		shouldnt: [
			byteArray
				replaceFrom: 1
				to: stringByteSize
				with: testString
				startingAt: 1 ]
		raise: Exception
		description: 'Primitive 105 should be optional for ByteArray'

]

{ #category : #tests }
ByteArrayTest >> testFourthByteArraysReturnTheCorrectValues [
	
	self assert: (#[16r3F 16r80 0 0]  floatAt:1 bigEndian: true) equals: 1.0.
	self assert: (#[16rC0 0 0 0]  floatAt:1 bigEndian: true) equals: -2.0.


]

{ #category : #tests }
ByteArrayTest >> testHex [	
	
	self assert: #[122 43 213 7] hex = '7a2bd507'.
	self assert: #[151 193 242 221 249 32 153 72 179 41 49 154 48 193 99 134] hex = '97c1f2ddf9209948b329319a30c16386'.
					
	self assert: (ByteArray readHexFrom: '7A2BD507') = #[122 43 213 7].
	self assert: (ByteArray readHexFrom: '7a2bd507') = #[122 43 213 7].
	
]

{ #category : #tests }
ByteArrayTest >> testIsLiteral [
	self assert: #[122 43 213 7] isLiteral description: 'ByteArray instances are literal'.
	self deny: thisContext method isLiteral description: 'ByteArray sub instances are not literal'
]

{ #category : #tests }
ByteArrayTest >> testindexOfStartingAt [
	|array|
	array := #[1 2 3 4 5 6 7 8 9 10 11 1 2 3 4 5 6 7 8 0 0].
	self assert: (array indexOf: 1 startingAt: 10) equals: 12.
	self assert: (array indexOf: 17 startingAt: 10) equals: 0.


]
