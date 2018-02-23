"
DoubleByteArrays store 16-bit unsigned Integer values.
"
Class {
	#name : #DoubleByteArray,
	#superclass : #ArrayedCollection,
	#type : #words,
	#category : #'Collections-Native'
}

{ #category : #private }
DoubleByteArray >> atAllPut: value [
	"Fill the receiver with the given value"

	<primitive: 145>
	super atAllPut: value
]

{ #category : #accessing }
DoubleByteArray >> byteSize [
	^self size * 2
]

{ #category : #accessing }
DoubleByteArray >> bytesPerElement [
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 2
]

{ #category : #accessing }
DoubleByteArray >> defaultElement [
	"Return the default element of the receiver"
	^0
]

{ #category : #private }
DoubleByteArray >> replaceFrom: start to: stop with: replacement startingAt: repStart [ 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]
