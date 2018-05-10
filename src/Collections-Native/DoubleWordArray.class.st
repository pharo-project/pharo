"
DoubleWordArrays store 64-bit unsigned Integer values.
"
Class {
	#name : #DoubleWordArray,
	#superclass : #ArrayedCollection,
	#type : #words,
	#category : #'Collections-Native-Base'
}

{ #category : #private }
DoubleWordArray >> atAllPut: value [
	"Fill the receiver with the given value"

	<primitive: 145>
	super atAllPut: value
]

{ #category : #accessing }
DoubleWordArray >> byteSize [
	^self size * 8
]

{ #category : #accessing }
DoubleWordArray >> bytesPerElement [
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 8
]

{ #category : #accessing }
DoubleWordArray >> defaultElement [
	"Return the default element of the receiver"
	^0
]

{ #category : #private }
DoubleWordArray >> replaceFrom: start to: stop with: replacement startingAt: repStart [ 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]
