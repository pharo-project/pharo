Class {
	#name : #TFUFFIDerivedTypeSizeTest,
	#superclass : #TFUFFITestCase,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #helpers }
TFUFFIDerivedTypeSizeTest >> assertType: typename equals: aSize [

	self assert: (self perform: ('sizeof_', typename) asSymbol) equals: aSize
]

{ #category : #helpers }
TFUFFIDerivedTypeSizeTest >> sizeof_size_t [
	self ffiCall: #(size_t sizeof_size_t())
]

{ #category : #helpers }
TFUFFIDerivedTypeSizeTest >> testSizeSizeT [

	self assertType: 'size_t' equals: Smalltalk wordSize
]
