"
SUnit tests for ByteArray, DoubleByteArray, WordArray and DoubleWordArray.
"
Class {
	#name : #NativeArrayTest,
	#superclass : #TestCase,
	#category : #'Collections-Tests-Arrayed'
}

{ #category : #utilities }
NativeArrayTest >> alignmentIndexesDo: aBlock [
	| max |
	max := self wordSize.
	1 to: max do: [ :i |
		1 to: max do: [ :j |
			1 to: max do: [ :k |
				aBlock value: i value: j value: k ] ] ] 
]

{ #category : #utilities }
NativeArrayTest >> alignments [
	"Arrays aligned differently on 8 bytes boundary (when turned into byteArrays)"
	^ #( 
	(10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80) "aligned"
	(10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10) "+ 1.."
	(10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20)
	(10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30)
	(10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40)
	(10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50)
	(10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60)
	(10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60 70 80 10 20 30 40 50 60 70) "+ 7 !"
	)
]

{ #category : #utilities }
NativeArrayTest >> guineaPigClasses [
	^ { ByteArray . DoubleByteArray . WordArray . DoubleWordArray}
]

{ #category : #utilities }
NativeArrayTest >> guineaPigHerd [
	"Different alignment, different classes"
	^ self guineaPigClasses flatCollect: [ :cls |
		self alignments collect: [ :align |
			cls newFrom: align ] ]
]

{ #category : #tests }
NativeArrayTest >> testAtAllPut [
	"Makes sure the primitive works correctly with different data structures"
	self guineaPigHerd do: [ :guineaPig |
		guineaPig atAllPut: 42.
		1 to: (self wordSize min: guineaPig size) do: [ :i |
			self assert: (guineaPig at: i) equals: 42 ].
		(guineaPig size - self wordSize max: 1) to: guineaPig size do: [ :i |
			self assert: (guineaPig at: i) equals: 42 ] ]
]

{ #category : #tests }
NativeArrayTest >> testAtAllPutFail [
	"Makes sure the primitive works correctly with different data structures"
	self guineaPigHerd do: [ :guineaPig |
		self should: [guineaPig atAllPut: -1] raise: Error ]
]

{ #category : #tests }
NativeArrayTest >> testAtPutBiggerNumberThrowsException [
	"Make sure native arrays can contain elements of their slot size"

	self guineaPigClasses do: [ :guineaPig | | nativeArray smallerNumber bytesToShift |
		nativeArray := guineaPig new: 1.
		bytesToShift := guineaPig classLayout bytesPerSlot * 8 "bits per byte".
		smallerNumber := 1 << bytesToShift.

		self
			should: [ nativeArray at: 1 put: smallerNumber ]
			raise: Error ]
]

{ #category : #tests }
NativeArrayTest >> testAtPutExactSizeNumber [
	"Make sure native arrays can contain elements of their slot size"

	self guineaPigClasses do: [ :guineaPig | | nativeArray smallerNumber bytesToShift |
		nativeArray := guineaPig new: 1.
		bytesToShift := guineaPig classLayout bytesPerSlot * 8 "bits per byte" - 1.
		smallerNumber := 1 << bytesToShift.

		nativeArray at: 1 put: smallerNumber.
		self assert: (nativeArray at: 1) equals: smallerNumber
	]
]

{ #category : #tests }
NativeArrayTest >> testReplaceFromToWithStartingAt [
	"Makes sure the primitive works correctly with different data structures"
	self guineaPigHerd do: [ :guineaPig |
		| copy |
		copy := guineaPig copy.
		1 to: copy size do: [ :l | copy at: l put: l ]. 
		self alignmentIndexesDo: [ :i :j :k |
					guineaPig replaceFrom: i to: j + 8 with: copy startingAt: k.
					self assert: (guineaPig at: i) equals: (copy at: k).
					self assert: (guineaPig at: j + 8) equals: (copy at: j + 8 - i + k) ] ]
]

{ #category : #utilities }
NativeArrayTest >> wordSize [
	^ Smalltalk wordSize
]
