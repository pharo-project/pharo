"
SUnit tests for ByteArray, DoubleByteArray, WordArray and DoubleWordArray.
"
Class {
	#name : #NativeArrayTest,
	#superclass : #TestCase,
	#category : #'Collections-Tests-Arrayed'
}

{ #category : #utils }
NativeArrayTest >> alignmentIndexesDo: aBlock [
	| max |
	max := self wordSize.
	1 to: max do: [ :i |
		1 to: max do: [ :j |
			1 to: max do: [ :k |
				aBlock value: i value: j value: k ] ] ] 
]

{ #category : #utils }
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

{ #category : #utils }
NativeArrayTest >> guineaPigClasses [
	^ { ByteArray . DoubleByteArray . WordArray . DoubleWordArray}
]

{ #category : #utils }
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
			self assert: (guineaPig at: i) = 42 ].
		(guineaPig size - self wordSize max: 1) to: guineaPig size do: [ :i |
			self assert: (guineaPig at: i) = 42 ] ]
]

{ #category : #tests }
NativeArrayTest >> testAtAllPutFail [
	"Makes sure the primitive works correctly with different data structures"
	self guineaPigHerd do: [ :guineaPig |
		self should: [guineaPig atAllPut: -1] raise: Error ].
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
					self assert: (guineaPig at: i) = (copy at: k).
					self assert: (guineaPig at: j + 8) = (copy at: j + 8 - i + k) ] ]
]

{ #category : #utils }
NativeArrayTest >> wordSize [
	^ Smalltalk wordSize
]
