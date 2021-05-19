"
I am the abstract cluster for CompiledCode classes. I implement shared serialization and materialization behavior.

See the class comment of CompiledCode for information on the format.
"
Class {
	#name : #FLCompiledCodeCluster,
	#superclass : #FLIteratingCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLCompiledCodeCluster class >> clusterBucketIn: aClusterization [

	^aClusterization primitivesBucket
]

{ #category : #protected }
FLCompiledCodeCluster >> decodeHeaderWith: aDecoder [
	"Read the sign bit and remove it from the integer since SmallInteger only
	uses 31 bits.
	Finally, set the sign bit properly, respecting word size.
	See #encodeHeader:with:"
	| header signBit |
	header := aDecoder nextEncodedInt32.
	signBit := header bitShift: -31.
	^ signBit = 1
		ifTrue: [ (header bitAnd: 16r7FFFFFFF) + SmallInteger minVal ]
		ifFalse: [ header ]

]

{ #category : #protected }
FLCompiledCodeCluster >> encodeHeader: aSmallInteger with: anEncoder [
	"Reduce storage by storing the sign bit as the top bit of the 32 bit integer.
	We know that bit 32 is unused because SmallInteger uses 31 bits (on 32-bits systems).
	This operation is redundant on 32-bits systems but necessary on 64-bits systems since
	the sign bit will be bit 63, not 31."
	anEncoder encodeInt32: (aSmallInteger < 0
		ifTrue: [ aSmallInteger bitOr: 16r80000000 ]
		ifFalse: [ aSmallInteger ])
]

{ #category : #protected }
FLCompiledCodeCluster >> materializeLiteralsTo: compiledCode with: aDecoder [
	1
		to: compiledCode numLiterals 
		do: [ :index |
			compiledCode
				literalAt: index
				put: aDecoder nextEncodedReference ]
]

{ #category : #'serialize/materialize' }
FLCompiledCodeCluster >> materializeReferencesStepWith: aDecoder [
	objects do: [ :compiledCode |
		self
			materializeLiteralsTo: compiledCode
			with: aDecoder ]
]

{ #category : #analyzing }
FLCompiledCodeCluster >> referencesOf: compiledCode do: aBlock [
	1
		to: compiledCode numLiterals
		do: [ :index | aBlock value: (compiledCode literalAt: index) ]
]

{ #category : #protected }
FLCompiledCodeCluster >> serializeLiteralsOf: aCompiledBlock with: anEncoder [
	1
		to: aCompiledBlock numLiterals 
		do: [ :index |
			anEncoder encodeReferenceTo: (aCompiledBlock literalAt: index) ]
]

{ #category : #'serialize/materialize' }
FLCompiledCodeCluster >> serializeReferencesStepWith: anEncoder [
	objects do: [ :compiledCode |
		self
			serializeLiteralsOf: compiledCode
			with: anEncoder ]
]
