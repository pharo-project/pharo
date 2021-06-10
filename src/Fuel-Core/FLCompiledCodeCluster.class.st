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
	| signBit header |
	signBit := aDecoder nextEncodedByte.
	header := aDecoder nextEncodedInt32.

	^ signBit = 1
		ifTrue: [ header + SmallInteger minVal ]
		ifFalse: [ header ]

]

{ #category : #protected }
FLCompiledCodeCluster >> encodeHeader: aSmallInteger with: anEncoder [
	"Store the sign bit separately. We can't encoded it within the 32 bit integer because the
	high bit will be truncated.
	See #encodeInt32:."
	anEncoder
		encodeByte: (aSmallInteger < 0
			ifTrue: [ 1 ]
			ifFalse: [ 0 ]);
		encodeInt32: aSmallInteger
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
