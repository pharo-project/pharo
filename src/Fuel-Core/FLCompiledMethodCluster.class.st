"
I am a cluster for CompiledMethod instances.

How CompiledMethod trailers are serialized can be established using methods in 'configurating' class-side protocol.
"
Class {
	#name : #FLCompiledMethodCluster,
	#superclass : #FLIteratingCluster,
	#classInstVars : [
		'transformationForSerializing'
	],
	#category : 'Fuel-Core-Clusters'
}

{ #category : #cleanup }
FLCompiledMethodCluster class >> cleanUp [
	self initialize 
]

{ #category : #analyzing }
FLCompiledMethodCluster class >> clusterBucketIn: aClusterization [

	^aClusterization primitivesBucket
]

{ #category : #'class initialization' }
FLCompiledMethodCluster class >> initialize [ 
	self setTrailerWithSourceCode
]

{ #category : #configuring }
FLCompiledMethodCluster class >> setTrailerWithNoChange [
	"Make compiled methods be serialized as they are."

	^ self transformationForSerializing: [:aCompiledMethod | aCompiledMethod ]
]

{ #category : #configuring }
FLCompiledMethodCluster class >> setTrailerWithNoSource [
	"Make compiled methods be serialized without source code."

	^ self transformationForSerializing: [:aCompiledMethod |
		aCompiledMethod copyWithTrailerBytes: CompiledMethodTrailer empty ]
]

{ #category : #configuring }
FLCompiledMethodCluster class >> setTrailerWithSourceCode [
	"Make compiled methods be serialized with embedded source code."

	^ self transformationForSerializing: [:aCompiledMethod | 
		aCompiledMethod copyWithTrailerBytes: 
			(CompiledMethodTrailer new sourceCode: aCompiledMethod sourceCode) ]
]

{ #category : #accessing }
FLCompiledMethodCluster class >> transformationForSerializing [ 
	"Answer a block closure that receives a CompiledMethod and returns another or the same CompiledMethod ready for being serialized."
	^ transformationForSerializing
]

{ #category : #accessing }
FLCompiledMethodCluster class >> transformationForSerializing: aBlockWithOneArgument [ 
	transformationForSerializing := aBlockWithOneArgument
]

{ #category : #'serialize/materialize' }
FLCompiledMethodCluster >> materializeInstanceWith: aDecoder [

	| compiledMethod header bytecodesPlusTrailerSize |

	header := aDecoder nextEncodedInt32.
	bytecodesPlusTrailerSize := aDecoder nextEncodedUint16.

	compiledMethod := CompiledMethod newMethod: bytecodesPlusTrailerSize header: header.

	compiledMethod initialPC to: compiledMethod size
		do: [ :index | compiledMethod at: index put: aDecoder nextEncodedByte ].
		
	^ compiledMethod
]

{ #category : #protected }
FLCompiledMethodCluster >> materializeLiteralsTo: compiledMethod with: aDecoder [

	1 to: compiledMethod numLiterals 
		do: [ :index | compiledMethod literalAt: index put: aDecoder nextEncodedReference ]

]

{ #category : #'serialize/materialize' }
FLCompiledMethodCluster >> materializeReferencesStepWith: aDecoder [

	objects do: [ :aCompiledMethod | self materializeLiteralsTo: aCompiledMethod with: aDecoder ]
]

{ #category : #analyzing }
FLCompiledMethodCluster >> referencesOf: aCompiledMethod do: aBlock [

	aCompiledMethod fuelPrepare.
	1 to: aCompiledMethod numLiterals
		do: [ :index | aBlock value: (aCompiledMethod literalAt: index) ]
]

{ #category : #'serialize/materialize' }
FLCompiledMethodCluster >> serializeInstance: aCompiledMethodToSerialize with: anEncoder [
	| header bytecodesPlusTrailerSize cmSize cmInitialPC aCompiledMethod |
	"In 64 bit VMs the header is actually 64 bits wide but only 32 bits
	contain information."
	aCompiledMethod := self class transformationForSerializing value: aCompiledMethodToSerialize.

	header := aCompiledMethod header.
	cmSize := aCompiledMethod size.
	cmInitialPC := aCompiledMethod initialPC.
	bytecodesPlusTrailerSize := cmSize - cmInitialPC + 1.

	anEncoder 
		encodeInt32: header; "Signed int as of Pharo 40"
		encodeUint16: bytecodesPlusTrailerSize.	

	cmInitialPC to: cmSize
		do: [ :index | anEncoder encodeByte: (aCompiledMethod at: index) ]
]

{ #category : #protected }
FLCompiledMethodCluster >> serializeLiteralsOf: aCompiledMethod with: anEncoder [

	aCompiledMethod fuelPrepare.
	1 to: aCompiledMethod numLiterals 
		do: [ :index | anEncoder encodeReferenceTo: (aCompiledMethod literalAt: index) ]
]

{ #category : #'serialize/materialize' }
FLCompiledMethodCluster >> serializeReferencesStepWith: anEncoder [

	objects do: [ :aCompiledMethod | self serializeLiteralsOf: aCompiledMethod with: anEncoder ]
]
