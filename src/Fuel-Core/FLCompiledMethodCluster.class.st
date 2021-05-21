"
I am a cluster for CompiledMethod instances.

How CompiledMethod trailers are serialized can be established using methods in 'configurating' class-side protocol.
"
Class {
	#name : #FLCompiledMethodCluster,
	#superclass : #FLCompiledCodeCluster,
	#classInstVars : [
		'transformationForSerializing'
	],
	#category : #'Fuel-Core-Clusters'
}

{ #category : #cleanup }
FLCompiledMethodCluster class >> cleanUp [
	self initialize 
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
	header := self decodeHeaderWith: aDecoder.
	bytecodesPlusTrailerSize := aDecoder nextEncodedUint16.

	compiledMethod := CompiledMethod
			newMethod: bytecodesPlusTrailerSize
			header: header.

	compiledMethod initialPC
		to: compiledMethod size
		do: [ :index |
			compiledMethod
				at: index
				put: aDecoder nextEncodedByte ].
		
	^ compiledMethod
]

{ #category : #'serialize/materialize' }
FLCompiledMethodCluster >> prepareObjectsStep [
	objects do: [ :compiledMethod |
		compiledMethod fuelPrepare ]
]

{ #category : #'serialize/materialize' }
FLCompiledMethodCluster >> serializeInstance: aCompiledMethodToSerialize with: anEncoder [
	| header bytecodesPlusTrailerSize cmSize cmInitialPC aCompiledMethod |
	aCompiledMethod := self class transformationForSerializing value: aCompiledMethodToSerialize.

	header := aCompiledMethod header.
	cmSize := aCompiledMethod size.
	cmInitialPC := aCompiledMethod initialPC.
	bytecodesPlusTrailerSize := cmSize - cmInitialPC + 1.

	self
		encodeHeader: header
		with: anEncoder.
	anEncoder encodeUint16: bytecodesPlusTrailerSize.	

	cmInitialPC
		to: cmSize
		do: [ :index |	anEncoder encodeByte: (aCompiledMethod at: index) ]
]
