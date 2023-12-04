"
I am a cluster for CompiledMethod instances.

How CompiledMethod trailers are serialized can be established using methods in 'configurating' class-side protocol.
"
Class {
	#name : 'FLCompiledMethodCluster',
	#superclass : 'FLCompiledCodeCluster',
	#classInstVars : [
		'transformationForSerializing'
	],
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'cleanup' }
FLCompiledMethodCluster class >> cleanUp [
	self initialize
]

{ #category : 'class initialization' }
FLCompiledMethodCluster class >> initialize [ 
	self setTrailerWithoutSourceCode
]

{ #category : 'configuring' }
FLCompiledMethodCluster class >> setTrailerWithoutSourceCode [
	"Make compiled methods be serialized without source code.
	As of Pharo 12, source code is stored in the #source property
	and no longer the method trailer."

	self transformationForSerializing: [ :aCompiledMethod |
		aCompiledMethod copy clearSourcePointer ]
]

{ #category : 'accessing' }
FLCompiledMethodCluster class >> transformationForSerializing [ 
	"Answer a block closure that receives a CompiledMethod and returns another or the same CompiledMethod ready for being serialized."
	^ transformationForSerializing
]

{ #category : 'accessing' }
FLCompiledMethodCluster class >> transformationForSerializing: aBlockWithOneArgument [ 
	transformationForSerializing := aBlockWithOneArgument
]

{ #category : 'serialize/materialize' }
FLCompiledMethodCluster >> materializeInstanceWith: aDecoder [
	| compiledMethod header bytecodesPlusTrailerSize |
	header := self decodeHeaderWith: aDecoder.
	bytecodesPlusTrailerSize := aDecoder nextEncodedUint16.

	compiledMethod := CompiledMethod
		basicNew: bytecodesPlusTrailerSize
		header: header.

	compiledMethod initialPC
		to: compiledMethod size
		do: [ :index |
			compiledMethod
				at: index
				put: aDecoder nextEncodedByte ].
		
	^ compiledMethod
]

{ #category : 'serialize/materialize' }
FLCompiledMethodCluster >> prepareObjectsStep [
	objects do: [ :compiledMethod |
		compiledMethod fuelPrepare ]
]

{ #category : 'serialize/materialize' }
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
