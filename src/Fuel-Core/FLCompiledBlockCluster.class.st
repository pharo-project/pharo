"
I am a cluster for CompiledBlock instances.

Since Pharo 9 blocks can exist independently of an associated compiled method and, hence, require special treatment.
"
Class {
	#name : 'FLCompiledBlockCluster',
	#superclass : 'FLCompiledCodeCluster',
	#category : 'Fuel-Core-Clusters',
	#package : 'Fuel-Core',
	#tag : 'Clusters'
}

{ #category : 'serialize/materialize' }
FLCompiledBlockCluster >> afterMaterializationStepWith: aDecoder [
	"Warn users that the method of a fully serialized block has changed.
	The block can be evaluated but the current form of the method might
	no longer include the closure or the closure implementation might have
	changed.
	NOTE: this method is intentionally not implemented as #fuelAfterMaterialization
		on the class because that method could potentially also be used by a global
		cluster."
	aDecoder configuration isMethodChangedWarningDisabled ifTrue: [
		^ self ].

	objects
		select: [ :block | self shouldWarnThatMethodChangedFor: block ]
		thenDo: [ :block |
			FLMethodChanged
				signalWith: block methodClass name
				and: block method selector ]
]

{ #category : 'serialize/materialize' }
FLCompiledBlockCluster >> materializeInstanceWith: aDecoder [
	| instance size header initialPC |
	header := self decodeHeaderWith: aDecoder.
	size := aDecoder nextEncodedUint32.

	instance := CompiledBlock
		newMethod: size
		header: header.
	initialPC := instance initialPC.
	initialPC
		to: initialPC + size - 1
		do: [ :index |
			instance
				at: index
				put: aDecoder nextEncodedByte ].
	^ instance
]

{ #category : 'serialize/materialize' }
FLCompiledBlockCluster >> serializeInstance: aCompiledBlock with: anEncoder [
	| header bytecode |
	bytecode := aCompiledBlock bytecodes.
	header := aCompiledBlock header.

	self
		encodeHeader: header
		with: anEncoder.
	anEncoder
		encodeUint32: bytecode basicSize;
		encodeBytes: bytecode
]

{ #category : 'serialize/materialize' }
FLCompiledBlockCluster >> shouldWarnThatMethodChangedFor: aBlock [
	| method currentMethod |
	method := aBlock method.
	(method methodClass includesSelector: method selector) ifFalse: [ ^ true ].
	
	currentMethod := method methodClass compiledMethodAt: method selector.
	^ currentMethod bytecodesHash ~= method bytecodesHash or: [
		currentMethod literals ~= method literals ]
]
