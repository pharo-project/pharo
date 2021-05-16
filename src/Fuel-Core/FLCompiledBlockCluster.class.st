"
I am a cluster for CompiledBlock instances.

Since Pharo 9 blocks can exist independently of an associated compiled method and, hence, require special treatment.
"
Class {
	#name : #FLCompiledBlockCluster,
	#superclass : #FLCompiledCodeCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLCompiledBlockCluster >> materializeInstanceWith: aDecoder [
	| instance size header |
	header := self decodeHeaderWith: aDecoder.
	size := aDecoder nextEncodedUint32.
	
	instance := CompiledBlock
		newMethod: size
		header: header.
	instance initialPC
		to: instance endPC
		do: [ :index |
			instance
				at: index
				put: aDecoder nextEncodedByte ].
	^ instance
]

{ #category : #'serialize/materialize' }
FLCompiledBlockCluster >> serializeInstance: aCompiledBlock with: anEncoder [
	| header bytecode |
	bytecode := aCompiledBlock bytecode.
	header := aCompiledBlock header.

	self
		encodeHeader: header
		with: anEncoder.
	anEncoder
		encodeUint32: bytecode basicSize;
		encodeBytes: bytecode
]
