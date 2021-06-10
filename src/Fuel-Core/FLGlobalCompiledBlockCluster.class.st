"
I clusterize CompiledBlocks that belong to CompiledMethods that are considered global (see FLGlobalCompiledMethodCluster).

Before Pharo 9, the bytecode of blocks was stored in the associated CompiledMethod. As of Pharo 9, CompiledBlocks can
exist independantly of an associated CompiledMethod and store their own bytecode. Hence, when a CompiledMethod is considered global, all blocks must be treated equivalently. Otherwise, blocks would be serialized and materialized as normal and evaluating such a block may answer a result different from the actually installed block if the corresponding block was changed in the installed method in the mean time.

See FLBlockClosureSerializationTest>>testBlockClosureChangeDifferentBytecodes and #testBlockClosureChangeSameBytecodes.
"
Class {
	#name : #FLGlobalCompiledBlockCluster,
	#superclass : #FLGlobalCompiledCodeCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #'serialize/materialize' }
FLGlobalCompiledBlockCluster >> materializeInstanceWith: aDecoder [
	| methodClass selector method literalIndex |
	methodClass := self materializeGlobalClassFrom: aDecoder.
	selector := aDecoder nextEncodedString asSymbol.
	literalIndex := aDecoder nextEncodedUint8.
	
	"It's ok if the method was changed or removed."
	method := methodClass  
		compiledMethodAt: selector
		ifAbsent: [ ^ nil ].
	^ literalIndex > method numLiterals
		ifTrue: [ nil ]
		ifFalse: [ method literalAt: literalIndex ]
]

{ #category : #'serialize/materialize' }
FLGlobalCompiledBlockCluster >> serializeInstance: aCompiledBlock with: anEncoder [
	"The method will be looked up on the existing class upon materialization
	and the block will be compiled as part of that method (possibly with a different
	set of instructions than this one but that's expected behavior).
	
	See #testBlockClosureChangeDifferendBytecodes and #testBlockClosureChangeSameBytecodes
	on FLBlockClosureSerializationTest."
	| method literalIndex |
	method := aCompiledBlock method.
	literalIndex := method literals indexOf: aCompiledBlock.
	
	self
		serializeGlobalClass: aCompiledBlock methodClass
		on: anEncoder.
	anEncoder
		encodeString: method selector;
		encodeUint8: literalIndex

]
