"
I clusterize CompiledBlocks that belong to CompiledMethods that are considered global (see FLGlobalCompiledMethodCluster).

Before Pharo 9, the bytecode of blocks was stored in the associated CompiledMethod. As of Pharo 9, CompiledBlocks can
exist independantly of an associated CompiledMethod and store their own bytecode. Hence, when a CompiledMethod is considered global, all blocks must be treated equivalently. Otherwise, blocks would be serialized and materialized as normal and evaluating such a block may answer a result different from the actually installed block if the corresponding block was changed in the installed method in the mean time.

See FLBlockClosureSerializationTest>>testBlockClosureChangeDifferentBytecodes and #testBlockClosureChangeSameBytecodes.
"
Class {
	#name : 'FLGlobalCompiledBlockCluster',
	#superclass : 'FLGlobalCompiledCodeCluster',
	#category : 'Fuel-Core-Clusters-Global',
	#package : 'Fuel-Core',
	#tag : 'Clusters-Global'
}

{ #category : 'serialize/materialize' }
FLGlobalCompiledBlockCluster >> materializeInstanceWith: aDecoder [

	| methodClass selector method pathSize path theBlock |
	methodClass := self materializeGlobalClassFrom: aDecoder.
	selector := aDecoder nextEncodedString asSymbol.
	pathSize := aDecoder nextEncodedUint8.
	path := ByteArray new: pathSize.
	aDecoder nextEncodedBytesInto: path.

	"It's ok if the method was changed or removed."
	method := self findMethodWithSelector: selector in: methodClass.

	"Follow the saved path from the method to the compiled block"
	theBlock := method.
	path do: [ :index |
		theBlock := index = 0
			            ifTrue: [
			            theBlock compiledBlock "0 means it is in a Clean Block" ]
			            ifFalse: [ theBlock literalAt: index ].
		"If at some point we find another object that is not a closure, we skip.
		Probably the code changed, we cannot get the block from old method."
		(theBlock isBlock or: [ theBlock isCompiledBlock ]) ifFalse: [ 
			"Do not throw an exception here, constant blocks are still possible to
				metarialize as normal object."
			^ nil ] ].
	^ theBlock
]

{ #category : 'serialize/materialize' }
FLGlobalCompiledBlockCluster >> serializeInstance: aCompiledBlock with: anEncoder [
	"The method will be looked up on the existing class upon materialization
	and the block:
	  - will be compiled as part of that method (possibly with a different set of instructions than this one but that's expected behavior).
	  - or the method will have a Clean Block with the compiled block inside (that's why we save all the path from the method)
	See #testBlockClosureChangeDifferendBytecodes and #testBlockClosureChangeSameBytecodes on FLBlockClosureSerializationTest."

	| method path |
	method := aCompiledBlock method.
	self serializeGlobalClass: aCompiledBlock methodClass on: anEncoder.
	path := aCompiledBlock pathOfLiteralIndexes.
	anEncoder
		encodeString: method selector;
		encodeUint8: path size;
		encodeBytes: path
]
