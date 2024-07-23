Extension { #name : 'Context' }

{ #category : '*Reflectivity' }
Context >> computePCForNewMethod: aCompiledMethod [

	| offset currentSymbolicBytecodes newSymbolicBytecodes currentSymbolicBytecodeIndex newSymbolicBytecodeIndex currentBytecode newBytecode |
	"This method makes the assumption that the new method only adds bytecodes to the old one, and does not remove any bytecode of the old method"
	offset := self pc - self startpc.
	newSymbolicBytecodes := aCompiledMethod symbolicBytecodes.
	currentSymbolicBytecodes := self compiledCode symbolicBytecodes
		                            select: [ :bytecode |
		                            bytecode offset <= self pc ].
	currentSymbolicBytecodeIndex := 1.
	currentBytecode := currentSymbolicBytecodes at:
		                   currentSymbolicBytecodeIndex.
	newSymbolicBytecodeIndex := 1.
	newBytecode := newSymbolicBytecodes at: newSymbolicBytecodeIndex.
	[ "While all bytecodes from current method have not been found in new method:"
	currentSymbolicBytecodeIndex < currentSymbolicBytecodes size "=" ]
		whileTrue: [ "If the two bytecodes are the same, then no offset need to be added, else it means a bytecode has been added in the new method and the offset should be increased by the size of this bytecode:"
			currentBytecode := currentSymbolicBytecodes at:
				                   currentSymbolicBytecodeIndex.
			newBytecode := newSymbolicBytecodes at: newSymbolicBytecodeIndex.
			(self
				 currentBytecode: currentBytecode
				 equalsToNewBytecode: newBytecode
				 withCurrents: self compiledCode symbolicBytecodes
				 withNews: aCompiledMethod symbolicBytecodes)
				ifTrue: [
				currentSymbolicBytecodeIndex := currentSymbolicBytecodeIndex + 1 ]
				ifFalse: [ offset := offset + newBytecode bytes size ].
			newSymbolicBytecodeIndex := newSymbolicBytecodeIndex + 1 ].
	^ aCompiledMethod initialPC + offset
]

{ #category : '*Reflectivity' }
Context >> currentBytecode: currentBytecode equalsToNewBytecode: newBytecode withCurrents: currentSymbolicBytecodes withNews: newSymbolicBytecodes [

	| result |
	result := currentBytecode description = newBytecode description.
	^ result
		  ifTrue: [ true ]
		  ifFalse: [
			  ((currentBytecode description beginsWith: 'jump') and: [
				   newBytecode description beginsWith: 'jump' ])
				  ifFalse: [ false ]
				  ifTrue: [
					  | currentSplitDescription newSplitDescription |
					  currentSplitDescription := $: split:
						                             currentBytecode description.
					  newSplitDescription := $: split: newBytecode description.
					  (currentSplitDescription size = 2 and: [
						   newSplitDescription size = 2 and: [
							   currentSplitDescription first = newSplitDescription first ] ])
						  ifFalse: [ false ]
						  ifTrue: [
							  | currentJumpPc newJumpPc currentJumpBytecode newJumpBytecode |
							  currentJumpPc := (currentSplitDescription at: 2) asInteger.
							  newJumpPc := (newSplitDescription at: 2) asInteger.
							  currentJumpBytecode := currentSymbolicBytecodes
								                         detect: [ :csb |
								                         csb offset = currentJumpPc ]
								                         ifNone: [ ^ false ].
							  newJumpBytecode := newSymbolicBytecodes
								                     detect: [ :nsb | nsb offset = newJumpPc ]
								                     ifNone: [ ^ false ].
							  self
								  currentBytecode: currentJumpBytecode
								  equalsToNewBytecode: newJumpBytecode
								  withCurrents: currentSymbolicBytecodes
								  withNews: newSymbolicBytecodes ] ] ]
]

{ #category : '*Reflectivity' }
Context >> method: aCompiledMethod [

	| compiledCode |
	compiledCode := method ast == aCompiledMethod ast
		                ifTrue: [ aCompiledMethod ]
		                ifFalse: [
			                aCompiledMethod allBlocks detect: [ :compiledBlock |
				                compiledBlock ast = method ast ] ].

	1 to: self stackPtr do: [ :index |
		| obj |
		obj := self at: index.
		obj isClosure ifTrue: [
			obj compiledBlock:
				(aCompiledMethod allBlocks detect: [ :compiledBlock |
					 compiledBlock ast = obj compiledBlock ast ]) ] ].

	pc := self computePCForNewMethod: compiledCode.
	method := compiledCode.
	closureOrNil ifNotNil: [
		closureOrNil compiledBlock: compiledCode ]
]
