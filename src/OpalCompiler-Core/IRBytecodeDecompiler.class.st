"
I interpret bytecode instructions, sending the appropriate instruction messages to my IRBuilder, resulting in an IRMethod.
"
Class {
	#name : #IRBytecodeDecompiler,
	#superclass : #InstructionClient,
	#instVars : [
		'instructionStream',
		'irBuilder',
		'newTempVector',
		'scopeStack'
	],
	#category : #'OpalCompiler-Core-Bytecode'
}

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> blockReturnTop [
	self popScope.
	irBuilder blockReturnTop.
]

{ #category : #private }
IRBytecodeDecompiler >> checkIfJumpTarget [
	irBuilder testJumpAheadTarget: instructionStream pc
]

{ #category : #'public access' }
IRBytecodeDecompiler >> decompile: aCompiledMethod [
	instructionStream := InstructionStream on: aCompiledMethod.
	irBuilder := IRReconstructor new.
	scopeStack := Stack new.
	self pushScope: #() numArgs: aCompiledMethod numArgs.
	irBuilder irPrimitive: aCompiledMethod irPrimitive.
	
	aCompiledMethod hasProperties ifTrue: [irBuilder properties: aCompiledMethod properties copy].
	irBuilder numArgs: aCompiledMethod numArgs.
	irBuilder addTemps: self scope args.
	aCompiledMethod isQuick
		ifTrue: [self quickMethod]
		ifFalse: [self interpret].
	self popScope.
	"just add all literals of the compiledMethod as additional literals.
	 duplicates will be filtered out, but we keep the optimized ones"
	irBuilder additionalLiterals: aCompiledMethod literals.
	"when compiling methods of context classes, force long form for iVar access by getting the correct context"
	irBuilder compilationContext: aCompiledMethod methodClass compiler compilationContext.
	^ irBuilder ir.
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> doDup [
	irBuilder pushDup
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> doPop [
	irBuilder popTop
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> interpret [
	| endPC |
	endPC := instructionStream method endPC.
	[instructionStream pc > endPC ] 
		whileFalse: [
			self checkIfJumpTarget.
			irBuilder mapToByteIndex: instructionStream pc.
			instructionStream interpretNextInstructionFor: self.]
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> jump: dist [
	| index seq instr newSeq seqs |
	index := instructionStream pc + dist.

	dist >= 0 ifTrue: [
		"jump forward"
		^ irBuilder jumpAheadTo: index ].
	"jump to the jump instuction itself"
	dist >= -2 ifTrue: 
		[	irBuilder jumpBackTarget: index.
			irBuilder jumpBackTo: index.
		^self].
	"jump backward"
	seqs := irBuilder ir allSequences.
	seq := seqs findLast: [:s | s notEmpty and: [s first bytecodeIndex <= index]].
	seq := seqs at: seq.
	seq first bytecodeIndex = index
		ifTrue: [ newSeq := seq ]
		ifFalse: [
			instr := seq detect: [:i | (seq after: i) bytecodeIndex = index].
			newSeq := seq splitAfter: instr ].
	irBuilder addJumpBackTarget: index to: newSeq.
	"if we have split the currentSequence of the irBuilder, make sure to set it
	to the newSeq"
	irBuilder currentSequence = seq ifTrue: [irBuilder currentSequence: newSeq].
	irBuilder jumpBackTo: index.


]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> jump: dist if: bool [
	| index |
	index := instructionStream pc + dist .

	dist >= 0 ifTrue: [
		"conditional jump forward"
		^ irBuilder jumpAheadTo: index if: bool ].

	self error: 'Should not do this'
]

{ #category : #private }
IRBytecodeDecompiler >> methodClass [
	^ instructionStream  method methodClass
]

{ #category : #'quick methods' }
IRBytecodeDecompiler >> methodReturnConstant: value [
	self pushConstant: value.
	self methodReturnTop.
]

{ #category : #'quick methods' }
IRBytecodeDecompiler >> methodReturnReceiver [
	self pushReceiver.
	self methodReturnTop.
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> methodReturnTop [
	irBuilder isLastClosureInstruction
		ifTrue: [
			self popScope.
			irBuilder fixPushNilsForTemps.
			irBuilder returnTop.
			irBuilder popScope. ]
		ifFalse: [ irBuilder returnTop ]
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> popIntoLiteralVariable: offset [
	self storeIntoLiteralVariable: offset.
	self doPop.
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> popIntoReceiverVariable: offset [
	self storeIntoReceiverVariable: offset.
	self doPop.
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	self storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex.
	self doPop.
	
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> popIntoTemporaryVariable: offset [
	newTempVector
		ifNil: [
			self storeIntoTemporaryVariable: offset.
			self doPop. ]
		ifNotNil: [
			self scope newTempVector: newTempVector at: offset.
			" Keep offset for remapping in popScope "
			newTempVector index: offset.
			irBuilder createTempVectorNamed: newTempVector withVars: newTempVector.
			newTempVector := nil ]
]

{ #category : #scope }
IRBytecodeDecompiler >> popScope [
	| scope tempIndex |
	scope := self scope.
	
	irBuilder addTemps: scope temps.
	
	" Remap your own temp vectors "
	scope ownTempVectors do: [ :tempVector |
		irBuilder
			remapTemp: (scope -> tempVector index)
			toRemote: tempVector ].
	
	" Remap the copied values "
	tempIndex := scope numArgs.
	scope copiedValues do: [ :copiedValue |
		irBuilder
			remapTemp: (scope -> tempIndex)
			toRemote: copiedValue.
		tempIndex := tempIndex + 1. ].
	
	^ scopeStack pop
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushActiveContext [
	irBuilder pushThisContext
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize [ 
	| closureEnd copiedValues |
	copiedValues := irBuilder removeLast: numCopied.
	self pushScope: copiedValues numArgs: numArgs.
	closureEnd := instructionStream pc + blockSize.
	
	irBuilder
		pushClosureCopyCopiedValues: copiedValues
		args: self scope args
		jumpTo: closureEnd.
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushConsArrayWithElements: numElements [ 
	irBuilder pushConsArray: numElements

]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushConstant: value [
	irBuilder pushLiteral: value
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushLiteralVariable: assoc [
	irBuilder pushLiteralVariable: assoc.
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushNewArrayOfSize: size [
	newTempVector := IRRemoteArray new size: size
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushReceiver [
	irBuilder pushReceiver
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushReceiverVariable: offset [
	irBuilder pushInstVar: offset + 1.

]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushRemoteTemp: remoteIndex inVectorAt: tempIndex [ 
	|  remoteArray remoteTempName |
	remoteArray := self scope tempAt: tempIndex.
	remoteTempName := self scope tempAt: remoteIndex inRemote: remoteArray.
	irBuilder pushRemoteTemp: remoteTempName inVector: remoteArray.
]

{ #category : #scope }
IRBytecodeDecompiler >> pushScope: copiedValues numArgs: numArgs [
	|scope |
	scope := IRBytecodeScope new numArgs: numArgs.
	scopeStack push: scope.
	scope copiedValues: copiedValues.
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> pushTemporaryVariable: offset [
	irBuilder pushTemp: (self scope tempAt: offset)
]

{ #category : #'quick methods' }
IRBytecodeDecompiler >> quickMethod [

	instructionStream method primitive = 256 ifTrue: [
		^ self methodReturnReceiver
	]. 
	instructionStream method isReturnSpecial ifTrue: [
		^ self methodReturnConstant: (instructionStream method encoderClass quickPrimSpecialConstants at: instructionStream method primitive - 256)
	].
	instructionStream method isReturnField ifTrue: [
		self pushReceiverVariable: instructionStream method returnField.
		^ self methodReturnTop
	].
	self error: 'quick method inconsistency'
]

{ #category : #scope }
IRBytecodeDecompiler >> scope [
	^ scopeStack top
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> send: selector super: superFlag numArgs: numArgs [
	superFlag
		ifTrue: [irBuilder send: selector toSuperOf: self methodClass]
		ifFalse: [irBuilder send: selector]
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> storeIntoLiteralVariable: value [
	irBuilder storeIntoLiteralVariable: value
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> storeIntoReceiverVariable: offset [
	irBuilder storeInstVar: offset + 1.

]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> storeIntoRemoteTemp: remoteIndex inVectorAt: tempIndex [ 
	|  remoteArray remoteTempName |
	remoteArray := self scope tempAt: tempIndex.
	remoteTempName := self scope tempAt: remoteIndex inRemote: remoteArray.	
	irBuilder storeRemoteTemp: remoteTempName inVector: remoteArray.
	
]

{ #category : #'instruction decoding' }
IRBytecodeDecompiler >> storeIntoTemporaryVariable: offset [
	irBuilder storeTemp: (self scope tempAt: offset)
]
