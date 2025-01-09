"
I interpret bytecode instructions, sending the appropriate instruction messages to my IRBuilder, resulting in an IRMethod.

My entry point is `CompiledMethod>>#decompileIR`
"
Class {
	#name : 'FBIRBytecodeDecompiler',
	#superclass : 'InstructionClient',
	#instVars : [
		'instructionStream',
		'irBuilder',
		'newTempVector',
		'scopeStack'
	],
	#category : 'Flashback-Decompiler-Bytecode',
	#package : 'Flashback-Decompiler',
	#tag : 'Bytecode'
}

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> blockReturnTop [
	self popScope.
	irBuilder blockReturnTop
]

{ #category : 'private' }
FBIRBytecodeDecompiler >> checkIfJumpTarget [
	irBuilder testJumpAheadTarget: instructionStream pc
]

{ #category : 'public access' }
FBIRBytecodeDecompiler >> decompile: aCompiledMethod [
	instructionStream := InstructionStream on: aCompiledMethod.
	irBuilder := OCIRReconstructorBuilder new.
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
	^ irBuilder ir
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> doDup [
	irBuilder pushDup
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> doPop [
	irBuilder popTop
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> interpret [
	| endPC |
	endPC := instructionStream method endPC.
	[instructionStream pc > endPC ]
		whileFalse: [
			self checkIfJumpTarget.
			irBuilder mapToByteIndex: instructionStream pc.
			instructionStream interpretNextInstructionFor: self.]
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> jump: dist [

	| index seq instr newSeq seqs |
	index := instructionStream pc + dist.

	dist >= 0 ifTrue: [ "jump forward" ^ irBuilder jumpAheadTo: index ].
	"jump to the jump instuction itself"
	dist >= -2 ifTrue: [
		irBuilder jumpBackTarget: index.
		irBuilder jumpBackTo: index.
		^ self ].
	"jump backward"
	seqs := irBuilder ir allSequences.
	seq := seqs findLast: [ :s |
		       s isNotEmpty and: [ s first bytecodeIndex <= index ] ].
	seq := seqs at: seq.
	seq first bytecodeIndex = index
		ifTrue: [ newSeq := seq ]
		ifFalse: [
			instr := seq detect: [ :i | (seq after: i) bytecodeIndex = index ].
			newSeq := seq splitAfter: instr ].
	irBuilder addJumpBackTarget: index to: newSeq.
	"if we have split the currentSequence of the irBuilder, make sure to set it
	to the newSeq"
	irBuilder currentSequence = seq ifTrue: [
		irBuilder currentSequence: newSeq ].
	irBuilder jumpBackTo: index
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> jump: dist if: bool [
	| index |
	index := instructionStream pc + dist .

	dist >= 0 ifTrue: [
		"conditional jump forward"
		^ irBuilder jumpAheadTo: index if: bool ].

	self error: 'Should not do this'
]

{ #category : 'private' }
FBIRBytecodeDecompiler >> methodClass [
	^ instructionStream  method methodClass
]

{ #category : 'quick methods' }
FBIRBytecodeDecompiler >> methodReturnConstant: value [
	self pushConstant: value.
	self methodReturnTop
]

{ #category : 'quick methods' }
FBIRBytecodeDecompiler >> methodReturnReceiver [
	self pushReceiver.
	self methodReturnTop
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> methodReturnTop [
	irBuilder isLastClosureInstruction
		ifTrue: [
			self popScope.
			irBuilder fixPushNilsForTemps.
			irBuilder returnTop.
			irBuilder popScope. ]
		ifFalse: [ irBuilder returnTop ]
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> popIntoLiteralVariable: offset [
	self storeIntoLiteralVariable: offset.
	self doPop
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> popIntoReceiverVariable: offset [
	self storeIntoReceiverVariable: offset.
	self doPop
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	self storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex.
	self doPop
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> popIntoTemporaryVariable: offset [
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

{ #category : 'scope' }
FBIRBytecodeDecompiler >> popScope [
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

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushActiveContext [
	irBuilder pushThisContext
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushConsArrayWithElements: numElements [
	irBuilder pushConsArray: numElements
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushConstant: value [
	irBuilder pushLiteral: value
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushFullClosure: compiledBlock numCopied: numCopied receiverOnStack: receiverOnStack ignoreOuterContext: ignoreOuterContext [

	| copiedValues |

	copiedValues := irBuilder removeLast: numCopied.

	irBuilder
		pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushLiteralVariable: assoc [
	irBuilder pushLiteralVariable: assoc
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushNewArrayOfSize: size [
	newTempVector := OCIRRemoteArray new size: size
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushReceiver [
	irBuilder pushReceiver
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushReceiverVariable: offset [
	irBuilder pushInstVar: offset + 1
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushRemoteTemp: remoteIndex inVectorAt: tempIndex [
	|  remoteArray remoteTempName |
	remoteArray := self scope tempAt: tempIndex.
	remoteTempName := self scope tempAt: remoteIndex inRemote: remoteArray.
	irBuilder pushRemoteTemp: remoteTempName inVector: remoteArray
]

{ #category : 'scope' }
FBIRBytecodeDecompiler >> pushScope: copiedValues numArgs: numArgs [
	|scope |
	scope := OCIRBytecodeScope new numArgs: numArgs.
	scopeStack push: scope.
	scope copiedValues: copiedValues
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> pushTemporaryVariable: offset [
	irBuilder pushTemp: (self scope tempAt: offset)
]

{ #category : 'quick methods' }
FBIRBytecodeDecompiler >> quickMethod [

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

{ #category : 'scope' }
FBIRBytecodeDecompiler >> scope [
	^ scopeStack top
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> send: selector super: superFlag numArgs: numArgs [
	superFlag
		ifTrue: [irBuilder send: selector toSuperOf: self methodClass]
		ifFalse: [irBuilder send: selector]
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> storeIntoLiteralVariable: value [
	irBuilder storeIntoLiteralVariable: value
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> storeIntoReceiverVariable: offset [
	irBuilder storeInstVar: offset + 1
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> storeIntoRemoteTemp: remoteIndex inVectorAt: tempIndex [
	|  remoteArray remoteTempName |
	remoteArray := self scope tempAt: tempIndex.
	remoteTempName := self scope tempAt: remoteIndex inRemote: remoteArray.
	irBuilder storeRemoteTemp: remoteTempName inVector: remoteArray
]

{ #category : 'instruction decoding' }
FBIRBytecodeDecompiler >> storeIntoTemporaryVariable: offset [
	irBuilder storeTemp: (self scope tempAt: offset)
]
