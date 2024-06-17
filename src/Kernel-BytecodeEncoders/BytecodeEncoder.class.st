"
I am an abstract superclass for different bytecode set encoders.  Subclasses inherit the literal management of Encoder and encapsulate the mapping of opcodes to specific bytecodes.
"
Class {
	#name : 'BytecodeEncoder',
	#superclass : 'Object',
	#instVars : [
		'stream',
		'position'
	],
	#category : 'Kernel-BytecodeEncoders',
	#package : 'Kernel-BytecodeEncoders'
}

{ #category : 'accessing' }
BytecodeEncoder class >> instructionSizeAt: pc of: aMehtod [

	self subclassResponsibility
]

{ #category : 'testing' }
BytecodeEncoder class >> isAbstract [

	^ self == BytecodeEncoder
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isExtension: byte [
	"Answer if the bytecode is that of an extension bytecodee (a prefix for other bytecodes to extend their range)."
	^self subclassResponsibility
]

{ #category : 'accessing' }
BytecodeEncoder class >> literalMethodBytecodes [
	"A literal method is a method that just answers a literal value.
	 This is a constant sequence of bytecodes but it changes between enconders"

	^ self subclassResponsibility
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> nonExtensionBytecodeAt: pc in: method [
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."
	| thePC bytecode |
	thePC := pc.
	[self isExtension: (bytecode := method at: thePC)] whileTrue:
		[thePC := thePC + (self bytecodeSize: bytecode)].
	^bytecode
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> nonExtensionPcAt: pc in: method [
	"Answer the pc of the actual bytecode at pc in method, skipping past any preceeding extensions."
	| thePC bytecode |
	thePC := pc.
	[self isExtension: (bytecode := method at: thePC)] whileTrue:
		[thePC := thePC + (self bytecodeSize: bytecode)].
	^thePC
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> quickPrimSpecialConstants [
	"Note: they are different from the encoder special literals."
	^ #(true false nil -1 0 1 2)
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> specialSelectors [
	^ #(#+ #- #< #> #'<=' #'>=' #= #'~=' #* #/ #'\\' #@ #bitShift: #'//' #bitAnd: #bitOr: #at: #at:put: #size #next #nextPut: #atEnd #'==' nil "class" #'~~' #value #value: #do: #new #new: #x #y)
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> stackDeltaForPrimitive: primitiveIndex in: method [
	"This is the default implementation. Subclasses with inline primitives
	will need to override."
	^ 0
]

{ #category : 'bytecode generation' }
BytecodeEncoder >> genPushSpecialLiteral: aLiteral [

	self subclassResponsibility
]

{ #category : 'bytecode generation' }
BytecodeEncoder >> genPushThisProcess [
	"only available in Sista BC"
]

{ #category : 'accessing' }
BytecodeEncoder >> methodStreamPosition [
	^stream position
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> nextPut: aByte [
	"For sizing make the encoder its own stream and
	 keep track of position with this version of nextPut:"
	position := position + 1
]

{ #category : 'bytecode generation' }
BytecodeEncoder >> outOfRangeError: string index: index range: rangeStart to: rangeEnd [
	"For now..."
	^self error: thisContext sender homeMethod selector, ' ', string
				, ' index ', index printString
				, ' is out of range ', rangeStart printString, ' to ', rangeEnd printString
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeBranchPopFalse: distance [
	^self sizeOpcodeSelector: #genBranchPopFalse: withArguments: {distance}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeBranchPopTrue: distance [
	^self sizeOpcodeSelector: #genBranchPopTrue: withArguments: {distance}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeCallPrimitive: primitiveIndex [
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeDup [
	^self sizeOpcodeSelector: #genDup withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeJump: distance [
	^self sizeOpcodeSelector: #genJump: withArguments: {distance}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeJumpLong: distance [
	^self sizeOpcodeSelector: #genJumpLong: withArguments: {distance}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeOpcodeSelector: genSelector withArguments: args [
	stream := self.
	position := 0.
	self perform: genSelector withArguments: args.
	^position
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePop [
	^self sizeOpcodeSelector: #genPop withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushConsArray: numElements [
	^self sizeOpcodeSelector: #genPushConsArray: withArguments: {numElements}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushInstVar: instVarIndex [
	^self sizeOpcodeSelector: #genPushInstVar: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushInstVarLong: instVarIndex [
	^self sizeOpcodeSelector: #genPushInstVarLong: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushLiteral: literalIndex [
	^self sizeOpcodeSelector: #genPushLiteral: withArguments: {literalIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushLiteralVar: literalIndex [
	^self sizeOpcodeSelector: #genPushLiteralVar: withArguments: {literalIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushNewArray: size [
	^self sizeOpcodeSelector: #genPushNewArray: withArguments: {size}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushReceiver [
	^self sizeOpcodeSelector: #genPushReceiver withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	^self sizeOpcodeSelector: #genPushRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushSpecialLiteral: specialLiteral [
	^self sizeOpcodeSelector: #genPushSpecialLiteral: withArguments: {specialLiteral}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushTemp: tempIndex [
	^self sizeOpcodeSelector: #genPushTemp: withArguments: {tempIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushThisContext [
	^self sizeOpcodeSelector: #genPushThisContext withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushThisProcess [
	^self sizeOpcodeSelector: #genPushThisProcess withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeReturnReceiver [
	^self sizeOpcodeSelector: #genReturnReceiver withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeReturnSpecialLiteral: specialLiteral [
	^self sizeOpcodeSelector: #genReturnSpecialLiteral: withArguments: {specialLiteral}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeReturnTop [
	^self sizeOpcodeSelector: #genReturnTop withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeReturnTopToCaller [
	^self sizeOpcodeSelector: #genReturnTopToCaller withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeSend: selectorLiteralIndex numArgs: nArgs [
	^self sizeOpcodeSelector: #genSend:numArgs: withArguments: {selectorLiteralIndex. nArgs}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeSendSuper: selectorLiteralIndex numArgs: nArgs [
	^self sizeOpcodeSelector: #genSendSuper:numArgs: withArguments: {selectorLiteralIndex. nArgs}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreInstVar: instVarIndex [
	^self sizeOpcodeSelector: #genStoreInstVar: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreInstVarLong: instVarIndex [
	^self sizeOpcodeSelector: #genStoreInstVarLong: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreLiteralVar: literalIndex [
	^self sizeOpcodeSelector: #genStoreLiteralVar: withArguments: {literalIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopInstVar: instVarIndex [
	^self sizeOpcodeSelector: #genStorePopInstVar: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopInstVarLong: instVarIndex [
	^self sizeOpcodeSelector: #genStorePopInstVarLong: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopLiteralVar: literalIndex [
	^self sizeOpcodeSelector: #genStorePopLiteralVar: withArguments: {literalIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	^self sizeOpcodeSelector: #genStorePopRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopTemp: tempIndex [
	^self sizeOpcodeSelector: #genStorePopTemp: withArguments: {tempIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	^self sizeOpcodeSelector: #genStoreRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreTemp: tempIndex [
	^self sizeOpcodeSelector: #genStoreTemp: withArguments: {tempIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeTrapIfNotInstanceOf: litIndex [
	^ self sizeOpcodeSelector: #genTrapIfNotInstanceOf: withArguments: {litIndex}
]

{ #category : 'accessing' }
BytecodeEncoder >> stream: s [
	stream := s
]

{ #category : 'initialization' }
BytecodeEncoder >> streamToMethod: aCompiledMethod [
	stream := WriteStream with: aCompiledMethod.
	stream position: aCompiledMethod initialPC - 1
]
