"
I am an abstract superclass for different bytecode set encoders.  Subclasses inherit the literal management of Encoder and encapsulate the mapping of opcodes to specific bytecodes.
"
Class {
	#name : #BytecodeEncoder,
	#superclass : #Encoder,
	#instVars : [
		'stream',
		'position',
		'rootNode',
		'blockExtentsToLocals'
	],
	#category : 'Kernel-BytecodeEncoders'
}

{ #category : #'bytecode decoding' }
BytecodeEncoder class >> nonExtensionBytecodeAt: pc in: method [
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."
	| thePC bytecode |
	thePC := pc.
	[self isExtension: (bytecode := method at: thePC)] whileTrue:
		[thePC := thePC + (self bytecodeSize: bytecode)].
	^bytecode
]

{ #category : #'bytecode decoding' }
BytecodeEncoder class >> specialSelectors [	
	^ #(#+ #- #< #> #'<=' #'>=' #= #'~=' #* #/ #'\\' #@ #bitShift: #'//' #bitAnd: #bitOr: #at: #at:put: #size #next #nextPut: #atEnd #'==' nil "class" #'~~' #value #value: #do: #new #new: #x #y)
]

{ #category : #'bytecode decoding' }
BytecodeEncoder class >> stackDeltaForPrimitive: primitiveIndex in: method [ 
	"This is the default implementation. Subclasses with inline primitives
	will need to override."
	^ 0
]

{ #category : #temps }
BytecodeEncoder >> bindAndJuggle: name [
	"This is used to insert a new temp and reorcder temps on editing.
	 It doesn't really work for closure compilation since we have multiple
	 locations for temps.  Simply signal a reparse is necessary."

	ReparseAfterSourceEditing signal
]

{ #category : #temps }
BytecodeEncoder >> blockExtentsToTempsMap [
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."
	| blockExtentsToTempsMap |
	blockExtentsToLocals ifNil:
		[^nil].
	blockExtentsToTempsMap := Dictionary new.
	blockExtentsToLocals keysAndValuesDo:
		[:blockExtent :locals|
		blockExtentsToTempsMap
			at: blockExtent
			put: (Array streamContents:
					[:str|
					locals withIndexDo:
						[:local :index|
						local isIndirectTempVector
							ifTrue: [local remoteTemps withIndexDo:
										[:remoteLocal :innerIndex| str nextPut: { remoteLocal key. { index. innerIndex } }]]
							ifFalse: [str nextPut: { local key. index }]]])].
	^blockExtentsToTempsMap
]

{ #category : #'bytecode generation' }
BytecodeEncoder >> genPushNClosureTemps: numTemps [
	numTemps timesRepeat: [ self genPushSpecialLiteral: nil ]
]

{ #category : #'bytecode generation' }
BytecodeEncoder >> genPushSpecialLiteral: aLiteral [

	self subclassResponsibility
]

{ #category : #testing }
BytecodeEncoder >> hasGeneratedMethod [
	^blockExtentsToLocals notNil
]

{ #category : #accessing }
BytecodeEncoder >> methodStreamPosition [
	^stream position
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> nextPut: aByte [
	"For sizing make the encoder its own stream and
	 keep track of position with this version of nextPut:"
	position := position + 1
]

{ #category : #temps }
BytecodeEncoder >> noteBlockExtent: blockExtent hasLocals: tempNodes [
	blockExtentsToLocals ifNil:
		[blockExtentsToLocals := Dictionary new].
	blockExtentsToLocals at: blockExtent put: tempNodes asArray
]

{ #category : #'bytecode generation' }
BytecodeEncoder >> outOfRangeError: string index: index range: rangeStart to: rangeEnd [
	"For now..."
	^self error: thisContext sender method selector, ' ', string
				, ' index ', index printString
				, ' is out of range ', rangeStart printString, ' to ', rangeEnd printString
]

{ #category : #accessing }
BytecodeEncoder >> rootNode [ "^<BlockNode>"
	^rootNode
]

{ #category : #accessing }
BytecodeEncoder >> rootNode: node [ "<BlockNode>"
	rootNode := node
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeBranchPopFalse: distance [
	^self sizeOpcodeSelector: #genBranchPopFalse: withArguments: {distance}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeBranchPopTrue: distance [
	^self sizeOpcodeSelector: #genBranchPopTrue: withArguments: {distance}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeCallPrimitive: primitiveIndex [
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeDup [
	^self sizeOpcodeSelector: #genDup withArguments: #()
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeJump: distance [
	^self sizeOpcodeSelector: #genJump: withArguments: {distance}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeJumpLong: distance [
	^self sizeOpcodeSelector: #genJumpLong: withArguments: {distance}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeOpcodeSelector: genSelector withArguments: args [
	stream := self.
	position := 0.
	self perform: genSelector withArguments: args.
	^position
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePop [
	^self sizeOpcodeSelector: #genPop withArguments: #()
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize [
	^self
		sizeOpcodeSelector: #genPushClosureCopyNumCopiedValues:numArgs:jumpSize:
		withArguments: {numCopied. numArgs. jumpSize}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushConsArray: numElements [
	^self sizeOpcodeSelector: #genPushConsArray: withArguments: {numElements}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushInstVar: instVarIndex [
	^self sizeOpcodeSelector: #genPushInstVar: withArguments: {instVarIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushInstVarLong: instVarIndex [
	^self sizeOpcodeSelector: #genPushInstVarLong: withArguments: {instVarIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushLiteral: literalIndex [
	^self sizeOpcodeSelector: #genPushLiteral: withArguments: {literalIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushLiteralVar: literalIndex [
	^self sizeOpcodeSelector: #genPushLiteralVar: withArguments: {literalIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushNewArray: size [
	^self sizeOpcodeSelector: #genPushNewArray: withArguments: {size}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushReceiver [
	^self sizeOpcodeSelector: #genPushReceiver withArguments: #()
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	^self sizeOpcodeSelector: #genPushRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushSpecialLiteral: specialLiteral [
	^self sizeOpcodeSelector: #genPushSpecialLiteral: withArguments: {specialLiteral}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushTemp: tempIndex [
	^self sizeOpcodeSelector: #genPushTemp: withArguments: {tempIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizePushThisContext [
	^self sizeOpcodeSelector: #genPushThisContext withArguments: #()
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeReturnReceiver [
	^self sizeOpcodeSelector: #genReturnReceiver withArguments: #()
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeReturnSpecialLiteral: specialLiteral [
	^self sizeOpcodeSelector: #genReturnSpecialLiteral: withArguments: {specialLiteral}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeReturnTop [
	^self sizeOpcodeSelector: #genReturnTop withArguments: #()
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeReturnTopToCaller [
	^self sizeOpcodeSelector: #genReturnTopToCaller withArguments: #()
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeSend: selectorLiteralIndex numArgs: nArgs [
	^self sizeOpcodeSelector: #genSend:numArgs: withArguments: {selectorLiteralIndex. nArgs}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeSendSuper: selectorLiteralIndex numArgs: nArgs [
	^self sizeOpcodeSelector: #genSendSuper:numArgs: withArguments: {selectorLiteralIndex. nArgs}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStoreInstVar: instVarIndex [
	^self sizeOpcodeSelector: #genStoreInstVar: withArguments: {instVarIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStoreInstVarLong: instVarIndex [
	^self sizeOpcodeSelector: #genStoreInstVarLong: withArguments: {instVarIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStoreLiteralVar: literalIndex [
	^self sizeOpcodeSelector: #genStoreLiteralVar: withArguments: {literalIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStorePopInstVar: instVarIndex [
	^self sizeOpcodeSelector: #genStorePopInstVar: withArguments: {instVarIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStorePopInstVarLong: instVarIndex [
	^self sizeOpcodeSelector: #genStorePopInstVarLong: withArguments: {instVarIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStorePopLiteralVar: literalIndex [
	^self sizeOpcodeSelector: #genStorePopLiteralVar: withArguments: {literalIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	^self sizeOpcodeSelector: #genStorePopRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStorePopTemp: tempIndex [
	^self sizeOpcodeSelector: #genStorePopTemp: withArguments: {tempIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	^self sizeOpcodeSelector: #genStoreRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeStoreTemp: tempIndex [
	^self sizeOpcodeSelector: #genStoreTemp: withArguments: {tempIndex}
]

{ #category : #'opcode sizing' }
BytecodeEncoder >> sizeTrapIfNotInstanceOf: litIndex [ 
	^ self sizeOpcodeSelector: #genTrapIfNotInstanceOf: withArguments: {litIndex}

]

{ #category : #accessing }
BytecodeEncoder >> stream: s [
	stream := s
]

{ #category : #initialize-release }
BytecodeEncoder >> streamToMethod: aCompiledMethod [
	stream := WriteStream with: aCompiledMethod.
	stream position: aCompiledMethod initialPC - 1
]

{ #category : #testing }
BytecodeEncoder >> supportsClosureOpcodes [
	"Answer if the receiver supports the
		genPushNewArray:/genPushConsArray:
		genPushRemoteTemp:inVectorAt:
		genStoreRemoteTemp:inVectorAt:
		genStorePopRemoteTemp:inVectorAt:
		genPushClosureCopyCopiedValues:numArgs:jumpSize:
	 opcodes"
	^false
]
