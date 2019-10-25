"
I provide a simple interface for constructing an IRMethod.  For example, to create an ir method that compares first instVar to first arg and returns 'yes' or 'no' (same example as in BytecodeGenerator), do:

	IRBuilder new
		numArgs: 1;
		addTemps: #(a z);
		pushReceiver;
		pushInstVar: 1;
		pushTemp: #a;
		send: #>;
		jumpAheadTo: #else if: false;
		pushLiteral: 'yes';
		returnTop;
		jumpAheadTarget: #else;
		pushLiteral: 'no';
		returnTop;
		ir

Sending #compiledMethod to an ir method will generate its compiledMethod.  Sending #methodNode to it will decompile to its parse tree.

"
Class {
	#name : #IRBuilder,
	#superclass : #Object,
	#instVars : [
		'ir',
		'currentScope',
		'jumpBackTargetStacks',
		'jumpAheadStacks',
		'currentSequence',
		'sourceMapNodes',
		'sourceMapByteIndex'
	],
	#category : #'OpalCompiler-Core-IR-Manipulation'
}

{ #category : #'builder api' }
IRBuilder class >> buildIR: aBlock [
	^(aBlock value: self new) ir.
]

{ #category : #'builder api' }
IRBuilder class >> buildMethod: aBlock [
	^(self buildIR: aBlock) compiledMethod
]

{ #category : #private }
IRBuilder >> add: instr [
	
	"Associate instr with current parse node or byte range"
	instr sourceNode: self sourceNode.
	instr bytecodeIndex: self sourceByteIndex.
	^ currentSequence add: instr
]

{ #category : #instructions }
IRBuilder >> addBlockReturnTopIfRequired [
	"If the current sequence is empty this means that there was a returntop before 
	then since there is no more stmts we do not need a blockreturntop"
	
	"cant we optimize this away later? Then the frontend can always just add a return...."
	
	| predecessors |
	self flag: 'cant we do this automatically.... frontend always adds return, we ignore it if there is a ret method before?'.
	
	predecessors := ir predecessorsOf: currentSequence.
	(currentSequence isEmpty and: [(predecessors isEmpty) or: [((ir predecessorsOf: currentSequence) 
								anySatisfy: [:each | (each last isBlockReturnTop not) and: [(each last isReturn) 
												or: [(each size > 1) and: [(each at: each size -1) isReturn] ]]])] ])
							ifTrue: [self popScope]
							ifFalse: [self blockReturnTop ]
]

{ #category : #decompiling }
IRBuilder >> addJumpBackTarget: label to: sequence [

	(jumpBackTargetStacks at: label ifAbsentPut: [OrderedCollection new])
		addLast: sequence
]

{ #category : #initialize }
IRBuilder >> addLiteral: aLiteral [
	"Add this literal at the end of the literal array if there is space left"
	aLiteral ifNil: [ ^ self ].
	ir addAdditionalLiteral: aLiteral.
]

{ #category : #initialize }
IRBuilder >> addPragma: aPragma [
	^ir addPragma: aPragma
]

{ #category : #initialize }
IRBuilder >> addTemp: tempKey [
	
	self addTemps: {tempKey}
]

{ #category : #initialize }
IRBuilder >> addTemps: newKeys [
	| i offset |
	offset := self currentScope tempMap size.
	i := 1.
	newKeys do: [:key |
		self  currentScope tempMap at: key ifAbsentPut: [i:=i+1.offset + i - 1].
	].

]

{ #category : #initialize }
IRBuilder >> additionalLiterals:  literals [
	"Add this literal at the end of the literal array if there is space left"
	ir addAdditionalLiterals: literals.
]

{ #category : #instructions }
IRBuilder >> blockReturnTop [
	| retInst |
	
	retInst := IRInstruction blockReturnTop.
	self add: retInst.
	self startNewSequence.
	retInst  successor: currentSequence.
	self popScope.
]

{ #category : #accessing }
IRBuilder >> compilationContext [
	^ir compilationContext 
]

{ #category : #initialize }
IRBuilder >> compilationContext: aCompilationContext [
	ir compilationContext: aCompilationContext
]

{ #category : #initialize }
IRBuilder >> createTempVectorNamed: name withVars: anArray [
	
	"self addVectorTemps: anArray."
	self addTemp: name.
	self add: (IRInstruction createTempVectorNamed: name withVars: anArray).

]

{ #category : #scopes }
IRBuilder >> currentScope [ 
	^currentScope top
]

{ #category : #initialization }
IRBuilder >> initialize [
	ir := IRMethod new.
	jumpAheadStacks := IdentityDictionary new.
	jumpBackTargetStacks := IdentityDictionary new.
	sourceMapNodes := OrderedCollection new.	"stack"
	currentScope := Stack new.
	self pushScope: ir.
	
	"Leave an empty sequence up front (guaranteed not to be in loop)"
	ir startSequence: ((IRSequence orderNumber: 0) method:ir).
	currentSequence := (IRSequence orderNumber: 1) method:ir.
	ir startSequence add:
		(IRJump new
			destination: currentSequence;
			bytecodeIndex: sourceMapByteIndex;
			yourself).

]

{ #category : #results }
IRBuilder >> ir [

	^ ir optimize
]

{ #category : #accessing }
IRBuilder >> irPrimitive: primNode [

	ir irPrimitive: primNode
]

{ #category : #instructions }
IRBuilder >> jumpAheadTarget: labelSymbol [
	"Pop latest jumpAheadTo: with this labelSymbol and have it point to this new instruction sequence"

	| jumpInstr |
	self startNewSequence.
	jumpInstr := (jumpAheadStacks at: labelSymbol ifAbsent: [
			self error: 'Missing jumpAheadTo: ', labelSymbol printString]) removeLast.
	jumpInstr destination: currentSequence.

]

{ #category : #instructions }
IRBuilder >> jumpAheadTo: labelSymbol [
	"Jump to the sequence that will be created when jumpAheadTarget: labelSymbol is sent to self.  This is and its corresponding target is only good for one use.  Other jumpAheadTo: with the same label will be put on a stack and superceed existing ones until its jumpAheadTarget: is called."

	"jumpAheadTarget: label will pop this and replace destination with its basic block"

	(jumpAheadStacks at: labelSymbol ifAbsentPut: [OrderedCollection new])
		addLast: (self add: IRJump new).
	self startNewSequence.

]

{ #category : #instructions }
IRBuilder >> jumpAheadTo: labelSymbol if: boolean [
	"Conditional jump to the sequence that will be created when jumpAheadTarget: labelSymbol is sent to self.  This and its corresponding target is only good for one use.  Other jumpAheadTo:... with the same label will be put on a stack and superceed existing ones until its jumpAheadTarget: is called."

	| instr |
	"jumpAheadTarget: label will pop this and replace destination with its basic block"
	(jumpAheadStacks at: labelSymbol ifAbsentPut: [OrderedCollection new])
		addLast: (instr := self add: (IRJumpIf new boolean: boolean)).
	self startNewSequence.
	instr otherwise: currentSequence.

]

{ #category : #instructions }
IRBuilder >> jumpBackTarget: labelSymbol [
	"Remember this basic block for a future jumpBackTo: labelSymbol.  Stack up remembered targets with same name and remove them from stack for each jumpBackTo: called with same name."

	self startNewSequence.
	(jumpBackTargetStacks at: labelSymbol ifAbsentPut: [OrderedCollection new])
		addLast: currentSequence.

]

{ #category : #instructions }
IRBuilder >> jumpBackTo: labelSymbol [
	"Pop last remembered position with this label and write an unconditional jump to it"

	| sequence jump |
	sequence := (jumpBackTargetStacks at: labelSymbol ifAbsent: [self error: 'Missing jumpBackTarget: ', labelSymbol printString]) removeLast.
	jump := IRJump new destination: sequence.
	self add: jump.
	self startNewSequence.
	jump successor: currentSequence.
	
]

{ #category : #mapping }
IRBuilder >> mapToByteIndex: index [
	"decompiling"

	sourceMapByteIndex := index
]

{ #category : #mapping }
IRBuilder >> mapToNode: object [
	"new instructions will be associated with object"

	sourceMapNodes addLast: object
]

{ #category : #initialize }
IRBuilder >> numArgs: anInteger [
	ir numArgs: anInteger.
	ir sourceNode: self sourceNode.
]

{ #category : #mapping }
IRBuilder >> popMap [

	sourceMapNodes removeLast
]

{ #category : #scopes }
IRBuilder >> popScope [
	currentScope pop.
]

{ #category : #instructions }
IRBuilder >> popTop [

	self add: IRInstruction popTop
]

{ #category : #accessing }
IRBuilder >> properties: aDict [
	ir properties: aDict
]

{ #category : #instructions }
IRBuilder >> pushClosureCopyCopiedValues: copiedValuesNames args: args jumpTo: aJumpLabel [
	
	|  irInstruction |
	
	irInstruction := IRInstruction pushClosureCopyCopiedValues: copiedValuesNames args: args.
	self add: irInstruction.
	(jumpAheadStacks at: aJumpLabel ifAbsentPut: [OrderedCollection new]) addLast: irInstruction.
	self startNewSequence.
	irInstruction blockSequence: currentSequence.
	self pushScope: irInstruction.
	self addTemps: args.
	self addTemps: copiedValuesNames.


	^irInstruction
]

{ #category : #instructions }
IRBuilder >> pushConsArray: size [

	self add: (IRInstruction pushConsArray: size)
]

{ #category : #instructions }
IRBuilder >> pushDup [

	self add: IRInstruction pushDup
]

{ #category : #instructions }
IRBuilder >> pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues [

	self add: (IRInstruction pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues)
]

{ #category : #instructions }
IRBuilder >> pushInstVar: index [

	self add: (IRInstruction pushInstVar: index).
]

{ #category : #instructions }
IRBuilder >> pushLiteral: object [

	self add: (IRInstruction pushLiteral: object)
]

{ #category : #instructions }
IRBuilder >> pushLiteralVariable: object [

	self add: (IRInstruction pushLiteralVariable: object)
]

{ #category : #instructions }
IRBuilder >> pushNewArray: size [

	self add: (IRInstruction pushNewArray: size)
]

{ #category : #instructions }
IRBuilder >> pushReceiver [

	self add: (IRInstruction pushReceiver)
]

{ #category : #instructions }
IRBuilder >> pushRemoteTemp: name inVector: nameOfVector [

	^self add: (IRInstruction pushRemoteTemp: name inVectorAt: nameOfVector)
]

{ #category : #scopes }
IRBuilder >> pushScope: anIRBlockOrMethod [

	currentScope push: anIRBlockOrMethod
]

{ #category : #instructions }
IRBuilder >> pushTemp: aSelector [

	^ self add: (IRInstruction pushTemp: aSelector)
]

{ #category : #instructions }
IRBuilder >> pushThisContext [

	self add: (IRInstruction pushThisContext)
]

{ #category : #instructions }
IRBuilder >> returnTop [

	self add: IRInstruction returnTop.
	self startNewSequence.

]

{ #category : #instructions }
IRBuilder >> send: selector [

	^self add: (IRInstruction send: selector)
]

{ #category : #instructions }
IRBuilder >> send: selector toSuperOf: behavior [

	^self add: (IRInstruction send: selector toSuperOf: behavior)
]

{ #category : #mapping }
IRBuilder >> sourceByteIndex [

	^ sourceMapByteIndex
]

{ #category : #mapping }
IRBuilder >> sourceNode [
	^ sourceMapNodes
		ifEmpty: [nil]
		ifNotEmpty: [sourceMapNodes last]
]

{ #category : #private }
IRBuilder >> startNewSequence [
	"End current instruction sequence and start a new sequence to add instructions to.  If ending block just falls through to new block then add an explicit jump to it so they stay linked"

	| newSequence | 
	currentSequence ifEmpty: [^ self].	"block is still empty, continue using it"
	newSequence := IRSequence orderNumber: currentSequence orderNumber + 1.
	newSequence method: ir.

	currentSequence last transitionsToNextSequence
		ifFalse: [ self add: (IRJump new destination: newSequence)].
	currentSequence := newSequence.

]

{ #category : #instructions }
IRBuilder >> storeInstVar: name [

	^self add: (IRInstruction storeInstVar: name).
]

{ #category : #instructions }
IRBuilder >> storeIntoLiteralVariable: name [
	
	^self add: (IRInstruction storeIntoLiteralVariable: name)
]

{ #category : #instructions }
IRBuilder >> storeRemoteTemp: name inVector: nameOfVector [

	^self add: (IRInstruction storeRemoteTemp: name inVectorAt: nameOfVector)
]

{ #category : #instructions }
IRBuilder >> storeTemp: aSymbol [

	^self add: (IRInstruction storeTemp: aSymbol)
]

{ #category : #decompiling }
IRBuilder >> testJumpAheadTarget: label [

	jumpAheadStacks at: label ifPresent: [:stack |
		[stack isEmpty] whileFalse: [self jumpAheadTarget: label]
	]
]
