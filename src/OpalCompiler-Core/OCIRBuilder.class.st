"
I provide a simple interface for constructing an `IRMethod`.  

For example, to create an ir method that compares first instVar to first arg and returns 'yes' or 'no' (same example as in `BytecodeGenerator`), do:

```
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
```

Sending the message `compiledMethod` to an ir method generates its compiledMethod.  Sending the message `methodNode` to it decompiles to its parse tree.

"
Class {
	#name : 'OCIRBuilder',
	#superclass : 'Object',
	#instVars : [
		'ir',
		'currentScope',
		'jumpBackTargetStacks',
		'jumpAheadStacks',
		'currentSequence',
		'sourceMapNodes',
		'sourceMapByteIndex'
	],
	#category : 'OpalCompiler-Core-IR-Manipulation',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Manipulation'
}

{ #category : 'builder api' }
OCIRBuilder class >> buildIR: aBlock [
	^(aBlock value: self new) ir
]

{ #category : 'builder api' }
OCIRBuilder class >> buildMethod: aBlock [
	^(self buildIR: aBlock) compiledMethod
]

{ #category : 'private' }
OCIRBuilder >> add: instr [

	"Associate instr with current parse node or byte range"
	instr sourceNode: self sourceNode.
	instr bytecodeIndex: self sourceByteIndex.
	^ currentSequence add: instr
]

{ #category : 'instructions' }
OCIRBuilder >> addBlockReturnTopIfRequired [
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

{ #category : 'decompiling' }
OCIRBuilder >> addJumpBackTarget: label to: sequence [

	(jumpBackTargetStacks at: label ifAbsentPut: [OrderedCollection new])
		addLast: sequence
]

{ #category : 'initialize' }
OCIRBuilder >> addLiteral: aLiteral [
	"Add this literal at the end of the literal array if there is space left"
	aLiteral ifNil: [ ^ self ].
	ir addAdditionalLiteral: aLiteral
]

{ #category : 'initialize' }
OCIRBuilder >> addPragma: aPragma [
	^ir addPragma: aPragma
]

{ #category : 'initialize' }
OCIRBuilder >> addTemp: tempKey [

	| tempMap |
	tempMap := self currentScope tempMap.

	"adding the same var multiple times reuses the same offset"
	(tempMap includesKey: tempKey) ifTrue: [ ^ 	self cacheIndex: tempKey ].

	tempMap at: tempKey put: tempMap size + 1.
	self cacheIndex: tempKey
]

{ #category : 'initialize' }
OCIRBuilder >> addTemps: newKeys [
	newKeys do: [ :key | self addTemp: key ]
]

{ #category : 'initialize' }
OCIRBuilder >> additionalLiterals:  literals [
	"Add this literal at the end of the literal array if there is space left"
	ir addAdditionalLiterals: literals
]

{ #category : 'instructions' }
OCIRBuilder >> blockReturnTop [
	| retInst |

	retInst := OCIRInstruction blockReturnTop.
	self add: retInst.
	self startNewSequence.
	retInst  successor: currentSequence.
	self popScope
]

{ #category : 'initialize' }
OCIRBuilder >> cacheIndex: tempKey [
	"if we have the ast, we can store the temp offset"
	self sourceNode ifNotNil: [ :sourceNode |
		| var |
		var := sourceNode scope lookupVar: tempKey.
		"some vars (e.g. limits in loops) do not exist on the AST level"
		var ifNotNil: [ var index: (self currentScope tempMap at: tempKey) ] ]
]

{ #category : 'accessing' }
OCIRBuilder >> compilationContext [
	^ir compilationContext
]

{ #category : 'initialize' }
OCIRBuilder >> compilationContext: aCompilationContext [
	ir compilationContext: aCompilationContext
]

{ #category : 'initialize' }
OCIRBuilder >> createTempVectorNamed: name withVars: anArray [

	self addTemp: name.
	self add: (OCIRInstruction createTempVectorNamed: name withVars: anArray).

	"if we have the ast, we can store the temp offsets"
	self sourceNode ifNotNil: [ :sourceNode |
		anArray doWithIndex: [ :varName :i|
			(sourceNode scope lookupVar: varName) index: i]]
]

{ #category : 'scopes' }
OCIRBuilder >> currentScope [
	^currentScope top
]

{ #category : 'initialization' }
OCIRBuilder >> initialize [
	ir := OCIRMethod new.
	jumpAheadStacks := IdentityDictionary new.
	jumpBackTargetStacks := IdentityDictionary new.
	sourceMapNodes := OrderedCollection new.	"stack"
	currentScope := Stack new.
	self pushScope: ir.

	"Leave an empty sequence up front (guaranteed not to be in loop)"
	ir startSequence: ((OCIRSequence orderNumber: 0) method:ir).
	currentSequence := (OCIRSequence orderNumber: 1) method:ir.
	ir startSequence add:
		(OCIRJump new
			destination: currentSequence;
			bytecodeIndex: sourceMapByteIndex;
			yourself)
]

{ #category : 'results' }
OCIRBuilder >> ir [

	self optimize.
	^ ir
]

{ #category : 'accessing' }
OCIRBuilder >> irPrimitive: primNode [

	ir irPrimitive: primNode
]

{ #category : 'instructions' }
OCIRBuilder >> jumpAheadTarget: labelSymbol [
	"Pop latest jumpAheadTo: with this labelSymbol and have it point to this new instruction sequence"

	| jumpInstr |
	self startNewSequence.
	jumpInstr := (jumpAheadStacks at: labelSymbol ifAbsent: [
			self error: 'Missing jumpAheadTo: ', labelSymbol printString]) removeLast.
	jumpInstr destination: currentSequence
]

{ #category : 'instructions' }
OCIRBuilder >> jumpAheadTo: labelSymbol [
	"Jump to the sequence that will be created when jumpAheadTarget: labelSymbol is sent to self.  This is and its corresponding target is only good for one use.  Other jumpAheadTo: with the same label will be put on a stack and superceed existing ones until its jumpAheadTarget: is called."

	"jumpAheadTarget: label will pop this and replace destination with its basic block"

	(jumpAheadStacks at: labelSymbol ifAbsentPut: [OrderedCollection new])
		addLast: (self add: OCIRJump new).
	self startNewSequence
]

{ #category : 'instructions' }
OCIRBuilder >> jumpAheadTo: labelSymbol if: boolean [
	"Conditional jump to the sequence that will be created when jumpAheadTarget: labelSymbol is sent to self.  This and its corresponding target is only good for one use.  Other jumpAheadTo:... with the same label will be put on a stack and superceed existing ones until its jumpAheadTarget: is called."

	| instr |
	"jumpAheadTarget: label will pop this and replace destination with its basic block"
	(jumpAheadStacks at: labelSymbol ifAbsentPut: [OrderedCollection new])
		addLast: (instr := self add: (OCIRJumpIf new boolean: boolean)).
	self startNewSequence.
	instr otherwise: currentSequence
]

{ #category : 'instructions' }
OCIRBuilder >> jumpBackTarget: labelSymbol [
	"Remember this basic block for a future jumpBackTo: labelSymbol.  Stack up remembered targets with same name and remove them from stack for each jumpBackTo: called with same name."

	self startNewSequence.
	(jumpBackTargetStacks at: labelSymbol ifAbsentPut: [OrderedCollection new])
		addLast: currentSequence
]

{ #category : 'instructions' }
OCIRBuilder >> jumpBackTo: labelSymbol [
	"Pop last remembered position with this label and write an unconditional jump to it"

	| sequence jump |
	sequence := (jumpBackTargetStacks at: labelSymbol ifAbsent: [self error: 'Missing jumpBackTarget: ', labelSymbol printString]) removeLast.
	jump := OCIRJump new destination: sequence.
	self add: jump.
	self startNewSequence.
	jump successor: currentSequence
]

{ #category : 'mapping' }
OCIRBuilder >> mapToByteIndex: index [
	"decompiling"

	sourceMapByteIndex := index
]

{ #category : 'mapping' }
OCIRBuilder >> mapToNode: object [
	"new instructions will be associated with object"

	sourceMapNodes addLast: object
]

{ #category : 'initialize' }
OCIRBuilder >> numArgs: anInteger [
	ir numArgs: anInteger.
	ir sourceNode: self sourceNode
]

{ #category : 'optimizing' }
OCIRBuilder >> optimize [
	"Perform configured optimizations"

	ir removeEmptyStart.
	self compilationContext optionOptimizeIR ifFalse: [^self].
	ir absorbJumpsToSingleInstrs
]

{ #category : 'mapping' }
OCIRBuilder >> popMap [

	sourceMapNodes removeLast
]

{ #category : 'scopes' }
OCIRBuilder >> popScope [
	currentScope pop
]

{ #category : 'instructions' }
OCIRBuilder >> popTop [

	currentSequence ifNotEmpty: [
		currentSequence last isRemovableByPop ifTrue: [
			^ currentSequence removeLast ] ].
	self add: OCIRInstruction popTop
]

{ #category : 'accessing' }
OCIRBuilder >> properties: aDict [
	ir properties: aDict
]

{ #category : 'instructions' }
OCIRBuilder >> pushConsArray: size [

	self add: (OCIRInstruction pushConsArray: size)
]

{ #category : 'instructions' }
OCIRBuilder >> pushDup [

	self add: OCIRInstruction pushDup
]

{ #category : 'instructions' }
OCIRBuilder >> pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues [

	^ self add: (OCIRInstruction pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues)
]

{ #category : 'instructions' }
OCIRBuilder >> pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues outerContextNeeded: aBoolean [

	^ self add: (OCIRInstruction pushFullClosureCompiledBlock: compiledBlock copiedValues: copiedValues outerContextNeeded: aBoolean)
]

{ #category : 'instructions' }
OCIRBuilder >> pushInstVar: index [

	self add: (OCIRInstruction pushInstVar: index)
]

{ #category : 'instructions' }
OCIRBuilder >> pushLiteral: object [

	self add: (OCIRInstruction pushLiteral: object)
]

{ #category : 'instructions' }
OCIRBuilder >> pushLiteralVariable: object [

	self add: (OCIRInstruction pushLiteralVariable: object)
]

{ #category : 'instructions' }
OCIRBuilder >> pushNewArray: size [

	self add: (OCIRInstruction pushNewArray: size)
]

{ #category : 'instructions' }
OCIRBuilder >> pushReceiver [

	self add: (OCIRInstruction pushReceiver)
]

{ #category : 'instructions' }
OCIRBuilder >> pushRemoteTemp: name inVector: nameOfVector [

	^self add: (OCIRInstruction pushRemoteTemp: name inVectorAt: nameOfVector)
]

{ #category : 'scopes' }
OCIRBuilder >> pushScope: anIRBlockOrMethod [

	currentScope push: anIRBlockOrMethod
]

{ #category : 'instructions' }
OCIRBuilder >> pushTemp: aSelector [

	^ self add: (OCIRInstruction pushTemp: aSelector)
]

{ #category : 'instructions' }
OCIRBuilder >> pushThisContext [

	self add: (OCIRInstruction pushThisContext)
]

{ #category : 'instructions' }
OCIRBuilder >> pushThisProcess [
	"This is only supported with EncoderForSistaV1"
	self add: (OCIRInstruction pushThisProcess)
]

{ #category : 'instructions' }
OCIRBuilder >> returnTop [

	self add: OCIRInstruction returnTop.
	self startNewSequence
]

{ #category : 'instructions' }
OCIRBuilder >> send: selector [

	^self add: (OCIRInstruction send: selector)
]

{ #category : 'instructions' }
OCIRBuilder >> send: selector toSuperOf: behavior [

	^self add: (OCIRInstruction send: selector toSuperOf: behavior)
]

{ #category : 'mapping' }
OCIRBuilder >> sourceByteIndex [

	^ sourceMapByteIndex
]

{ #category : 'mapping' }
OCIRBuilder >> sourceNode [
	^ sourceMapNodes
		ifEmpty: [nil]
		ifNotEmpty: [sourceMapNodes last]
]

{ #category : 'private' }
OCIRBuilder >> startNewSequence [
	"End current instruction sequence and start a new sequence to add instructions to.  If ending block just falls through to new block then add an explicit jump to it so they stay linked"

	| newSequence |
	currentSequence ifEmpty: [^ self].	"block is still empty, continue using it"
	newSequence := OCIRSequence orderNumber: currentSequence orderNumber + 1.
	newSequence method: ir.

	currentSequence last transitionsToNextSequence
		ifFalse: [ self add: (OCIRJump new destination: newSequence)].
	currentSequence := newSequence
]

{ #category : 'instructions' }
OCIRBuilder >> storeInstVar: name [

	^self add: (OCIRInstruction storeInstVar: name)
]

{ #category : 'instructions' }
OCIRBuilder >> storeIntoLiteralVariable: name [

	^self add: (OCIRInstruction storeIntoLiteralVariable: name)
]

{ #category : 'instructions' }
OCIRBuilder >> storeRemoteTemp: name inVector: nameOfVector [

	^self add: (OCIRInstruction storeRemoteTemp: name inVectorAt: nameOfVector)
]

{ #category : 'instructions' }
OCIRBuilder >> storeTemp: aSymbol [

	^self add: (OCIRInstruction storeTemp: aSymbol)
]

{ #category : 'decompiling' }
OCIRBuilder >> testJumpAheadTarget: label [

	jumpAheadStacks at: label ifPresent: [:stack |
		[stack isEmpty] whileFalse: [self jumpAheadTarget: label]
	]
]
