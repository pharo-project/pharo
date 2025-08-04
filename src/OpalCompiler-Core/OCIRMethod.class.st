"
I am a method in the IR (intermediate representation) language consisting of IRInstructions grouped by IRSequence (basic block).  The IRSequences form a control graph (therefore I only have to hold onto the starting sequence).  
- The message `compiledMethod` will convert me to a CompiledMethod.  
- The message `methodNode` will convert me back to a parse tree.

"
Class {
	#name : 'OCIRMethod',
	#superclass : 'Object',
	#instVars : [
		'sourceNode',
		'startSequence',
		'numArgs',
		'properties',
		'tempMap',
		'pragmas',
		'additionalLiterals',
		'compiledMethod',
		'compilationContext',
		'irPrimitive'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'optimizing' }
OCIRMethod >> absorbJumpsToSingleInstrs [

	startSequence absorbJumpToSingleInstr: IdentitySet new
]

{ #category : 'visiting' }
OCIRMethod >> accept: aVisitor [
	^ aVisitor visitMethod: self
]

{ #category : 'accessing' }
OCIRMethod >> addAdditionalLiteral: aLiteral [
	additionalLiterals add: aLiteral
]

{ #category : 'accessing' }
OCIRMethod >> addAdditionalLiterals: literals [
	additionalLiterals :=  literals
]

{ #category : 'inlining' }
OCIRMethod >> addInstructionsAfter: aCollection [
	| returningSeqs  lastInstr |
	aCollection ifEmpty: [^self].
	returningSeqs := self allSequences select: [:each | each last isReturn].
	lastInstr := returningSeqs last last.
	lastInstr addInstructionsBefore: aCollection
]

{ #category : 'inlining' }
OCIRMethod >> addInstructionsBefore: aCollection [

	(self startSequence nextSequence first) addInstructionsBefore: aCollection
]

{ #category : 'accessing' }
OCIRMethod >> addPragma: aPragma [

	pragmas add: aPragma
]

{ #category : 'accessing' }
OCIRMethod >> additionalLiterals [
	^additionalLiterals
]

{ #category : 'enumerating' }
OCIRMethod >> allInstructions [
	" return irNodes as a flat collection "

	| irInstructions |
	irInstructions := OrderedCollection new.
	startSequence withAllSuccessorsDo: [:seq | seq do: [:bc | irInstructions add: bc]].
	^irInstructions
]

{ #category : 'enumerating' }
OCIRMethod >> allInstructionsMatching: aBlock [
	" return irNodes as a flat collection "

	| irInstructions |
	irInstructions := OrderedCollection new.
	startSequence withAllSuccessorsDo: [:seq | seq do: [:bc | (aBlock value: bc) ifTrue: [irInstructions add: bc]]].
	^irInstructions
]

{ #category : 'enumerating' }
OCIRMethod >> allSequences [
	^ startSequence withAllSuccessors
]

{ #category : 'enumerating' }
OCIRMethod >> allTempAccessInstructions [
	^self allInstructionsMatching: [:bc | bc isTemp]
]

{ #category : 'inspector' }
OCIRMethod >> children [
	^self startSequence withAllSuccessors
]

{ #category : 'accessing' }
OCIRMethod >> compilationContext [
	^ compilationContext ifNil: [
		"only happens when decompiling or using stand-alone"
		compilationContext := OCCompilationContext default]
]

{ #category : 'accessing' }
OCIRMethod >> compilationContext: aCompilationContext [
	compilationContext := aCompilationContext
]

{ #category : 'translating' }
OCIRMethod >> compiledBlock: scope [

	^compiledMethod
		ifNil: [self generateBlockWithScope: scope ]
		ifNotNil: [compiledMethod]
]

{ #category : 'translating' }
OCIRMethod >> compiledMethod [

	^ compiledMethod 
		ifNil: [self generate ]

]

{ #category : 'enumerating' }
OCIRMethod >> firstInstructionMatching: aBlock [
	" return irNodes as a flat collection "

	startSequence withAllSuccessorsDo: [:seq | seq do: [:bc | (aBlock value: bc) ifTrue: [^bc]]].
	^nil
]

{ #category : 'translating' }
OCIRMethod >> generate [

	| irTranslator |
   irTranslator := OCIRTranslatorVisitor context: self compilationContext.
	irTranslator
		visitNode: self;
		pragmas: pragmas.
	compiledMethod := irTranslator compiledMethod.
	self sourceNode
		ifNotNil: [
			compiledMethod classBinding: self sourceNode methodClass binding.
			compiledMethod selector: self sourceNode selector ]
		ifNil: [
			compiledMethod classBinding: UndefinedObject binding.
			compiledMethod selector: #UndefinedMethod ].
	^ compiledMethod
]

{ #category : 'translating' }
OCIRMethod >> generateBlockWithScope: scope [
	| irTranslator |
      irTranslator := OCIRTranslatorVisitor context: self compilationContext.
	irTranslator
		pushOuterVectors: scope;
		visitNode: self;
		pragmas: pragmas.
	compiledMethod := irTranslator compiledBlock.
	^ compiledMethod
]

{ #category : 'scoping' }
OCIRMethod >> indexForVarNamed: aName [

	^tempMap at: aName
]

{ #category : 'initialization' }
OCIRMethod >> initialize [

	irPrimitive := OCIRPrimitive null.
	tempMap := Dictionary new.
	pragmas := OrderedCollection new.
	additionalLiterals := OCLiteralSet new.
	numArgs := 0
]

{ #category : 'decompiling' }
OCIRMethod >> instructionsForDecompiling [
	"return all instructions, but skip the block bodies, as the decompiler
	recurses over blocks"

	^startSequence instructionsForDecompiling allButFirst
]

{ #category : 'accessing' }
OCIRMethod >> ir [
	^self
]

{ #category : 'accessing' }
OCIRMethod >> irPrimitive [

	^ irPrimitive
]

{ #category : 'accessing' }
OCIRMethod >> irPrimitive: aPrimitiveNode [

	irPrimitive := aPrimitiveNode
]

{ #category : 'testing' }
OCIRMethod >> isSend [
	^false
]

{ #category : 'printing' }
OCIRMethod >> longPrintString [

	^ String streamContents: [ :stream |
		OCIRPrinterVisitor new
			stream: stream;
			visitNode: self ]
]

{ #category : 'accessing' }
OCIRMethod >> method [
	^self
]

{ #category : 'accessing' }
OCIRMethod >> numArgs [

	^ numArgs
]

{ #category : 'accessing' }
OCIRMethod >> numArgs: anInteger [

	numArgs := anInteger
]

{ #category : 'optimizing' }
OCIRMethod >> optimize [
	"Perform all optimizations"

	self removeEmptyStart.
	self absorbJumpsToSingleInstrs
]

{ #category : 'accessing' }
OCIRMethod >> predecessorsOf: aSequence [
	| predecessors |
	predecessors := OrderedCollection new.
	self allSequences do: [:each | (each successorSequences includes: aSequence) ifTrue: [predecessors add: each]].
	^predecessors
]

{ #category : 'accessing' }
OCIRMethod >> properties [
	^properties
]

{ #category : 'accessing' }
OCIRMethod >> properties: propDict [
	properties := propDict
]

{ #category : 'optimizing' }
OCIRMethod >> removeEmptyStart [

	 (startSequence size = 1) ifTrue: [
        "startSeq is just unconditional jump, forget it"
        startSequence := startSequence last destination]
]

{ #category : 'accessing' }
OCIRMethod >> sourceInterval [
	^self sourceNode sourceInterval
]

{ #category : 'accessing' }
OCIRMethod >> sourceNode [
	^sourceNode
]

{ #category : 'accessing' }
OCIRMethod >> sourceNode: aNode [
	sourceNode := aNode
]

{ #category : 'accessing' }
OCIRMethod >> startSequence [

	^ startSequence
]

{ #category : 'initialize' }
OCIRMethod >> startSequence: irSequence [

	startSequence := irSequence.
	irSequence method: self
]

{ #category : 'accessing' }
OCIRMethod >> tempKeys [

	^ tempMap keys
]

{ #category : 'accessing' }
OCIRMethod >> tempMap [
	^ tempMap
]
