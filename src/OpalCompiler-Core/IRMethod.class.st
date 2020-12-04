"
I am a method in the IR (intermediate representation) language consisting of IRInstructions grouped by IRSequence (basic block).  The IRSequences form a control graph (therefore I only have to hold onto the starting sequence).  #compiledMethod will convert me to a CompiledMethod.  #methodNode will convert me back to a parse tree.

"
Class {
	#name : #IRMethod,
	#superclass : #Object,
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
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #optimizing }
IRMethod >> absorbConstantConditionalJumps [

	startSequence absorbConstantConditionalJumps: IdentitySet new
]

{ #category : #optimizing }
IRMethod >> absorbJumpsToSingleInstrs [

	startSequence absorbJumpToSingleInstr: IdentitySet new
]

{ #category : #visiting }
IRMethod >> accept: aVisitor [
	^ aVisitor visitMethod: self
]

{ #category : #accessing }
IRMethod >> addAdditionalLiteral: aLiteral [
	additionalLiterals add: aLiteral
]

{ #category : #accessing }
IRMethod >> addAdditionalLiterals: literals [ 
	additionalLiterals :=  literals
]

{ #category : #inlining }
IRMethod >> addInstructionsAfter: aCollection [
	| returningSeqs  lastInstr |
	aCollection ifEmpty: [^self].
	returningSeqs := self allSequences select: [:each | each last isReturn].
	lastInstr := returningSeqs last last.
	lastInstr addInstructionsBefore: aCollection.
	
]

{ #category : #inlining }
IRMethod >> addInstructionsBefore: aCollection [

	(self startSequence nextSequence first) addInstructionsBefore: aCollection.
	
]

{ #category : #accessing }
IRMethod >> addPragma: aPragma [
	
	pragmas add: aPragma.
]

{ #category : #accessing }
IRMethod >> additionalLiterals [
	^additionalLiterals
]

{ #category : #enumerating }
IRMethod >> allInstructions [
	" return irNodes as a flat collection "

	| irInstructions |
	irInstructions := OrderedCollection new.
	startSequence withAllSuccessorsDo: [:seq | seq do: [:bc | irInstructions add: bc]].
	^irInstructions
]

{ #category : #enumerating }
IRMethod >> allInstructionsMatching: aBlock [
	" return irNodes as a flat collection "

	| irInstructions |
	irInstructions := OrderedCollection new.
	startSequence withAllSuccessorsDo: [:seq | seq do: [:bc | (aBlock value: bc) ifTrue: [irInstructions add: bc]]].
	^irInstructions
]

{ #category : #enumerating }
IRMethod >> allSequences [
	^ startSequence withAllSuccessors
]

{ #category : #enumerating }
IRMethod >> allTempAccessInstructions [
	^self allInstructionsMatching: [:bc | bc isTemp].
]

{ #category : #inspector }
IRMethod >> children [
	^self startSequence withAllSuccessors
]

{ #category : #accessing }
IRMethod >> compilationContext [
	^ compilationContext ifNil: [ 
		"only happens when decompiling or using stand-alone"
		compilationContext := CompilationContext default]
]

{ #category : #accessing }
IRMethod >> compilationContext: aCompilationContext [
	compilationContext := aCompilationContext
]

{ #category : #translating }
IRMethod >> compiledBlock: scope [

	^compiledMethod 
		ifNil: [self generateBlock: CompiledMethodTrailer empty withScope: scope ]
		ifNotNil: [compiledMethod]
]

{ #category : #translating }
IRMethod >> compiledMethod [

	^ self compiledMethodWith: CompiledMethodTrailer empty
]

{ #category : #translating }
IRMethod >> compiledMethodWith: trailer [

	^compiledMethod 
		ifNil: [self generate: trailer ]
		ifNotNil: [compiledMethod]
]

{ #category : #enumerating }
IRMethod >> firstInstructionMatching: aBlock [
	" return irNodes as a flat collection "

	startSequence withAllSuccessorsDo: [:seq | seq do: [:bc | (aBlock value: bc) ifTrue: [^bc]]].
	^nil
]

{ #category : #accessing }
IRMethod >> forceLongForm [
	compilationContext ifNil: [ ^ false ].
	^compilationContext optionLongIvarAccessBytecodes
		
]

{ #category : #translating }
IRMethod >> generate [
	^self generate: CompiledMethodTrailer empty
]

{ #category : #translating }
IRMethod >> generate: trailer [
	
	| irTranslator |
   irTranslator := IRTranslator context: compilationContext trailer: trailer.
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

{ #category : #translating }
IRMethod >> generateBlock: trailer withScope: scope [
	| irTranslator |
      irTranslator := IRTranslator context: compilationContext trailer: trailer.
	irTranslator 
		pushOuterVectors: scope;
		visitNode: self;
		pragmas: pragmas.
	compiledMethod := irTranslator compiledBlock.
	^ compiledMethod
]

{ #category : #scoping }
IRMethod >> indexForVarNamed: aName [

	^tempMap at: aName
]

{ #category : #initialization }
IRMethod >> initialize [

	irPrimitive := IRPrimitive null.
	tempMap := Dictionary new.
	pragmas := OrderedCollection new.
	additionalLiterals := OCLiteralSet new.
	numArgs := 0.
]

{ #category : #decompiling }
IRMethod >> instructionsForDecompiling [
	"return all instructions, but skip the block bodies, as the decompiler
	recurses over blocks"
	
	^startSequence instructionsForDecompiling allButFirst.
]

{ #category : #accessing }
IRMethod >> ir [
	^self.
]

{ #category : #accessing }
IRMethod >> irPrimitive [

	^ irPrimitive
]

{ #category : #accessing }
IRMethod >> irPrimitive: aPrimitiveNode [

	irPrimitive := aPrimitiveNode
]

{ #category : #testing }
IRMethod >> isPushClosureCopy [
	^false.
]

{ #category : #testing }
IRMethod >> isSend [
	^false.
]

{ #category : #printing }
IRMethod >> longPrintOn: stream [

	IRPrinter new
		stream: stream;
		visitNode: self
]

{ #category : #accessing }
IRMethod >> method [
	^self.
]

{ #category : #accessing }
IRMethod >> numArgs [

	^ numArgs
]

{ #category : #accessing }
IRMethod >> numArgs: anInteger [

	numArgs := anInteger
]

{ #category : #optimizing }
IRMethod >> optimize [
	self removeEmptyStart.
	self compilationContext optionOptimizeIR ifFalse: [^self].
	self absorbJumpsToSingleInstrs.
	
	"This last two optimizations works but the problem 
	when you absorb conditional jumps 
	is that and: / or: chained generate explicitly 
	constant conditional jumps to tip the JIt and the 
	decompiler that they are and: or: and not conditions/loops
	
	self absorbConstantConditionalJumps.
	self absorbJumpsToSingleInstrs"
]

{ #category : #accessing }
IRMethod >> predecessorsOf: aSequence [
	| predecessors |
	predecessors := OrderedCollection new.
	self allSequences do: [:each | (each successorSequences includes: aSequence) ifTrue: [predecessors add: each]].
	^predecessors
]

{ #category : #accessing }
IRMethod >> properties [
	^properties
]

{ #category : #accessing }
IRMethod >> properties: propDict [
	properties := propDict.
]

{ #category : #optimizing }
IRMethod >> removeEmptyStart [

	 (startSequence size = 1 and: [ startSequence last isPushClosureCopy not ]) ifTrue: [
        "startSeq is just unconditional jump, forget it"
        startSequence := startSequence last destination].

]

{ #category : #accessing }
IRMethod >> sourceInterval [
	^self sourceNode sourceInterval
]

{ #category : #accessing }
IRMethod >> sourceNode [
	^sourceNode
]

{ #category : #accessing }
IRMethod >> sourceNode: aNode [
	sourceNode := aNode
]

{ #category : #accessing }
IRMethod >> startSequence [

	^ startSequence
]

{ #category : #initialize }
IRMethod >> startSequence: irSequence [

	startSequence := irSequence.
	irSequence method: self.
]

{ #category : #accessing }
IRMethod >> tempKeys [

	^ tempMap keys
]

{ #category : #accessing }
IRMethod >> tempMap [
	^ tempMap
]
