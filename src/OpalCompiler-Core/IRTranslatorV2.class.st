"
I visit IRMethod instructions, sending the appropriate bytecode messages to my BytecodeGenerator (gen). 
"
Class {
	#name : #IRTranslatorV2,
	#superclass : #IRVisitor,
	#instVars : [
		'gen',
		'currentScope',
		'trailer',
		'tempVectorStack',
		'compilationContext'
	],
	#category : #'OpalCompiler-Core-IR-Manipulation'
}

{ #category : #'instance creation' }
IRTranslatorV2 class >> context: aCompilationContext trailer: aCompiledMethodTrailer [
	^self basicNew 
		initialize;
		compilationContext: aCompilationContext;
		trailer: aCompiledMethodTrailer
]

{ #category : #'instance creation' }
IRTranslatorV2 class >> new [
	^self context: CompilationContext default trailer: CompiledMethodTrailer empty
]

{ #category : #accessing }
IRTranslatorV2 >> compilationContext [
	^ compilationContext
]

{ #category : #accessing }
IRTranslatorV2 >> compilationContext: aContext [
	compilationContext := aContext.
	gen := compilationContext bytecodeGeneratorClass newWithEncoderClass: compilationContext encoderClass.
	gen compilationContext: aContext.
]

{ #category : #results }
IRTranslatorV2 >> compiledBlock [
	^ gen compiledBlockWith: trailer
]

{ #category : #results }
IRTranslatorV2 >> compiledMethod [
	^ gen compiledMethodWith: trailer
]

{ #category : #results }
IRTranslatorV2 >> compiledMethodWith: aTrailer [

	^ gen compiledMethodWith: aTrailer
]

{ #category : #private }
IRTranslatorV2 >> currentScope [ 
	^currentScope top
]

{ #category : #initialization }
IRTranslatorV2 >> initialize [
	currentScope := Stack new.
	tempVectorStack := Stack new.

]

{ #category : #private }
IRTranslatorV2 >> label: seqNum [

	gen label: seqNum.
	

]

{ #category : #private }
IRTranslatorV2 >> popScope [

	currentScope size = 1 ifFalse: [currentScope pop]

]

{ #category : #private }
IRTranslatorV2 >> pragmas: aCollection [
	gen pragmas: aCollection
]

{ #category : #results }
IRTranslatorV2 >> pushOuterVectors: scope [
	| scopesWithVector sc |
	scopesWithVector := OrderedCollection new.
	sc := scope.
	[ sc outerScope isNil ] whileFalse: [ 
		sc := sc outerScope.
		sc hasTempVector ifTrue: [ scopesWithVector add: sc ] ].
	scopesWithVector reverseDo: [ :scopeWithVector |
		tempVectorStack push: (IRInstruction 
			createTempVectorNamed: scopeWithVector tempVectorName
			withVars: (scopeWithVector tempVector collect: [:each| each name]) asArray ) ].
	gen inBlock: true.
]

{ #category : #private }
IRTranslatorV2 >> pushScope: anIRBlockOrMethod [

	currentScope push: anIRBlockOrMethod
]

{ #category : #initialize }
IRTranslatorV2 >> trailer: aTrailer [

	trailer := aTrailer
]

{ #category : #visiting }
IRTranslatorV2 >> visitBlockReturnTop: blockReturnTop [

	gen blockReturnTop

]

{ #category : #visiting }
IRTranslatorV2 >> visitInstruction: instr [
	gen mapBytesTo: instr.
	self visitNode: instr.

]

{ #category : #visiting }
IRTranslatorV2 >> visitJump: jump [

	gen goto: jump destination orderNumber

]

{ #category : #visiting }
IRTranslatorV2 >> visitJumpIf: jumpIf [

	gen 
		if: jumpIf boolean 
		goto: jumpIf destination orderNumber 
		otherwise: jumpIf otherwise orderNumber

]

{ #category : #visiting }
IRTranslatorV2 >> visitMethod: anIr [
	IRFix new visitNode: anIr.
	self pushScope: anIr.
	gen irPrimitive: anIr irPrimitive.
	gen numArgs: anIr numArgs.
	gen properties: anIr properties.
	gen numTemps: (anIr tempMap size).
	gen additionalLiterals: anIr additionalLiterals.
	gen forceLongForm: anIr forceLongForm.
	self visitSequences: anIr allSequences.
]

{ #category : #visiting }
IRTranslatorV2 >> visitPop: pop [

	gen popTop


]

{ #category : #visiting }
IRTranslatorV2 >> visitPopIntoInstVar: instVar [
	gen storePopInstVar: instVar index
]

{ #category : #visiting }
IRTranslatorV2 >> visitPopIntoLiteralVariable: var [

	gen storePopIntoLiteralVariable: var association
]

{ #category : #visiting }
IRTranslatorV2 >> visitPopIntoRemoteTemp: remoteTemp [
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [ :each | each name = remoteTemp tempVectorName ].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen storePopRemoteTemp: tempVectorIndex inVectorAt: tempIndex
]

{ #category : #visiting }
IRTranslatorV2 >> visitPopIntoTemp: tmp [

   	gen storePopTemp: (self currentScope indexForVarNamed: tmp name)
]

{ #category : #visiting }
IRTranslatorV2 >> visitPushArray: array [

	array cons 
		ifTrue: [  
			gen pushConsArray: array size ]
		ifFalse: [
			gen pushNewArray: array size ]
]

{ #category : #visiting }
IRTranslatorV2 >> visitPushClosureCopy: closure [

	closure copiedValues do: [:name | 	
		gen pushTemp: (self currentScope indexForVarNamed: name).
	].
	gen mapBytesTo: closure.
	gen pushClosureCopyNumCopiedValues: closure copiedValues size 
			numArgs: closure arguments size 
			to: closure destination orderNumber.
	self pushScope: closure.
	gen pushNClosureTemps: closure definedTemps size


]

{ #category : #visiting }
IRTranslatorV2 >> visitPushDup: dup [
	
	gen pushDup



]

{ #category : #visiting }
IRTranslatorV2 >> visitPushFullClosure: fullBlock [

	fullBlock copiedValues do: [:name | 	
		gen pushTemp: (self currentScope indexForVarNamed: name)].
	gen mapBytesTo: fullBlock.
	gen pushFullBlockClosure: fullBlock

]

{ #category : #visiting }
IRTranslatorV2 >> visitPushInstVar: instVar [
	
	gen pushInstVar: instVar index

]

{ #category : #visiting }
IRTranslatorV2 >> visitPushLiteral: lit [
	| literal |
	literal := lit literal.
	"During bootstrap script 1, 1 class isImmediateClass answers false, so we 
	 cannot rely on literal class isImmediateClass here or it breaks the bootstrap."
	({Character.SmallInteger.SmallFloat64} includes: literal class) ifTrue: [ ^ gen pushLiteral: literal ].
	Smalltalk vm supportsWriteBarrier ifFalse: [ ^ gen pushLiteral: literal ].
	"For symbol we need to set explicitly to writable or read-only. 
	 Compilation to read-only then back to writable keep the object 
	 read-only if we don't set it back to writable."
	compilationContext optionReadOnlyLiterals
		ifTrue: [ literal beReadOnlyObject ]
		ifFalse: [ literal beWritableObject ].
	^ gen pushLiteral: literal
	
]

{ #category : #visiting }
IRTranslatorV2 >> visitPushLiteralVariable: var [

	gen pushLiteralVariable: var association
]

{ #category : #visiting }
IRTranslatorV2 >> visitPushReceiver: receiver [

	gen pushReceiver



]

{ #category : #visiting }
IRTranslatorV2 >> visitPushRemoteTemp: remoteTemp [
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [:each | each name = remoteTemp tempVectorName].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen pushRemoteTemp: tempVectorIndex inVectorAt: tempIndex

]

{ #category : #visiting }
IRTranslatorV2 >> visitPushTemp: tmp [

	gen pushTemp: (self currentScope indexForVarNamed: tmp name)




]

{ #category : #visiting }
IRTranslatorV2 >> visitPushThisContext: ctxt [

	gen pushThisContext


]

{ #category : #visiting }
IRTranslatorV2 >> visitReturn: ret [
	
	gen returnTop



]

{ #category : #visiting }
IRTranslatorV2 >> visitReturnInstVar: instVar [

	gen returnInstVar: instVar index
]

{ #category : #visiting }
IRTranslatorV2 >> visitReturnLiteral: lit [

	gen returnConstant: lit literal

]

{ #category : #visiting }
IRTranslatorV2 >> visitReturnReceiver: rec [

	gen returnReceiver



]

{ #category : #visiting }
IRTranslatorV2 >> visitSend: send [

	send superOf
		ifNil: [ gen send: send selector ]
		ifNotNil: [ :behavior |  gen send: send selector toSuperOf: behavior ]

]

{ #category : #visiting }
IRTranslatorV2 >> visitSequence: instructionSequence [
	self currentScope isPushClosureCopy ifTrue: [ 
		self currentScope destination = instructionSequence ifTrue: [ self popScope ] ].
	self label: instructionSequence orderNumber.
	super visitSequence: instructionSequence

]

{ #category : #visiting }
IRTranslatorV2 >> visitSequences:  irSequences [
	irSequences withIndexDo: [ :seq :i | seq orderNumber: i].
	self visitNodes: irSequences.

]

{ #category : #visiting }
IRTranslatorV2 >> visitStoreInstVar: instVar [
	gen storeInstVar: instVar index
]

{ #category : #visiting }
IRTranslatorV2 >> visitStoreLiteralVariable: var [

	gen storeIntoLiteralVariable: var association
]

{ #category : #visiting }
IRTranslatorV2 >> visitStoreRemoteTemp: remoteTemp [
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [:each | each name = remoteTemp tempVectorName].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen storeRemoteTemp: tempVectorIndex inVectorAt: tempIndex
]

{ #category : #visiting }
IRTranslatorV2 >> visitStoreTemp: tmp [

   	gen storeTemp: (self currentScope indexForVarNamed: tmp name)
]

{ #category : #visiting }
IRTranslatorV2 >> visitTempVector: tempVector [

	tempVectorStack push: tempVector.
	gen pushNewArray: tempVector vars size. 
	gen storePopTemp: (self currentScope indexForVarNamed: tempVector name)
]
