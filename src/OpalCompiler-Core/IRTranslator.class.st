"
I visit IRMethod instructions, sending the appropriate bytecode messages to my BytecodeGenerator (gen). 
"
Class {
	#name : #IRTranslator,
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
IRTranslator class >> context: aCompilationContext trailer: aCompiledMethodTrailer [
	^self basicNew 
		initialize;
		compilationContext: aCompilationContext;
		trailer: aCompiledMethodTrailer
]

{ #category : #'instance creation' }
IRTranslator class >> new [
	^self context: CompilationContext default trailer: CompiledMethodTrailer empty
]

{ #category : #accessing }
IRTranslator >> compilationContext [
	^ compilationContext
]

{ #category : #accessing }
IRTranslator >> compilationContext: aContext [
	compilationContext := aContext.
	gen := compilationContext bytecodeGeneratorClass newWithEncoderClass: compilationContext encoderClass.
	gen compilationContext: aContext.
]

{ #category : #results }
IRTranslator >> compiledBlock [
	^ gen compiledBlockWith: trailer
]

{ #category : #results }
IRTranslator >> compiledMethod [
	^ gen compiledMethodWith: trailer
]

{ #category : #results }
IRTranslator >> compiledMethodWith: aTrailer [

	^ gen compiledMethodWith: aTrailer
]

{ #category : #private }
IRTranslator >> currentScope [ 
	^currentScope top
]

{ #category : #initialization }
IRTranslator >> initialize [
	currentScope := Stack new.
	tempVectorStack := Stack new.

]

{ #category : #private }
IRTranslator >> label: seqNum [

	gen label: seqNum.
	

]

{ #category : #private }
IRTranslator >> popScope [

	currentScope size = 1 ifFalse: [currentScope pop]

]

{ #category : #private }
IRTranslator >> pragmas: aCollection [
	gen pragmas: aCollection
]

{ #category : #results }
IRTranslator >> pushOuterVectors: scope [
	| scopesWithVector sc |
	scopesWithVector := OrderedCollection new.
	sc := scope.
	[ sc outerScope isNil or: [ sc outerScope isBehavior ] ] whileFalse: [ 
		sc := sc outerScope.
		sc hasTempVector ifTrue: [ scopesWithVector add: sc ] ].
	scopesWithVector reverseDo: [ :scopeWithVector |
		tempVectorStack push: (IRInstruction 
			createTempVectorNamed: scopeWithVector tempVectorName
			withVars: scopeWithVector tempVectorVarNames)].
	gen inBlock: true.
]

{ #category : #private }
IRTranslator >> pushScope: anIRBlockOrMethod [

	currentScope push: anIRBlockOrMethod
]

{ #category : #initialize }
IRTranslator >> trailer: aTrailer [

	trailer := aTrailer
]

{ #category : #visiting }
IRTranslator >> visitBlockReturnTop: blockReturnTop [

	gen blockReturnTop

]

{ #category : #visiting }
IRTranslator >> visitInstruction: instr [
	gen mapBytesTo: instr.
	self visitNode: instr.

]

{ #category : #visiting }
IRTranslator >> visitJump: jump [

	gen goto: jump destination orderNumber

]

{ #category : #visiting }
IRTranslator >> visitJumpIf: jumpIf [

	gen 
		if: jumpIf boolean 
		goto: jumpIf destination orderNumber 
		otherwise: jumpIf otherwise orderNumber

]

{ #category : #visiting }
IRTranslator >> visitMethod: anIr [
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
IRTranslator >> visitPop: pop [

	gen popTop


]

{ #category : #visiting }
IRTranslator >> visitPopIntoInstVar: instVar [
	gen storePopInstVar: instVar index
]

{ #category : #visiting }
IRTranslator >> visitPopIntoLiteralVariable: var [

	gen storePopIntoLiteralVariable: var association
]

{ #category : #visiting }
IRTranslator >> visitPopIntoRemoteTemp: remoteTemp [
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [ :each | each name = remoteTemp tempVectorName ].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen storePopRemoteTemp: tempVectorIndex inVectorAt: tempIndex
]

{ #category : #visiting }
IRTranslator >> visitPopIntoTemp: tmp [

   	gen storePopTemp: (self currentScope indexForVarNamed: tmp name)
]

{ #category : #visiting }
IRTranslator >> visitPushArray: array [

	array cons 
		ifTrue: [  
			gen pushConsArray: array size ]
		ifFalse: [
			gen pushNewArray: array size ]
]

{ #category : #visiting }
IRTranslator >> visitPushClosureCopy: closure [

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
IRTranslator >> visitPushDup: dup [
	
	gen pushDup



]

{ #category : #visiting }
IRTranslator >> visitPushFullClosure: fullBlock [

	fullBlock copiedValues do: [:name | 	
		gen pushTemp: (self currentScope indexForVarNamed: name)].
	gen mapBytesTo: fullBlock.
	gen pushFullBlockClosure: fullBlock

]

{ #category : #visiting }
IRTranslator >> visitPushInstVar: instVar [
	
	gen pushInstVar: instVar index

]

{ #category : #visiting }
IRTranslator >> visitPushLiteral: lit [
	| literal |
	literal := lit literal.
	"all objects that are not literals we keep writable"
	literal isLiteral ifFalse: [ ^ gen pushLiteral: literal ].
	"For symbol we need to set explicitly to writable or read-only. 
	 Compilation to read-only then back to writable keep the object 
	 read-only if we don't set it back to writable."
	compilationContext optionReadOnlyLiterals
		ifTrue: [ literal beReadOnlyObject ]
		ifFalse: [ literal beWritableObject ].
	^ gen pushLiteral: literal
	
]

{ #category : #visiting }
IRTranslator >> visitPushLiteralVariable: var [

	gen pushLiteralVariable: var association
]

{ #category : #visiting }
IRTranslator >> visitPushReceiver: receiver [

	gen pushReceiver



]

{ #category : #visiting }
IRTranslator >> visitPushRemoteTemp: remoteTemp [
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [:each | each name = remoteTemp tempVectorName].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen pushRemoteTemp: tempVectorIndex inVectorAt: tempIndex

]

{ #category : #visiting }
IRTranslator >> visitPushTemp: tmp [

	gen pushTemp: (self currentScope indexForVarNamed: tmp name)




]

{ #category : #visiting }
IRTranslator >> visitPushThisContext: ctxt [

	gen pushThisContext


]

{ #category : #visiting }
IRTranslator >> visitPushThisProcess: pushThisProcess [
	gen pushThisProcess
]

{ #category : #visiting }
IRTranslator >> visitReturn: ret [
	
	gen returnTop



]

{ #category : #visiting }
IRTranslator >> visitReturnInstVar: instVar [

	gen returnInstVar: instVar index
]

{ #category : #visiting }
IRTranslator >> visitReturnLiteral: lit [

	gen returnConstant: lit literal

]

{ #category : #visiting }
IRTranslator >> visitReturnReceiver: rec [

	gen returnReceiver



]

{ #category : #visiting }
IRTranslator >> visitSend: send [

	send superOf
		ifNil: [ gen send: send selector ]
		ifNotNil: [ :behavior |  gen send: send selector toSuperOf: behavior ]

]

{ #category : #visiting }
IRTranslator >> visitSequence: instructionSequence [
	self currentScope isPushClosureCopy ifTrue: [ 
		self currentScope destination = instructionSequence ifTrue: [ self popScope ] ].
	self label: instructionSequence orderNumber.
	super visitSequence: instructionSequence

]

{ #category : #visiting }
IRTranslator >> visitSequences:  irSequences [
	irSequences withIndexDo: [ :seq :i | seq orderNumber: i].
	self visitNodes: irSequences.

]

{ #category : #visiting }
IRTranslator >> visitStoreInstVar: instVar [
	gen storeInstVar: instVar index
]

{ #category : #visiting }
IRTranslator >> visitStoreLiteralVariable: var [

	gen storeIntoLiteralVariable: var association
]

{ #category : #visiting }
IRTranslator >> visitStoreRemoteTemp: remoteTemp [
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [:each | each name = remoteTemp tempVectorName].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen storeRemoteTemp: tempVectorIndex inVectorAt: tempIndex
]

{ #category : #visiting }
IRTranslator >> visitStoreTemp: tmp [

   	gen storeTemp: (self currentScope indexForVarNamed: tmp name)
]

{ #category : #visiting }
IRTranslator >> visitTempVector: tempVector [

	tempVectorStack push: tempVector.
	gen pushNewArray: tempVector vars size. 
	gen storePopTemp: (self currentScope indexForVarNamed: tempVector name)
]
