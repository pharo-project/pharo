"
I responsible to translate the IR instructions into bytecode. 
I delegate, however such a task to a BytecodeGenerator.
So I visit IRMethod instructions, sending the appropriate messages to my BytecodeGenerator (gen). 

Check `IRMethod>>#generate`.
"
Class {
	#name : 'OCIRTranslatorVisitor',
	#superclass : 'OCIRVisitor',
	#instVars : [
		'gen',
		'currentScope',
		'tempVectorStack',
		'compilationContext'
	],
	#category : 'OpalCompiler-Core-IR-Manipulation',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Manipulation'
}

{ #category : 'instance creation' }
OCIRTranslatorVisitor class >> context: aCompilationContext [
	^self basicNew
		initialize;
		compilationContext: aCompilationContext
]

{ #category : 'instance creation' }
OCIRTranslatorVisitor class >> new [
	^self context: OCCompilationContext default
]

{ #category : 'accessing' }
OCIRTranslatorVisitor >> compilationContext [
	^ compilationContext
]

{ #category : 'accessing' }
OCIRTranslatorVisitor >> compilationContext: aContext [
	compilationContext := aContext.
	gen := compilationContext bytecodeGeneratorClass newWithEncoderClass: compilationContext encoderClass.
	gen compilationContext: aContext
]

{ #category : 'results' }
OCIRTranslatorVisitor >> compiledBlock [
	^ gen compiledBlock
]

{ #category : 'results' }
OCIRTranslatorVisitor >> compiledMethod [
	^ gen compiledMethod
]

{ #category : 'private' }
OCIRTranslatorVisitor >> currentScope [
	^currentScope top
]

{ #category : 'initialization' }
OCIRTranslatorVisitor >> initialize [
	currentScope := Stack new.
	tempVectorStack := Stack new
]

{ #category : 'private' }
OCIRTranslatorVisitor >> label: seqNum [

	gen label: seqNum
]

{ #category : 'private' }
OCIRTranslatorVisitor >> popScope [

	currentScope size = 1 ifFalse: [currentScope pop]
]

{ #category : 'private' }
OCIRTranslatorVisitor >> pragmas: aCollection [
	gen pragmas: aCollection
]

{ #category : 'results' }
OCIRTranslatorVisitor >> pushOuterVectors: scope [
	| scopesWithVector sc |
	scopesWithVector := OrderedCollection new.
	sc := scope.
	[ sc outerScope isNil or: [ sc outerScope isBehavior ] ] whileFalse: [
		sc := sc outerScope.
		sc hasTempVector ifTrue: [ scopesWithVector add: sc ] ].
	scopesWithVector reverseDo: [ :scopeWithVector |
		tempVectorStack push: (OCIRInstruction
			createTempVectorNamed: scopeWithVector tempVectorName
			withVars: scopeWithVector tempVectorVarNames)].
	gen inBlock: true
]

{ #category : 'private' }
OCIRTranslatorVisitor >> pushScope: anIRBlockOrMethod [

	currentScope push: anIRBlockOrMethod
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitBlockReturnTop: blockReturnTop [

	gen blockReturnTop
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitInstruction: instr [
	gen mapBytesTo: instr.
	self visitNode: instr
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitJump: jump [

	gen goto: jump destination orderNumber
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitJumpIf: jumpIf [

	gen
		if: jumpIf boolean
		goto: jumpIf destination orderNumber
		otherwise: jumpIf otherwise orderNumber
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitMethod: anIr [
	OCIRSimpleOptimizerVisitor new visitNode: anIr.
	self pushScope: anIr.
	gen irPrimitive: anIr irPrimitive.
	gen numArgs: anIr numArgs.
	gen properties: anIr properties.
	gen numTemps: (anIr tempMap size).
	gen forceLongForm: self compilationContext optionLongIvarAccessBytecodes.
	self visitSequences: anIr allSequences.
	"Literals for sends and pushLiteral: are added later, here we
	add all the additionalLiterals from e.g. optimized constructs like #ifTrue:.
	that have been added with #addLiteral:.
	With the new bytecode set we have no constraints and can just add all of them."
	anIr additionalLiterals do: [ :each | gen addLiteral: each ]
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPop: pop [

	gen popTop
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPopIntoInstVar: instVar [
	gen storePopInstVar: instVar index
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPopIntoLiteralVariable: var [

	gen storePopIntoLiteralVariable: var association
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPopIntoRemoteTemp: remoteTemp [
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [ :each | each name = remoteTemp tempVectorName ].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen storePopRemoteTemp: tempVectorIndex inVectorAt: tempIndex
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPopIntoTemp: tmp [

   	gen storePopTemp: (self currentScope indexForVarNamed: tmp name)
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushArray: array [

	array cons
		ifTrue: [
			gen pushConsArray: array size ]
		ifFalse: [
			gen pushNewArray: array size ]
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushDup: dup [

	gen pushDup
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushFullClosure: fullBlock [

	fullBlock copiedValues do: [:name |
		gen pushTemp: (self currentScope indexForVarNamed: name)].
	gen mapBytesTo: fullBlock.
	gen pushFullBlockClosure: fullBlock
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushInstVar: instVar [

	gen pushInstVar: instVar index
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushLiteral: lit [
	^ gen pushLiteral: lit literal beReadOnlyLiteral
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushLiteralVariable: var [

	gen pushLiteralVariable: var association
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushReceiver: receiver [

	gen pushReceiver
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushRemoteTemp: remoteTemp [
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [:each | each name = remoteTemp tempVectorName].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen pushRemoteTemp: tempVectorIndex inVectorAt: tempIndex
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushTemp: tmp [

	gen pushTemp: (self currentScope indexForVarNamed: tmp name)
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushThisContext: ctxt [

	gen pushThisContext
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitPushThisProcess: pushThisProcess [
	gen pushThisProcess
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitReturn: ret [

	gen returnTop
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitReturnInstVar: instVar [

	gen returnInstVar: instVar index
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitReturnLiteral: lit [

	gen returnConstant: lit literal
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitReturnReceiver: rec [

	gen returnReceiver
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitSend: send [

	send superOf
		ifNil: [ gen send: send selector ]
		ifNotNil: [ :behavior |  gen send: send selector toSuperOf: behavior ]
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitSequence: instructionSequence [
	self label: instructionSequence orderNumber.
	super visitSequence: instructionSequence
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitSequences:  irSequences [
	irSequences withIndexDo: [ :seq :i | seq orderNumber: i].
	self visitNodes: irSequences
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitStoreInstVar: instVar [
	gen storeInstVar: instVar index
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitStoreLiteralVariable: var [

	gen storeIntoLiteralVariable: var association
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitStoreRemoteTemp: remoteTemp [
	| tempIndex tempVectorIndex tempVector |

	tempVector := tempVectorStack detect: [:each | each name = remoteTemp tempVectorName].
	tempVectorIndex := tempVector indexForVarNamed: remoteTemp name.
	tempIndex := self currentScope indexForVarNamed: remoteTemp tempVectorName.

	gen storeRemoteTemp: tempVectorIndex inVectorAt: tempIndex
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitStoreTemp: tmp [

   	gen storeTemp: (self currentScope indexForVarNamed: tmp name)
]

{ #category : 'visiting' }
OCIRTranslatorVisitor >> visitTempVector: tempVector [

	tempVectorStack push: tempVector.
	gen pushNewArray: tempVector vars size.
	gen storePopTemp: (self currentScope indexForVarNamed: tempVector name)
]
