"
I visit an abstract syntax tree and generate IR (intermediate representation) instructions for each node by sending the appropriate message to my methodBuilder (an `IRBuilder`).
"
Class {
	#name : 'OCASTTranslator',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'methodBuilder',
		'nextUniqueInlineID'
	],
	#classVars : [
		'OptimizedMessages'
	],
	#category : 'OpalCompiler-Core-Translator',
	#package : 'OpalCompiler-Core',
	#tag : 'Translator'
}

{ #category : 'class initialization' }
OCASTTranslator class >> initialize [

	OptimizedMessages := {
		                     (#caseOf: -> #emitCaseOf:).
		                     (#caseOf:otherwise: -> #emitCaseOfOtherwise:).
		                     (#ifFalse: -> #emitIfFalse:).
		                     (#ifFalse:ifTrue: -> #emitIfFalseIfTrue:).
		                     (#ifNil: -> #emitIfNil:).
		                     (#ifNil:ifNotNil: -> #emitIfNilIfNotNil:).
		                     (#ifNotNil: -> #emitIfNotNil:).
		                     (#ifNotNil:ifNil: -> #emitIfNotNilIfNil:).
		                     (#ifTrue: -> #emitIfTrue:).
		                     (#ifTrue:ifFalse: -> #emitIfTrueIfFalse:).
		                     (#or: -> #emitOr:).
		                     (#and: -> #emitAnd:).
		                     (#timesRepeat: -> #emitTimesRepeat:).
		                     (#repeat -> #emitRepeat:).
		                     (#to:by:do: -> #emitToByDo:).
		                     (#to:do: -> #emitToDo:).
		                     (#whileFalse: -> #emitWhileFalse:).
		                     (#whileTrue: -> #emitWhileTrue:).
		                     (#whileFalse -> #emitWhileFalse:).
		                     (#whileTrue -> #emitWhileTrue:) } asDictionary
]

{ #category : 'accessing' }
OCASTTranslator >> compilationContext [
	^methodBuilder compilationContext
]

{ #category : 'accessing' }
OCASTTranslator >> compilationContext: anObject [
	methodBuilder compilationContext: anObject
]

{ #category : 'inline messages factored' }
OCASTTranslator >> emitAllButLastCases: cases [

	|  assocMessageNode  |

	1 to: cases size - 1 do: [:i |
		methodBuilder pushDup.
		assocMessageNode := cases at: i.
		self visitInlinedBlockNode: assocMessageNode receiver.
		methodBuilder send: #=.
		methodBuilder jumpAheadTo: #next if: false.
		methodBuilder popTop.
		self visitInlinedBlockNode: assocMessageNode arguments first.
		methodBuilder jumpAheadTo: #end.
		methodBuilder jumpAheadTarget: #next.
	]
]

{ #category : 'inline messages' }
OCASTTranslator >> emitAnd: aMessageNode [

	self visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #else if: false.
	self visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #else.
	methodBuilder pushLiteral: false.
	methodBuilder jumpAheadTarget: #end
]

{ #category : 'inline messages' }
OCASTTranslator >> emitCaseOf: aMessageNode [

	self
		emitCaseOf: aMessageNode
		otherwiseBlock: [
			methodBuilder pushReceiver.
			methodBuilder send: #caseError ]
]

{ #category : 'inline messages factored' }
OCASTTranslator >> emitCaseOf: aMessageNode otherwiseBlock: aBlock [

	| cases assocMessageNode  |

	cases := aMessageNode arguments first statements.
	self visitNode: aMessageNode receiver.
	self emitAllButLastCases: cases.

	"last case with otherwise"
	assocMessageNode := cases last.
	self visitInlinedBlockNode: assocMessageNode receiver.
	methodBuilder send: #=.
	methodBuilder jumpAheadTo: #next if: false.
	self visitInlinedBlockNode: assocMessageNode arguments first.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #next.
	aBlock value.

	cases size timesRepeat: [methodBuilder jumpAheadTarget: #end].

	aMessageNode lastIsReturn
		ifTrue: [
			(aMessageNode owningScope) isBlockScope
				ifTrue: [methodBuilder blockReturnTop]
				ifFalse: [methodBuilder returnTop]]
]

{ #category : 'inline messages' }
OCASTTranslator >> emitCaseOfOtherwise: aMessageNode [

	self
		emitCaseOf: aMessageNode
		otherwiseBlock: [ self visitInlinedBlockNode: aMessageNode arguments last ]
]

{ #category : 'inline messages factored' }
OCASTTranslator >> emitCondition: args boolean: aBoolean [
	"emits the jumps so that one of the 2 blocks in args is evaluated depending on boolean"

	methodBuilder jumpAheadTo: #else if: aBoolean.
	self visitInlinedBlockNode: args first.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #else.
	self visitInlinedBlockNode: args last.
	methodBuilder jumpAheadTarget: #end
]

{ #category : 'inline messages factored' }
OCASTTranslator >> emitIf: aMessageNode boolean: aBoolean [

	self visitNode: aMessageNode receiver.

	self emitCondition: aMessageNode arguments boolean: aBoolean
]

{ #category : 'inline messages' }
OCASTTranslator >> emitIfFalse: aMessageNode [

	self visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #false if: false.
		methodBuilder pushLiteral: nil.
		methodBuilder jumpAheadTo: #end.
		methodBuilder jumpAheadTarget: #false.
	self visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTarget: #end
]

{ #category : 'inline messages' }
OCASTTranslator >> emitIfFalseIfTrue: aMessageNode [

	self emitIf: aMessageNode boolean: true
]

{ #category : 'inline messages' }
OCASTTranslator >> emitIfNil: aMessageNode [

	self visitNode: aMessageNode receiver.
	methodBuilder pushDup.
	methodBuilder pushLiteral: nil.
	methodBuilder send: #==.
	methodBuilder jumpAheadTo: #else if: false.
	methodBuilder popTop.
	self visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTarget: #else
]

{ #category : 'inline messages factored' }
OCASTTranslator >> emitIfNil: aMessageNode boolean: aBoolean [
	| args notNilBlock |

	self visitNode: aMessageNode receiver.

	"emits the == nil code and push correct value on stack if the ifNotNil block has an argument"
	args := aMessageNode arguments.
	notNilBlock := aBoolean ifTrue: [args first] ifFalse: [args last].
	notNilBlock arguments ifNotEmpty: [ notNilBlock arguments first variable emitStore: methodBuilder ].
	methodBuilder pushLiteral: nil.
	methodBuilder send: #==.

	self emitCondition: args boolean: aBoolean
]

{ #category : 'inline messages' }
OCASTTranslator >> emitIfNilIfNotNil: aMessageNode [
	self emitIfNil: aMessageNode boolean: false
]

{ #category : 'inline messages' }
OCASTTranslator >> emitIfNotNil: aMessageNode [
	| args |
	self visitNode: aMessageNode receiver.
	args := aMessageNode arguments.
	args first arguments ifNotEmpty: [ args first arguments first variable emitStore: methodBuilder ].
	methodBuilder pushDup.
	methodBuilder pushLiteral: nil.
	methodBuilder send: #==.
	methodBuilder jumpAheadTo: #end if: true.
	methodBuilder popTop.
	self visitInlinedBlockNode: args first.
	methodBuilder jumpAheadTarget: #end
]

{ #category : 'inline messages' }
OCASTTranslator >> emitIfNotNilIfNil: aMessageNode [
	self emitIfNil: aMessageNode boolean: true
]

{ #category : 'inline messages' }
OCASTTranslator >> emitIfTrue: aMessageNode [

	self visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #else if: false.
	self visitInlinedBlockNode:  aMessageNode arguments first.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #else.
	methodBuilder pushLiteral: nil.
	methodBuilder jumpAheadTarget: #end
]

{ #category : 'inline messages' }
OCASTTranslator >> emitIfTrueIfFalse: aMessageNode [

	self emitIf: aMessageNode boolean: false
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> emitMessageNode: aMessageNode [

	aMessageNode isCascaded ifFalse: [
		self visitNode: aMessageNode receiver].
	aMessageNode arguments do: [:each |
		self visitNode: each].
	aMessageNode isSuperSend
		ifTrue: [methodBuilder send: aMessageNode selector toSuperOf: aMessageNode superOf ]
		ifFalse: [methodBuilder send: aMessageNode selector]
]

{ #category : 'inline messages' }
OCASTTranslator >> emitOr: aMessageNode [

	self visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #else if: false.
	methodBuilder pushLiteral: true.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #else.
	self visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTarget: #end
]

{ #category : 'inline messages' }
OCASTTranslator >> emitRepeat: aMessageNode [
	| block |
	block := aMessageNode receiver.
	methodBuilder jumpBackTarget: #start.
	self visitEffectInlinedBlockNode: block.
	methodBuilder jumpBackTo: #start.
	methodBuilder pushLiteral: nil
]

{ #category : 'errors' }
OCASTTranslator >> emitRuntimeError: aNode [
	"Runtime errors should only be emited on faulty code"

	| errors |
	errors := aNode allErrorNotices.

	methodBuilder
		pushLiteralVariable: OCRuntimeSyntaxError binding;
		pushLiteral: (errors isEmpty
				 ifTrue: [ 'Syntax error' ]
				 ifFalse: [ errors first messageText ]);
		send: #signalSyntaxError:
]

{ #category : 'inline messages' }
OCASTTranslator >> emitTimesRepeat: aMessageNode [

	| limit block limitEmit limitVariableName iteratorVariableName uniqueInlineID startLabelName doneLabelName |
	limit := aMessageNode receiver.
	block := aMessageNode arguments last.
	uniqueInlineID := self nextUniqueInlineID.
	limitVariableName := uniqueInlineID , #limit.
	iteratorVariableName := uniqueInlineID , #iterator.
	startLabelName := uniqueInlineID , #start.
	doneLabelName := uniqueInlineID , #done.

	limitEmit := [ self visitNode: limit ].
	"if the limit is not just a literal or a non-writable variable, make a temp store it there"
	(limit isLiteralNode or: [
		 limit isVariable and: [ limit variable isWritable not ] ])
		ifFalse: [
			self visitNode: limit.
			methodBuilder addTemp: limitVariableName.
			methodBuilder storeTemp: limitVariableName.
			methodBuilder popTop.
			limitEmit := [ methodBuilder pushTemp: limitVariableName ] ].

	"push start. allocate and initialize iterator"
	limitEmit value.
	methodBuilder pushLiteral: 1.
	methodBuilder addTemp: iteratorVariableName.
	methodBuilder storeTemp: iteratorVariableName.
	methodBuilder popTop.
	methodBuilder jumpBackTarget: startLabelName.
	methodBuilder pushTemp: iteratorVariableName.
	limitEmit value.
	methodBuilder send: #'<='.
	methodBuilder jumpAheadTo: doneLabelName if: false.

	self visitEffectInlinedBlockNode: block.
	methodBuilder pushTemp: iteratorVariableName.
	methodBuilder pushLiteral: 1.
	methodBuilder send: #+.
	methodBuilder storeTemp: iteratorVariableName.
	methodBuilder popTop.
	methodBuilder jumpBackTo: startLabelName.
	methodBuilder jumpAheadTarget: doneLabelName
]

{ #category : 'inline messages' }
OCASTTranslator >> emitToByDo: aMessageNode [

	| step |

	step := aMessageNode arguments second.
	step isLiteralNode ifFalse: [self error: 'should not have been inlined'].
	step := step value.

	self emitToDo: aMessageNode step: step
]

{ #category : 'inline messages' }
OCASTTranslator >> emitToDo: aMessageNode [

	self emitToDo: aMessageNode step: 1
]

{ #category : 'inline messages factored' }
OCASTTranslator >> emitToDo: aMessageNode step: step [
	| limit block iterator limitEmit |

	limit := aMessageNode arguments first.
	block := aMessageNode arguments last.
	iterator := block arguments first variable.

	limitEmit := [self visitNode: limit].
	"if the limit is not just a literal or a non-writable variable, make a temp store it there"
	(limit isLiteralNode or: [limit isVariable and: [limit variable isWritable not]]) ifFalse: [
		self visitNode: limit.
		methodBuilder addTemp: ('0limit_', iterator name).
		methodBuilder storeTemp: ('0limit_', iterator name).
		methodBuilder popTop.
		limitEmit := [methodBuilder pushTemp: ('0limit_', iterator name)]].

	"push start. allocate and initialize iterator"
	self visitNode: aMessageNode receiver.
	iterator emitStore: methodBuilder.
	methodBuilder jumpBackTarget: #start.
	iterator emitValue: methodBuilder.
	limitEmit value.
	methodBuilder send: (step > 0 ifTrue: [#<=] ifFalse: [#>=]).
	methodBuilder jumpAheadTo: #done if: false.

	self visitEffectInlinedBlockNode: block.
	iterator emitValue: methodBuilder.
	methodBuilder pushLiteral: step.
	methodBuilder send: #+.
	iterator emitStore: methodBuilder.
	methodBuilder popTop.
	methodBuilder jumpBackTo: #start.
	methodBuilder jumpAheadTarget: #done
]

{ #category : 'inline messages factored' }
OCASTTranslator >> emitWhile: aMessageNode boolean: aBoolean [

	methodBuilder jumpBackTarget: #begin.
	self visitInlinedBlockNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #end if: aBoolean.
	aMessageNode arguments ifNotEmpty: [
			self visitEffectInlinedBlockNode: aMessageNode arguments first].
	methodBuilder jumpBackTo: #begin.
	methodBuilder jumpAheadTarget: #end.
	methodBuilder pushLiteral: nil
]

{ #category : 'inline messages' }
OCASTTranslator >> emitWhileFalse: aMessageNode [

	self emitWhile: aMessageNode boolean: true
]

{ #category : 'inline messages' }
OCASTTranslator >> emitWhileTrue: aMessageNode [

	self emitWhile: aMessageNode boolean: false
]

{ #category : 'initialization' }
OCASTTranslator >> initialize [

	super initialize.
	
	methodBuilder := OCIRBuilder new.
	nextUniqueInlineID := 0
]

{ #category : 'accessing' }
OCASTTranslator >> ir [

	^ methodBuilder ir
]

{ #category : 'inline messages' }
OCASTTranslator >> nextUniqueInlineID [

	nextUniqueInlineID := nextUniqueInlineID + 1.
	^ '0', nextUniqueInlineID asString asSymbol
]

{ #category : 'private' }
OCASTTranslator >> privateMethodBuilder [
	^ methodBuilder
]

{ #category : 'private' }
OCASTTranslator >> subTranslator [
	"Return a new translator that can be used on blocks"

	^ self compilationContext astTranslatorClass new compilationContext: self compilationContext
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> translateConstantBlock: aBlockNode [
	| ir |
	"even though we never execute the compiledBlock, we create it so that the BlockClosure can use e.g. to query on the bytecode level"
	ir := OCIRBuilder new
				mapToNode: aBlockNode;
				numArgs: aBlockNode arguments size;
				pushLiteral: aBlockNode constantValue;
				blockReturnTop;
				ir.
	aBlockNode ir: ir.
	aBlockNode resetBcToASTCache.
	^ ir compiledBlock: aBlockNode scope
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> translateFullBlock: aBlockNode [

	methodBuilder mapToNode: aBlockNode.

	"args, then copied, then temps"
	methodBuilder addTemps: aBlockNode argumentNames.
	methodBuilder addTemps: aBlockNode scope inComingCopiedVarNames.
	methodBuilder addTemps: aBlockNode scope tempVarNamesWithoutArguments.
	methodBuilder numArgs: aBlockNode arguments size.

	aBlockNode scope tempVector ifNotEmpty: [
		methodBuilder
			createTempVectorNamed: aBlockNode scope tempVectorName
			withVars: aBlockNode scope tempVectorVarNames.
	].
	self visitNode: aBlockNode body.
	methodBuilder mapToNode: aBlockNode body.
	methodBuilder addBlockReturnTopIfRequired.
	aBlockNode ir: self ir.
	aBlockNode resetBcToASTCache.
	^ aBlockNode ir compiledBlock: aBlockNode scope
]

{ #category : 'visiting' }
OCASTTranslator >> visitAnnotationMarkNode: aAnnotationValueNode [

	self emitRuntimeError: aAnnotationValueNode
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitArrayNode: anArrayNode [

	| elementNodes |

	anArrayNode statements size > 32 ifTrue: [^ self visitLargeArrayNode: anArrayNode ].

	elementNodes := anArrayNode children.
	elementNodes do: [:node | self visitNode: node].
	methodBuilder pushConsArray: elementNodes size
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitAssignmentNode: anAssignmentNode [

	| var |
	var := anAssignmentNode variable variable.
	self visitNode: anAssignmentNode value.
	"Even in faulty mode, do not baspass the isWritable semantic to avoid very bad consequences on the image"
	var isWritable ifFalse: [ ^ self emitRuntimeError: anAssignmentNode variable ].

	"Invalid variable should fail"
	var isInvalidVariable ifTrue: [ ^ self emitRuntimeError: anAssignmentNode ].

	"Because we are producing a CompiledMethod, register undeclared variables to `Undeclared`"
	var isUndeclaredVariable ifTrue: [ var registerFromNode: anAssignmentNode ].

	var emitStore: methodBuilder
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitBlockNode: aBlockNode [
	| compiledBlock |
	aBlockNode isError ifTrue: [ ^ self emitRuntimeError: aBlockNode ].

	aBlockNode isInlined ifTrue: [^ self visitInlinedBlockNode: aBlockNode ].
	(self compilationContext optionConstantBlockClosure and: [aBlockNode isConstant and: [ aBlockNode numArgs < 4  ]]) ifTrue: [ ^ self visitConstantBlockNode: aBlockNode].
	(self compilationContext optionCleanBlockClosure and: [ aBlockNode isClean ]) ifTrue: [ ^ self visitCleanBlockNode: aBlockNode].

	compiledBlock := self subTranslator translateFullBlock: aBlockNode.

	self compilationContext optionBlockClosureOptionalOuter
		ifTrue: [ methodBuilder pushFullClosureCompiledBlock: compiledBlock copiedValues: aBlockNode scope inComingCopiedVarNames outerContextNeeded: aBlockNode hasNonLocalReturn  ]
		ifFalse: [ methodBuilder pushFullClosureCompiledBlock: compiledBlock copiedValues: aBlockNode scope inComingCopiedVarNames ]
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitCascadeNode: aCascadeNode [

	self visitNode: aCascadeNode receiver.
	aCascadeNode messages allButLastDo: [:node |
		methodBuilder pushDup.
		self visitEffectNode: node.
	].
	self visitNode: aCascadeNode messages last
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitCleanBlockNode: aBlockNode [
	"create and push a clean blockclosure. As we create the clean block at compile time,
	block creation at runtime is much faster than that of a full block"

	| compiledBlock cleanBlock |

	compiledBlock := self subTranslator translateFullBlock: aBlockNode.
	cleanBlock := CleanBlockClosure compiledBlock: compiledBlock.

	methodBuilder pushLiteral: cleanBlock
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitConstantBlockNode: anBlockNode [
	"create statically a constant blockclosure (we support 0-3 arguments).
	Constant blockclosures are specialized clean blocks: same creation speed, but faster execution"
	| constantBlock compiledBlock |
	constantBlock := ConstantBlockClosure
							  numArgs: anBlockNode numArgs
		                 literal: anBlockNode constantValue.
	compiledBlock := self translateConstantBlock: anBlockNode.
	constantBlock compiledBlock: compiledBlock.
	methodBuilder pushLiteral: constantBlock
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitEffectInlinedBlockNode: anOptimizedBlockNode [

	self visitInlinedBlockNode: anOptimizedBlockNode.
	"Pop the stack top in the context of anOptimizedBlockNode"
	methodBuilder mapToNode: anOptimizedBlockNode.
	methodBuilder popTop.
	methodBuilder popMap
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitEffectNode: aNode [

	self visitNode: aNode.
	"Pop the stack top in the context of aNode"
	methodBuilder mapToNode: aNode.
	methodBuilder popTop.
	methodBuilder popMap
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitEnglobingErrorNode: anErrorNode [
	"raise error at runtime"
	self visitParseErrorNode: anErrorNode
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitInlinedBlockNode: anOptimizedBlockNode [

	"We are visiting a scope that is not a block, but inlined in the outer context.
	This means:
			- we do not create a Block
			- we call IRBuilder to add temps
	"

	methodBuilder mapToNode: anOptimizedBlockNode.
	anOptimizedBlockNode scope tempVector ifNotEmpty: [
		methodBuilder
			createTempVectorNamed: anOptimizedBlockNode scope tempVectorName
			withVars: anOptimizedBlockNode scope tempVectorVarNames.
	].
	methodBuilder addTemps: anOptimizedBlockNode scope tempVarNamesWithoutArguments.
	methodBuilder addTemps: anOptimizedBlockNode scope inComingCopiedVarNames.
	methodBuilder addTemps: anOptimizedBlockNode argumentNames.
	anOptimizedBlockNode isInlinedLoop ifTrue: [
		anOptimizedBlockNode scope tempVarNamesWithoutArguments do: [ :tempName |
			methodBuilder pushLiteral: nil.
			methodBuilder storeTemp: tempName.
			methodBuilder popTop.
		]].

	self visitNode: anOptimizedBlockNode body.
	methodBuilder popMap
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitLargeArrayNode: anArrayNode [
	"Long form: using at:put:"
	methodBuilder pushLiteralVariable: Array binding.
	methodBuilder pushLiteral: anArrayNode statements size.
	methodBuilder send: #new:.
	anArrayNode statements withIndexDo: [  :each :index |
		methodBuilder pushDup.
		methodBuilder pushLiteral: index.
		self visitNode: each.
		methodBuilder send: #at:put:.
		methodBuilder popTop.
	].
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitLiteralArrayNode: aLiteralArrayNode [

	methodBuilder pushLiteral: aLiteralArrayNode value
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitLiteralNode: aLiteralNode [

	methodBuilder pushLiteral: aLiteralNode value
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitMessageNode: aMessageNode [

	aMessageNode isAnnotation ifTrue: [
		^ aMessageNode receiver emitValue: methodBuilder ].

	aMessageNode isInlined ifTrue: [
		methodBuilder addLiteral: aMessageNode selector. "so searching for senders will work"
		^self
			perform: (OptimizedMessages at: aMessageNode selector)
			with: aMessageNode].
	^ self emitMessageNode: aMessageNode
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitMethodNode: aMethodNode [

	aMethodNode isError ifTrue: [self emitRuntimeError: aMethodNode ].

	methodBuilder addTemps: aMethodNode scope tempVarNames.

	methodBuilder properties: aMethodNode methodProperties.
	methodBuilder irPrimitive: aMethodNode primitiveFromPragma.
	aMethodNode pragmas do: [:each | self visitPragmaNode: each].
	methodBuilder numArgs: aMethodNode arguments size.

	aMethodNode scope tempVector ifNotEmpty: [
		methodBuilder
			createTempVectorNamed: aMethodNode scope tempVectorName
			withVars: aMethodNode scope tempVectorVarNames
	].

	aMethodNode body lastIsReturn
		ifTrue: [
			self visitEffectNode: aMethodNode body ]
		ifFalse: [ aMethodNode isDoIt
			ifTrue: [ 
				self visitNode: aMethodNode body.
				methodBuilder returnTop ]
			ifFalse: [ 
				self visitEffectNode: aMethodNode body.
				methodBuilder pushReceiver; returnTop ] ]
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitNode: aNode [
	methodBuilder mapToNode: aNode.
	super visitNode: aNode.
	methodBuilder popMap
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitParseErrorNode: anErrorNode [

	self emitRuntimeError: anErrorNode
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitPragmaNode: aPragmaNode [

	| var |
	aPragmaNode isParseError ifTrue: [ ^self ].
	methodBuilder addPragma: aPragmaNode pragma.

	"if the pragma is a primitive that defines an error variable, we need to store error value
	which is on the stack"
	aPragmaNode isPrimitiveError ifFalse: [ ^ self ].
	var := aPragmaNode methodNode scope lookupVar: aPragmaNode primitiveErrorVariableName.
	var emitStore: methodBuilder
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitReturnNode: aReturnNode [

	self visitNode: aReturnNode value.
	methodBuilder returnTop
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitSequenceNode: aSequenceNode [
	| statements |
	statements := aSequenceNode statements.
	statements ifEmpty: [
		methodBuilder pushLiteral: nil.
		^self].
	statements allButLastDo: [:each | self visitEffectNode: each].
	self visitNode: statements last
]

{ #category : 'visitor - double dispatching' }
OCASTTranslator >> visitVariableNode: aVariableNode [

	"Invalid variable should fail"
	aVariableNode variable isInvalidVariable ifTrue: [ ^ self emitRuntimeError: aVariableNode  ].

	"Because we are producing a CompiledMethod, register undeclared variables to `Undeclared`"
	aVariableNode variable isUndeclaredVariable ifTrue: [
		aVariableNode variable registerFromNode: aVariableNode  ].

	aVariableNode variable emitValue: methodBuilder
]
