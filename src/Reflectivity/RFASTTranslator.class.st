"
subclass of Opal that takes links into account
"
Class {
	#name : 'RFASTTranslator',
	#superclass : 'OCASTTranslator',
	#category : 'Reflectivity-Compiler',
	#package : 'Reflectivity',
	#tag : 'Compiler'
}

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> emitMessageNode: aMessageNode [

	(aMessageNode isCascaded and: [(aMessageNode hasProperty: #hook) not])
		ifFalse: [ self visitNode: aMessageNode receiver ].
	aMessageNode arguments do: [ :each | self visitNode: each ].
	self emitPreamble: aMessageNode.
	self emitMetaLinkBefore: aMessageNode.
	aMessageNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aMessageNode ]
		ifFalse: [
			aMessageNode isSuperSend
				ifTrue: [ methodBuilder send: aMessageNode selector toSuperOf: aMessageNode superOf ]
				ifFalse: [ methodBuilder send: aMessageNode selector ] ].
	self emitMetaLinkAfterNoEnsure: aMessageNode
]

{ #category : 'reflectivity' }
RFASTTranslator >> emitMetaLinkAfterEnsure: aNode [
	| ensureBlock compiledBlock |

	"this saves the value from top of stack, executed [operation <here>] ensure: [ afterhook ] "
	aNode postambles do: [ :each | self visitNode: each ].
	ensureBlock := OCBlockNode body: (OCSequenceNode statements: aNode afterHooks).

	ensureBlock parent: aNode.
	ensureBlock scope: (aNode parent scope newBlockScope: 20).
	(aNode parent scope copiedVars, aNode parent scope tempVars) do: [ :var |
		ensureBlock scope addCopyingTempToAllScopesUpToDefTemp: var].

	compiledBlock := self subTranslator translateFullBlock: ensureBlock.
	methodBuilder pushFullClosureCompiledBlock: compiledBlock copiedValues: ensureBlock scope copiedVarNames.
	methodBuilder send: #ensure:
]

{ #category : 'reflectivity' }
RFASTTranslator >> emitMetaLinkAfterNoEnsure: aNode [

	aNode hasMetalinkAfter ifFalse: [ ^self ].
	aNode postambles do: [ :each | self visitNode: each ].
	aNode afterHooks do: [ :each | self visitEffectNode: each ]
]

{ #category : 'reflectivity' }
RFASTTranslator >> emitMetaLinkBefore: aNode [
	aNode hasMetalinkBefore ifFalse: [ ^self ].
	aNode beforeHooks do: [ :hook | self visitEffectNode: hook ]
]

{ #category : 'reflectivity' }
RFASTTranslator >> emitMetaLinkInstead: aNode [
	self visitNode: aNode insteadHooks
]

{ #category : 'reflectivity' }
RFASTTranslator >> emitPreamble: aNode [
	aNode preambles do: [ :pre | self visitNode: pre]
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitArrayNode: anArrayNode [

	| elementNodes |

	anArrayNode statements size > 32 ifTrue: [^ self visitLargeArrayNode: anArrayNode ].

	elementNodes := anArrayNode children.
	elementNodes do: [:node | self visitNode: node].
	self emitPreamble: anArrayNode.
	self emitMetaLinkBefore: anArrayNode.
	anArrayNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: anArrayNode ]
		ifFalse: [ methodBuilder pushConsArray: elementNodes size ].
	self emitMetaLinkAfterNoEnsure: anArrayNode
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitAssignmentNode: anAssignmentNode [
	self visitNode: anAssignmentNode value.
	self emitPreamble: anAssignmentNode.
	self emitMetaLinkBefore: anAssignmentNode.
	self emitPreamble: anAssignmentNode variable.
	self emitMetaLinkBefore: anAssignmentNode variable.

	anAssignmentNode hasMetalinkInstead
				ifTrue: [ self emitMetaLinkInstead: anAssignmentNode ]
				ifFalse: [
					 anAssignmentNode variable hasMetalinkInstead
						ifTrue: [ self emitMetaLinkInstead: anAssignmentNode variable]
						ifFalse: [ anAssignmentNode variable binding emitStore: methodBuilder]].
	self emitMetaLinkAfterNoEnsure: anAssignmentNode variable.
	self emitMetaLinkAfterNoEnsure: anAssignmentNode
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitBlockNode: aBlockNode [
	| compiledBlock |
	aBlockNode isError ifTrue: [self emitRuntimeError: aBlockNode ].
	aBlockNode isInlined ifTrue: [^ self visitInlinedBlockNode: aBlockNode ].

	self emitPreamble: aBlockNode.
	self emitMetaLinkBefore: aBlockNode.


	aBlockNode hasMetalinkInstead
				ifTrue: [ self emitMetaLinkInstead: aBlockNode ]
				ifFalse: [
	compiledBlock := self subTranslator translateFullBlock: aBlockNode.
	(self compilationContext optionCleanBlockClosure and: [ aBlockNode isClean ])
		ifTrue: [ methodBuilder pushLiteral: (CleanBlockClosure compiledBlock: compiledBlock)]
		ifFalse: [methodBuilder pushFullClosureCompiledBlock: compiledBlock copiedValues: aBlockNode scope inComingCopiedVarNames  ].
	].
	self emitMetaLinkAfterNoEnsure: aBlockNode
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitCascadeNode: aCascadeNode [
	self visitNode: aCascadeNode receiver.

	self emitPreamble: aCascadeNode.
	self emitMetaLinkBefore: aCascadeNode.
	aCascadeNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aCascadeNode ]
		ifFalse: [
			aCascadeNode messages allButLastDo: [:node |
				methodBuilder pushDup.
				self visitEffectNode: node.
			].
				self visitNode: aCascadeNode messages last.].
	self emitMetaLinkAfterNoEnsure: aCascadeNode
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitInlinedBlockNode: anOptimizedBlockNode [

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
	self emitPreamble: anOptimizedBlockNode.
	self emitMetaLinkBefore: anOptimizedBlockNode.
	self visitNode: anOptimizedBlockNode body.
	self emitMetaLinkAfterNoEnsure: anOptimizedBlockNode.
	methodBuilder popMap
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitLiteralArrayNode: aLiteralArrayNode [
	self emitPreamble: aLiteralArrayNode.
	self emitMetaLinkBefore: aLiteralArrayNode.
	aLiteralArrayNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aLiteralArrayNode ]
		ifFalse: [ methodBuilder pushLiteral: aLiteralArrayNode value ].
	self emitMetaLinkAfterNoEnsure: aLiteralArrayNode
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitLiteralNode: aLiteralNode [
	self emitPreamble: aLiteralNode.
	self emitMetaLinkBefore: aLiteralNode.
	aLiteralNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aLiteralNode ]
		ifFalse: [ methodBuilder pushLiteral: aLiteralNode value ].
	self emitMetaLinkAfterNoEnsure: aLiteralNode
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitMessageNode: aMessageNode [

	aMessageNode isInlined
		ifTrue: [
			methodBuilder addLiteral: aMessageNode selector. "so searching for senders will work"
			self emitPreamble: aMessageNode.
			self emitMetaLinkBefore: aMessageNode.
			aMessageNode hasMetalinkInstead
				ifTrue: [ self emitMetaLinkInstead: aMessageNode ]
				ifFalse: [
					self
						perform: (OptimizedMessages at: aMessageNode selector)
						with: aMessageNode ].
			self emitMetaLinkAfterNoEnsure: aMessageNode ]
		ifFalse: [ self emitMessageNode: aMessageNode ]
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitMethodNode: aMethodNode [
	"I ignore all links when I am primitive as ReflectiveMethod compiles a wrapper"

	aMethodNode isError ifTrue: [self emitRuntimeError: aMethodNode ].

	methodBuilder addTemps: aMethodNode scope tempVarNames.

	aMethodNode isPrimitive ifFalse: [self emitPreamble: aMethodNode. self emitMetaLinkBefore: aMethodNode].

	methodBuilder properties: aMethodNode methodProperties.
	methodBuilder irPrimitive: aMethodNode primitiveFromPragma.
	aMethodNode pragmas do: [:each | self visitPragmaNode: each].
	methodBuilder numArgs: aMethodNode arguments size.

	(aMethodNode hasMetalinkInstead and: [ aMethodNode isPrimitive not ])
				ifTrue: [ self emitMetaLinkInstead: aMethodNode. methodBuilder returnTop.
						^self. ].

	aMethodNode scope tempVector ifNotEmpty: [
		methodBuilder
			createTempVectorNamed: aMethodNode scope tempVectorName
			withVars: aMethodNode scope tempVectorVarNames.
	].
	aMethodNode body hasMetalinkInstead
		ifTrue: [
			"Apparently, body replacements expect that the result value of the replacement is return value of the method"
			self visitNode: aMethodNode body.
			methodBuilder returnTop ]
		ifFalse: [
			self visitEffectNode: aMethodNode body ].
	(aMethodNode hasProperty: #wrapperMethod)
	 ifTrue: [
		"after links are only active in the wrapper method"
		self emitMetaLinkAfterNoEnsure: aMethodNode.
		"the wrapper has to return the value of the wrapped method"
		methodBuilder pushTemp: #RFReifyValueVar; returnTop]
	ifFalse: [
		"Always add a return reveiver to be safe since the ones in the body could have been removed"
		methodBuilder pushReceiver ; returnTop ]
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitPragmaNode: aPragmaNode [

	self emitPreamble: aPragmaNode.
	self emitMetaLinkBefore: aPragmaNode.
	aPragmaNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aPragmaNode ]
		ifFalse: [super visitPragmaNode: aPragmaNode  ].
	self emitMetaLinkAfterNoEnsure: aPragmaNode
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitReturnNode: aReturnNode [

	self visitNode: aReturnNode value.
	self emitPreamble: aReturnNode.
	self emitMetaLinkBefore: aReturnNode.
	aReturnNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aReturnNode ]
		ifFalse: [ methodBuilder returnTop ]
]

{ #category : 'visiting' }
RFASTTranslator >> visitSequenceNode: aSequenceNode [
	| statements |

	self emitPreamble: aSequenceNode.
	self emitMetaLinkBefore: aSequenceNode.

	aSequenceNode hasMetalinkAfter ifTrue: [ ^ self visitSequenceWithAfter: aSequenceNode  ].

	aSequenceNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aSequenceNode ]
		ifFalse: [
			statements := aSequenceNode statements.
			statements ifEmpty: [
			methodBuilder pushLiteral: nil.
			^self].
		statements allButLastDo: [:each | self visitEffectNode: each].
		self visitNode: statements last.
	]
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitSequenceWithAfter: aSequenceNode [
	| wrappedBlock compiledBlock |
	wrappedBlock := OCBlockNode body: (OCSequenceNode statements: aSequenceNode statements).
	wrappedBlock parent: aSequenceNode.
	wrappedBlock scope: (aSequenceNode parent scope newBlockScope: 20).
	 (aSequenceNode parent scope copiedVars, aSequenceNode parent scope tempVars) do: [ :var |
		wrappedBlock scope addCopyingTempToAllScopesUpToDefTemp: var].

	compiledBlock := self subTranslator translateFullBlock: wrappedBlock.
	methodBuilder pushFullClosureCompiledBlock: compiledBlock copiedValues: wrappedBlock scope copiedVarNames.
	self emitMetaLinkAfterEnsure: aSequenceNode
]

{ #category : 'reflectivity' }
RFASTTranslator >> visitStoreIntoTempNode: aNode [
	"we store the TOS into the temp defined by the reification node"

	aNode binding emitStore: methodBuilder
]

{ #category : 'reflectivity' }
RFASTTranslator >> visitStorePopIntoTempNode: aNode [
	"we store the TOS into the temp defined by the reification node"

	aNode binding emitStore: methodBuilder.
	methodBuilder popTop
]

{ #category : 'visitor - double dispatching' }
RFASTTranslator >> visitVariableNode: aVariableNode [
	self emitPreamble: aVariableNode.
	self emitMetaLinkBefore: aVariableNode.
	aVariableNode hasMetalinkInstead
				ifTrue: [ self emitMetaLinkInstead: aVariableNode ]
				ifFalse: [ self visitVariableValue: aVariableNode binding].
	self emitMetaLinkAfterNoEnsure: aVariableNode
]

{ #category : 'reflectivity' }
RFASTTranslator >> visitVariableValue: aVariable [
	self emitPreamble: aVariable.
	self emitMetaLinkBefore: aVariable.
	aVariable hasMetalinkInstead
		ifTrue: [
			methodBuilder addLiteral: aVariable.
			self emitMetaLinkInstead: aVariable]
		ifFalse: [aVariable emitValue: methodBuilder].
	self emitMetaLinkAfterNoEnsure: aVariable
]
