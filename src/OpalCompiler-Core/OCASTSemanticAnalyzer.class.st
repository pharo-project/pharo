"
I visit each node in the abstract syntax tree while growing and shrinking a scope chain. Each method and block node is linked with its corresponding scope object, and each variable def and ref is linked with its corresponding OCVariable. Exceptions are raised for undefined variable references and so on (see subclasses of OCSemanticWarning).

"
Class {
	#name : #OCASTSemanticAnalyzer,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'scope',
		'blockcounter',
		'compilationContext'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #api }
OCASTSemanticAnalyzer >> analyze: aNode [
	self visitNode: aNode.
	OCASTClosureAnalyzer new visitNode: aNode
]

{ #category : #accessing }
OCASTSemanticAnalyzer >> blockcounter [
	^blockcounter ifNil: [0]
]

{ #category : #accessing }
OCASTSemanticAnalyzer >> compilationContext [
	^ compilationContext
]

{ #category : #accessing }
OCASTSemanticAnalyzer >> compilationContext: aCompilationContext [
	compilationContext := aCompilationContext
]

{ #category : #variables }
OCASTSemanticAnalyzer >> declareArgumentNode: aVariableNode [
	^self declareVariableNode: aVariableNode as: OCArgumentVariable new
]

{ #category : #variables }
OCASTSemanticAnalyzer >> declareVariableNode: aVariableNode [
	^self declareVariableNode: aVariableNode as: OCTempVariable new
]

{ #category : #variables }
OCASTSemanticAnalyzer >> declareVariableNode: aVariableNode as: anOCTempVariable [
	| name var |
	name := aVariableNode name.
	var := scope lookupVarForDeclaration: name.
	var
		ifNotNil: [ 
			self variable: aVariableNode shadows: var.
			(var scope ~= scope)
				ifTrue: [ 
					"Create new var that shadows outer one"
					var := scope addTemp: anOCTempVariable withName: name ]
				]
		ifNil: [ 
			"new var"
			var := scope addTemp: anOCTempVariable withName: name ].
	aVariableNode binding: var.
	^ var
]

{ #category : #variables }
OCASTSemanticAnalyzer >> lookupVariableForRead: aVariableNode [

	| var |
	var := scope lookupVar: aVariableNode name.
	var ifNil: [^var].
	(var isTemp and: [var scope outerNotOptimizedScope ~= scope outerNotOptimizedScope] )  "only escaping when they will end up in different closures"
		ifTrue: [var markEscapingRead].
	^var
]

{ #category : #variables }
OCASTSemanticAnalyzer >> lookupVariableForWrite: aVariableNode [

	| var |
	
	var := scope lookupVar: aVariableNode name.

	var ifNil: [^var].
	var isSpecialVariable ifTrue: [ self storeIntoSpecialVariable: aVariableNode ].
	var isWritable ifFalse: [ self storeIntoReadOnlyVariable: aVariableNode ].
	
	var isTemp ifTrue: [
		(var scope outerNotOptimizedScope ~= scope outerNotOptimizedScope) "only escaping when they will end up in different closures"
			ifTrue: [ var markEscapingWrite]].
	^var
]

{ #category : #initialization }
OCASTSemanticAnalyzer >> scope: aSemScope [
	scope := aSemScope
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> storeIntoReadOnlyVariable: variableNode [
	compilationContext optionSkipSemanticWarnings ifTrue: [ ^ self ].
		
	^ OCStoreIntoReadOnlyVariableError new
		node: variableNode;
		compilationContext: compilationContext;
		messageText: 'Cannot store into';
		signal
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> storeIntoSpecialVariable: variableNode [
	compilationContext optionSkipSemanticWarnings ifTrue: [ ^ self ].
	^ OCStoreIntoSpecialVariableError new
		node: variableNode;
		compilationContext: compilationContext;
		messageText: 'Cannot store into';
		signal
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> undeclaredVariable: variableNode [
	compilationContext optionSkipSemanticWarnings
		ifTrue: [ ^OCUndeclaredVariable new name: variableNode name ].
	^ OCUndeclaredVariableWarning new
		node: variableNode;
		compilationContext: compilationContext;
		signal
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> uninitializedVariable: variableNode [
	variableNode binding markRead.
	variableNode propertyAt: #semanticWarning put: #unitialized.
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> unusedVariable: variableNode [

	variableNode propertyAt: #semanticWarning put: 'unused variable'
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> variable: variableNode shadows: semVar [
	compilationContext optionSkipSemanticWarnings ifTrue: [ ^semVar ].
	^ OCShadowVariableWarning new
		node: variableNode;
		shadowedVar: semVar;
		compilationContext: compilationContext;
		signal
]

{ #category : #visitor }
OCASTSemanticAnalyzer >> visitAssignmentNode: anAssignmentNode [
	| var |
	self visitNode: anAssignmentNode value.
	var := (self lookupVariableForWrite: anAssignmentNode variable)
		ifNil: [ self undeclaredVariable: anAssignmentNode variable ].
	scope isInsideOptimizedLoop
		ifTrue: [ var markRepeatedWrite ]
		ifFalse: [ var markWrite ].
	anAssignmentNode variable binding: var
]

{ #category : #visitor }
OCASTSemanticAnalyzer >> visitBlockNode: aBlockNode [
	blockcounter := self blockcounter + 1.

	aBlockNode isInlined ifTrue: [^ self visitInlinedBlockNode: aBlockNode ].	
	scope := scope newBlockScope: blockcounter. 
	aBlockNode scope: scope. scope node: aBlockNode.
	
	aBlockNode arguments do: [:node | self declareArgumentNode: node ].
	self visitNode: aBlockNode body.
	scope := scope popScope.
]

{ #category : #visitor }
OCASTSemanticAnalyzer >> visitInlinedBlockNode: aBlockNode [
	scope := scope newOptimizedBlockScope: blockcounter.
	aBlockNode isInlinedLoop ifTrue: [scope markInlinedLoop]. 
	aBlockNode scope: scope. scope node: aBlockNode.
	aBlockNode arguments do: [:node | self declareArgumentNode: node ].
	self visitNode: aBlockNode body.
	scope := scope popScope.
]

{ #category : #visitor }
OCASTSemanticAnalyzer >> visitMethodNode: aMethodNode [

	scope := compilationContext scope newMethodScope. 
	aMethodNode scope: scope.  scope node: aMethodNode.
	aMethodNode arguments do: [:node | self declareArgumentNode: node ].
	aMethodNode pragmas do: [:each | self visitNode: each].
	self visitNode: aMethodNode body.

]

{ #category : #visitor }
OCASTSemanticAnalyzer >> visitPragmaNode: aPragmaNode [
	super visitPragmaNode: aPragmaNode.
	(aPragmaNode selector = #compilerOptions:)
		ifTrue: [ aPragmaNode asPragma sendTo: aPragmaNode methodNode compilationContext ].
]

{ #category : #visitor }
OCASTSemanticAnalyzer >> visitSequenceNode: aSequenceNode [
	
	aSequenceNode temporaries do: [ :node | self declareVariableNode: node ].
	aSequenceNode parent isMethod
		ifTrue: [ 
			aSequenceNode parent pragmas
				detect: [ :pragma | pragma isPrimitiveError ]
				ifFound: [ :pragma | 
					self declareVariableNode: (RBVariableNode named: (pragma argumentAt: #error:) value asString) ] ].
	aSequenceNode statements do: [ :each | self visitNode: each ].
	aSequenceNode temporaries
		reverseDo: [ :node | 
			node binding isUnused
				ifTrue: [ self unusedVariable: node ] ]
]

{ #category : #visitor }
OCASTSemanticAnalyzer >> visitVariableNode: aVariableNode [
	| var |
	var := (self lookupVariableForRead: aVariableNode) ifNil: [(self undeclaredVariable: aVariableNode)].
	(var isTemp and: [var isEscaping and: [scope outerScope isInsideOptimizedLoop]])
		ifTrue: [
			 "only variables written within the loop needs to be marked as escaping write"
			var isRepeatedWrite ifTrue: [var markEscapingWrite].
			var isRead ifTrue: [var markEscapingRead]].
	aVariableNode binding: var.
	var isUninitialized ifTrue: [self uninitializedVariable: aVariableNode].
]
