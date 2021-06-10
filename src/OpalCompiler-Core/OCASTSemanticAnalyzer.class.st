"
I visit each node in the abstract syntax tree while growing and shrinking a scope chain. Each method and block node is linked with its corresponding scope object, and each variable def and ref is linked with its corresponding OCVariable. Exceptions are raised for undefined variable references and so on (see subclasses of OCSemanticWarning).

"
Class {
	#name : #OCASTSemanticAnalyzer,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'scope',
		'compilationContext',
		'blockCounter'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #api }
OCASTSemanticAnalyzer >> analyze: aNode [
	self visitNode: aNode.
	OCASTClosureAnalyzer new visitNode: aNode.
	OCASTMethodMetadataAnalyser new visitNode: aNode
]

{ #category : #visiting }
OCASTSemanticAnalyzer >> analyzeLocalVariableRead: aLocalVariable [
	aLocalVariable markRead.
	(aLocalVariable scope outerNotOptimizedScope ~= scope outerNotOptimizedScope ) ifFalse: [ ^self ].
	"only escaping when they will end up in different closures"
	aLocalVariable markEscapingRead.
	"if we read a variable in a loop that is a repeated write, it need to be marked as escaping write"	
	(scope isInsideOptimizedLoop and: [aLocalVariable isRepeatedWrite])
				ifTrue: [aLocalVariable markEscapingWrite]
]

{ #category : #visiting }
OCASTSemanticAnalyzer >> analyzeLocalVariableWrite: aLocalVariable [
	(aLocalVariable scope outerNotOptimizedScope ~= scope outerNotOptimizedScope) 
	"only escaping when they will end up in different closures"
			ifTrue: [ aLocalVariable markEscapingWrite].
	"if we write a variable in a loop, mark it as a repeated Write"	
	scope isInsideOptimizedLoop
					ifTrue: [ aLocalVariable markRepeatedWrite ]
					ifFalse: [ aLocalVariable markWrite ]
]

{ #category : #accessing }
OCASTSemanticAnalyzer >> blockCounter [
	^blockCounter ifNil: [0]
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
	^self declareVariableNode: aVariableNode as: (ArgumentVariable named: aVariableNode name) 
]

{ #category : #variables }
OCASTSemanticAnalyzer >> declareTemporaryNode: aVariableNode [
	^self declareVariableNode: aVariableNode as: (TemporaryVariable named: aVariableNode name)
]

{ #category : #variables }
OCASTSemanticAnalyzer >> declareVariableNode: aVariableNode as: anOCTempVariable [
	| name var shadowing |
	name := aVariableNode name. 
	var := scope lookupVarForDeclaration: name.
	"check if another variable with same name is visible"
	var ifNotNil: [ shadowing := var].
	var := scope addTemp: anOCTempVariable.
	aVariableNode binding: var.
	(shadowing notNil and: [ shadowing allowsShadowing not]) ifTrue: [self shadowingVariable: aVariableNode].
	^ var
]

{ #category : #variables }
OCASTSemanticAnalyzer >> resolveVariableNode: aVariableNode [
	| var |	
	var := (scope lookupVar: aVariableNode name) 
		ifNil: [ self undeclaredVariable: aVariableNode ].
	aVariableNode variable: var.
	^var
]

{ #category : #initialization }
OCASTSemanticAnalyzer >> scope: aSemScope [
	scope := aSemScope
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> shadowingVariable: aNode [
	compilationContext optionSkipSemanticWarnings ifTrue: [ ^aNode ].
	^ OCShadowVariableWarning new
		node: aNode;
		compilationContext: compilationContext;
		signal
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
OCASTSemanticAnalyzer >> storeIntoReservedVariable: variableNode [
	compilationContext optionSkipSemanticWarnings ifTrue: [ ^ self ].
	^ OCStoreIntoReservedVariableError new
		node: variableNode;
		compilationContext: compilationContext;
		messageText: 'Cannot store into';
		signal
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> undeclaredVariable: variableNode [
	compilationContext optionSkipSemanticWarnings
		ifTrue: [ ^UndeclaredVariable named: variableNode name asSymbol ].
	^ OCUndeclaredVariableWarning new
		node: variableNode;
		compilationContext: compilationContext;
		signal
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> uninitializedVariable: variableNode [
	variableNode propertyAt: #semanticWarning put: #unitialized.
]

{ #category : #'error handling' }
OCASTSemanticAnalyzer >> unusedVariable: variableNode [

	variableNode propertyAt: #semanticWarning put: 'unused variable'
]

{ #category : #visiting }
OCASTSemanticAnalyzer >> visitAssignmentNode: anAssignmentNode [
	| var |
	self visitNode: anAssignmentNode value.	
		
	var := self resolveVariableNode: anAssignmentNode variable.	
	var analyzeWrite: anAssignmentNode variable by: self
]

{ #category : #visiting }
OCASTSemanticAnalyzer >> visitBlockNode: aBlockNode [
	blockCounter := self blockCounter + 1.

	aBlockNode isInlined ifTrue: [^ self visitInlinedBlockNode: aBlockNode ].	
	scope := scope newBlockScope: blockCounter. 
	aBlockNode scope: scope. scope node: aBlockNode.
	
	aBlockNode arguments do: [:node | self declareArgumentNode: node ].
	self visitNode: aBlockNode body.
	scope := scope popScope.
]

{ #category : #visiting }
OCASTSemanticAnalyzer >> visitInlinedBlockNode: aBlockNode [

	scope := scope newOptimizedBlockScope: blockCounter.
	aBlockNode isInlinedLoop ifTrue: [scope markInlinedLoop]. 
	aBlockNode scope: scope. scope node: aBlockNode.
	aBlockNode arguments do: [:node | self declareArgumentNode: node ].
	self visitNode: aBlockNode body.
	scope := scope popScope.
]

{ #category : #visiting }
OCASTSemanticAnalyzer >> visitMethodNode: aMethodNode [

	scope := OCMethodScope new outerScope: compilationContext scope.
	aMethodNode scope: scope.  scope node: aMethodNode.
	aMethodNode arguments do: [:node | self declareArgumentNode: node ].
	aMethodNode pragmas do: [:each | self visitNode: each].
	self visitNode: aMethodNode body.

]

{ #category : #visiting }
OCASTSemanticAnalyzer >> visitPragmaNode: aPragmaNode [

	| varNode |
	super visitPragmaNode: aPragmaNode.
	aPragmaNode selector = #compilerOptions: ifTrue: [ 
		aPragmaNode asPragma sendTo:
			aPragmaNode methodNode compilationContext ].
	
	"if the pragma is a primitive that defines an error variable, we need to declare a temp
	for it"	
	aPragmaNode isPrimitiveError ifFalse: [ ^ self ].
	varNode := RBVariableNode named: aPragmaNode primitiveErrorVariableName.
	self declareVariableNode: varNode as: (PrimitiveErrorVariable node: varNode).
]

{ #category : #visiting }
OCASTSemanticAnalyzer >> visitSequenceNode: aSequenceNode [
	
	aSequenceNode temporaries do: [ :node | self declareTemporaryNode: node ].
	aSequenceNode statements do: [ :each | self visitNode: each ].
	aSequenceNode temporaries reverseDo: [ :node | 
			node variable isUsed
				ifFalse: [ self unusedVariable: node ] ]
]

{ #category : #visiting }
OCASTSemanticAnalyzer >> visitVariableNode: aVariableNode [
	| var |	
	var := self resolveVariableNode: aVariableNode.
	var analyzeRead: aVariableNode by: self
]
