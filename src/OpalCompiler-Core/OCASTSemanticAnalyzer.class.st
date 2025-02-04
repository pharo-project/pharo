"
I visit each node in the abstract syntax tree while growing and shrinking a scope chain. Each method and block node is linked with its corresponding scope object, and each variable def and ref is linked with its corresponding OCVariable. Exceptions are raised for undefined variable references and so on (see subclasses of OCSemanticWarning).

"
Class {
	#name : 'OCASTSemanticAnalyzer',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'scope',
		'outerScope',
		'compilationContext',
		'blockCounter',
		'undeclared',
		'invalidVariables'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'api' }
OCASTSemanticAnalyzer >> analyze: aNode [

	self visitNode: aNode.
	OCASTClosureAnalyzer new visitNode: aNode.
	OCASTMethodMetadataAnalyser new visitNode: aNode
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> analyzeLocalVariableRead: aLocalVariable [
	aLocalVariable markRead.
	(aLocalVariable scope outerNotOptimizedScope ~= scope outerNotOptimizedScope ) ifFalse: [ ^self ].
	"only escaping when they will end up in different closures"
	aLocalVariable markEscapingRead.
	"if we read a variable in a loop that is a repeated write, it need to be marked as escaping write"
	(scope isInsideOptimizedLoop and: [aLocalVariable isRepeatedWrite])
				ifTrue: [aLocalVariable markEscapingWrite]
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> analyzeLocalVariableWrite: aLocalVariable [
	(aLocalVariable scope outerNotOptimizedScope ~= scope outerNotOptimizedScope)
	"only escaping when they will end up in different closures"
			ifTrue: [ aLocalVariable markEscapingWrite].
	"if we write a variable in a loop, mark it as a repeated Write"
	scope isInsideOptimizedLoop
					ifTrue: [ aLocalVariable markRepeatedWrite ]
					ifFalse: [ aLocalVariable markWrite ]
]

{ #category : 'accessing' }
OCASTSemanticAnalyzer >> blockCounter [
	^blockCounter ifNil: [0]
]

{ #category : 'accessing' }
OCASTSemanticAnalyzer >> compilationContext [
	^ compilationContext
]

{ #category : 'accessing' }
OCASTSemanticAnalyzer >> compilationContext: aCompilationContext [
	compilationContext := aCompilationContext
]

{ #category : 'variables' }
OCASTSemanticAnalyzer >> declareArgumentNode: aVariableNode [
	^self declareVariableNode: aVariableNode as: (ArgumentVariable named: aVariableNode name)
]

{ #category : 'variables' }
OCASTSemanticAnalyzer >> declareInvalidVariableNode: aVariableNode [

	| var |
	var := self invalidVariables
		       at: aVariableNode name
		       ifAbsentPut: [ InvalidVariable named: aVariableNode name ].

	"Do not use `declareVariableNode:as:` as we dont care about shadowing"
	aVariableNode binding: var
]

{ #category : 'variables' }
OCASTSemanticAnalyzer >> declareTemporaryNode: aVariableNode [
	^self declareVariableNode: aVariableNode as: (TemporaryVariable named: aVariableNode name)
]

{ #category : 'variables' }
OCASTSemanticAnalyzer >> declareVariableNode: aVariableNode as: anOCTempVariable [
	| name var shadowing |
	name := aVariableNode name.
	"check if another variable with same name is visible"
	shadowing := scope lookupVar: name.
	var := scope addTemp: anOCTempVariable.
	aVariableNode binding: var.
	(shadowing isNotNil and: [ shadowing allowsShadowing not]) ifTrue: [self shadowing: shadowing withVariable: aVariableNode].
	^ var
]

{ #category : 'accessing' }
OCASTSemanticAnalyzer >> invalidVariables [
	^ invalidVariables ifNil: [ invalidVariables := Dictionary new ]
]

{ #category : 'accessing' }
OCASTSemanticAnalyzer >> outerScope [

	^ outerScope
]

{ #category : 'accessing' }
OCASTSemanticAnalyzer >> outerScope: anObject [

	outerScope := anObject
]

{ #category : 'variables' }
OCASTSemanticAnalyzer >> resolveVariableNode: aVariableNode [

	| var |
	var := (scope lookupVar: aVariableNode name) ifNil: [
		       (self compilationContext environment bindingOf:
			        aVariableNode name) ifNil: [
			       self undeclaredVariable: aVariableNode ] ].
	aVariableNode variable: var.
	^ var
]

{ #category : 'initialization' }
OCASTSemanticAnalyzer >> scope: aSemScope [
	scope := aSemScope
]

{ #category : 'error handling' }
OCASTSemanticAnalyzer >> shadowing: var withVariable: aNode [

	aNode addWarning: 'Name already defined'
]

{ #category : 'error handling' }
OCASTSemanticAnalyzer >> storeIntoReadOnlyVariable: variableNode [

	variableNode addError: 'Assignment to read-only variable'
]

{ #category : 'accessing' }
OCASTSemanticAnalyzer >> undeclared [

	^ undeclared ifNil: [ undeclared := Dictionary new ]
]

{ #category : 'error handling' }
OCASTSemanticAnalyzer >> undeclaredVariable: variableNode [

	| varName var notice |
	varName := variableNode name asSymbol.

	"If a invalid variable exists, we use it witout warning"
	self invalidVariables at: varName ifPresent: [ :v | ^ v ].

	notice := OCUndeclaredVariableNotice new messageText: 'Undeclared variable'.
	variableNode addNotice: notice.
	"If a registered undeclared variable exists, use it. Otherwise create an unregistered one."
	"It will be registered only at backend when a CompiledMethod is produced.
	Or never if compilation is aborted or if only frontend is requested."
	var := self undeclared
		       at: varName
		       ifAbsentPut: [
		       UndeclaredVariable possiblyRegisteredWithName: varName ].
	^ var
]

{ #category : 'error handling' }
OCASTSemanticAnalyzer >> uninitializedVariable: variableNode [
	variableNode addWarning: 'Unitialized variable'
]

{ #category : 'error handling' }
OCASTSemanticAnalyzer >> unusedVariable: variableNode [

	variableNode addWarning: 'Unused variable'
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitAnnotationMarkNode: aRBAnnotationValueNode [

	(aRBAnnotationValueNode parent isNotNil and: [
		 aRBAnnotationValueNode parent isMessage ]) ifTrue: [
		aRBAnnotationValueNode isHandled ifFalse: [
			aRBAnnotationValueNode parent addError:  'Unknown annotation' ].
		^ self ].
	aRBAnnotationValueNode addError: 'Unexpected token'
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitAssignmentNode: anAssignmentNode [
	| var |
	self visitNode: anAssignmentNode value.

	var := self resolveVariableNode: anAssignmentNode variable.
	var analyzeWrite: anAssignmentNode variable by: self
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitBlockNode: aBlockNode [
	aBlockNode arguments size >15 ifTrue: [ aBlockNode addError: 'Too many arguments' ].

	blockCounter := self blockCounter + 1.

	aBlockNode isInlined ifTrue: [^ self visitInlinedBlockNode: aBlockNode ].
	scope := scope newBlockScope: blockCounter.
	aBlockNode scope: scope. scope node: aBlockNode.

	aBlockNode arguments do: [:node | self declareArgumentNode: node ].
	self visitNode: aBlockNode body.
	scope := scope popScope
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitEnglobingErrorNode: aNode [
	"There is bad arguments or variable, just invalidate them to avoid noisy useless errors/warnings.
	Because, AST is very wrong with possible missing or superfluous [ or ], we should not believe in scope.
	The only thing that seems reasonable is that for now, we invalid any unexpected future variable with the same name"

	aNode arguments do: [ :node | self declareInvalidVariableNode: node ].
	aNode temporaries do: [ :node | self declareInvalidVariableNode: node ].

	^ super visitEnglobingErrorNode: aNode
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitInlinedBlockNode: aBlockNode [

	scope := scope newOptimizedBlockScope: blockCounter.
	aBlockNode isInlinedLoop ifTrue: [scope markInlinedLoop].
	aBlockNode scope: scope. scope node: aBlockNode.
	aBlockNode arguments do: [:node | self declareArgumentNode: node ].
	self visitNode: aBlockNode body.
	scope := scope popScope
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitMessageNode: aMessageNode [
	
	super visitMessageNode: aMessageNode.
	aMessageNode isSuperSend ifTrue: [ aMessageNode superOf: scope targetClass ]
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitMethodNode: aMethodNode [

	aMethodNode arguments size > 15 ifTrue: [ aMethodNode addError:  'Too many arguments' ].

	scope := OCMethodScope new outerScope: self outerScope.
	aMethodNode scope: scope.  scope node: aMethodNode.
	aMethodNode arguments do: [:node | self declareArgumentNode: node ].
	aMethodNode pragmas do: [:each | self visitNode: each].
	self visitNode: aMethodNode body
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitPragmaNode: aPragmaNode [

	| varNode |
	super visitPragmaNode: aPragmaNode.
	aPragmaNode pragma: aPragmaNode asPragma.
	aPragmaNode selector = #compilerOptions: ifTrue: [
		aPragmaNode pragma sendTo:
			self compilationContext ].

	"if the pragma is a primitive that defines an error variable, we need to declare a temp
	for it"
	aPragmaNode isPrimitiveError ifFalse: [ ^ self ].
	varNode := OCVariableNode named: aPragmaNode primitiveErrorVariableName.
	self declareVariableNode: varNode as: (PrimitiveErrorVariable node: varNode)
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitSequenceNode: aSequenceNode [

	aSequenceNode temporaries do: [ :node | self declareTemporaryNode: node ].
	aSequenceNode statements do: [ :each | self visitNode: each ].
	aSequenceNode temporaries reverseDo: [ :node |
			node variable isUsed
				ifFalse: [ self unusedVariable: node ] ]
]

{ #category : 'visiting' }
OCASTSemanticAnalyzer >> visitVariableNode: aVariableNode [
	| var |
	var := self resolveVariableNode: aVariableNode.
	var analyzeRead: aVariableNode by: self
]
