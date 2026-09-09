"
I visit the AST of method a second time (after OCASTSemanticAnalyzer) to analyze temps related to closures.
I annotate the AST to generate temp vector. 
"
Class {
	#name : 'OCASTClosureAnalyzer',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'scope',
		'currentBlockAccessesSelf',
		'currentBlockHasNonLocalReturn',
		'currentBlockHasIncomingVariables'
	],
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'variables' }
OCASTClosureAnalyzer >> lookupAndFixBinding: aVariableNode [
	| var |
	var := scope lookupVar: aVariableNode name.
	aVariableNode variable: var.
	^var
]

{ #category : 'visiting' }
OCASTClosureAnalyzer >> visitArgumentNode: aVariableNode [
	"nothing to do for arguments"
]

{ #category : 'visiting' }
OCASTClosureAnalyzer >> visitBlockNode: aBlockNode [
	"here look at the temps and make copying vars / tempVector out of them"
	
	"By default, block closures are clean.
	We now prove by checking the children that it is still the case.
	If not the case, we will mark it as not clean"
	| parentBlockAccessesSelf parentBlockHasIncomingVariables parentBlockHasNonLocalReturn |
	parentBlockAccessesSelf := currentBlockAccessesSelf.
	parentBlockHasIncomingVariables := currentBlockHasIncomingVariables.
	parentBlockHasNonLocalReturn := currentBlockHasNonLocalReturn.
	
	currentBlockAccessesSelf := false.
	currentBlockHasIncomingVariables := false.
	currentBlockHasNonLocalReturn := false.
	
	self visitArgumentNodes: aBlockNode arguments.
	scope := aBlockNode scope.
	scope moveEscapingWritesToTempVector.
	scope copyEscapingReads.
	self visitNode: aBlockNode body.
	aBlockNode temporaries do: [ :each | self lookupAndFixBinding: each ].
	
	scope hasEscapingVars ifTrue: [ currentBlockHasIncomingVariables := true ].
	scope isClean: (currentBlockHasIncomingVariables not and: [ currentBlockHasNonLocalReturn not and: [ currentBlockAccessesSelf not ] ]).

	"We come back to the parent scope.
	Restore the value of the analysis for the current scope"
	scope := scope popScope.
	currentBlockAccessesSelf := parentBlockAccessesSelf ifNotNil: [ parentBlockAccessesSelf or: [ currentBlockAccessesSelf ] ].
	currentBlockHasNonLocalReturn := parentBlockHasNonLocalReturn ifNotNil: [ parentBlockHasNonLocalReturn or: [ currentBlockHasNonLocalReturn ] ].
	currentBlockHasIncomingVariables := parentBlockHasIncomingVariables.
	
]

{ #category : 'visiting' }
OCASTClosureAnalyzer >> visitLocalVariableNode: aVariableNode [
	"re-lookup local variables..."
	| var |
	var := self lookupAndFixBinding: aVariableNode.
	var isTempVectorTemp ifTrue: [ | vectorVar |
		vectorVar := scope lookupVar: var vectorName.
		scope addCopyingTempToAllScopesUpToDefTemp: vectorVar].
	var isCopying ifTrue: [scope addCopyingTempToAllScopesUpToDefTemp: var].
	
	"Continue the iteration so we call other defined hooks"
	super visitLocalVariableNode: aVariableNode
]

{ #category : 'visiting' }
OCASTClosureAnalyzer >> visitMethodNode: aMethodNode [
	"here look at the temps and make copying vars / tempVector out of them"
	self visitArgumentNodes: aMethodNode arguments.
	scope := aMethodNode scope.
	scope moveEscapingWritesToTempVector.
	scope copyEscapingReads.
	self visitNode: aMethodNode body.
	aMethodNode temporaries do: [ :each | self lookupAndFixBinding: each ]
]

{ #category : 'visiting' }
OCASTClosureAnalyzer >> visitReturnNode: aReturnNode [

	currentBlockHasNonLocalReturn := true.
	^ super visitReturnNode: aReturnNode
]

{ #category : 'visiting' }
OCASTClosureAnalyzer >> visitVariableNode: aVariableNode [

	| accessesSelf variableBinding |
	variableBinding := aVariableNode variable.
	accessesSelf := variableBinding isInstanceVariable or: [ variableBinding isSelfVariable or: [ variableBinding isSuperVariable ] ].
	
	accessesSelf ifTrue: [ currentBlockAccessesSelf := true ].
	(variableBinding isLocalVariable and: [ variableBinding scope ~= scope ])
		ifTrue: [ currentBlockHasIncomingVariables := true ].

	^ super visitVariableNode: aVariableNode
]
