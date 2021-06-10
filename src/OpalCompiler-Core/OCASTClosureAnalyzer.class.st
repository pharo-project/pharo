"
I visit the AST of method a second time (after OCASTSemanticAnalyzer) to analyze temps related to closures.
"
Class {
	#name : #OCASTClosureAnalyzer,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'scope'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #variables }
OCASTClosureAnalyzer >> lookupAndFixBinding: aVariableNode [
	| var |
	var := scope lookupVar: aVariableNode name.
	aVariableNode variable: var.
	^var
]

{ #category : #visiting }
OCASTClosureAnalyzer >> visitArgumentNode: aVariableNode [
	"nothing to do for arguments"
]

{ #category : #visiting }
OCASTClosureAnalyzer >> visitBlockNode: aBlockNode [
	"here look at the temps and make copying vars / tempVector out of them"
	self visitArgumentNodes: aBlockNode arguments.
	scope := aBlockNode scope.	
	scope moveEscapingWritesToTempVector.
	scope copyEscapingReads.
	self visitNode: aBlockNode body.
	aBlockNode temporaries do: [ :each | self lookupAndFixBinding: each ].
	scope := scope popScope.
]

{ #category : #visiting }
OCASTClosureAnalyzer >> visitLocalVariableNode: aVariableNode [
	"re-lookup local variables..."
	| var |
	var := self lookupAndFixBinding: aVariableNode.
	var isTempVectorTemp ifTrue: [ | vectorVar |
		vectorVar := scope lookupVar: var vectorName.
		scope addCopyingTempToAllScopesUpToDefTemp: vectorVar].
	var isCopying ifTrue: [scope addCopyingTempToAllScopesUpToDefTemp: var].
]

{ #category : #visiting }
OCASTClosureAnalyzer >> visitMethodNode: aMethodNode [
	"here look at the temps and make copying vars / tempVector out of them"
	self visitArgumentNodes: aMethodNode arguments.
	scope := aMethodNode scope.	
	scope moveEscapingWritesToTempVector.
	scope copyEscapingReads.
	self visitNode: aMethodNode body.
	aMethodNode temporaries do: [ :each | self lookupAndFixBinding: each ]
]
