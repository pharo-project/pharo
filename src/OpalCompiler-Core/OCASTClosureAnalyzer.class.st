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
OCASTClosureAnalyzer >> doCopying [
	| copying |
	copying := scope tempVars select: [ :each | each isEscapingRead].	
	copying do: [ :each | scope addCopyingTemp: each]
]

{ #category : #variables }
OCASTClosureAnalyzer >> doRemotes [
	| remotes |
	remotes := scope tempVars select: [ :each | each isEscapingWrite].
	remotes do: [ :each | scope moveToVectorTemp: each].	
	
]

{ #category : #visiting }
OCASTClosureAnalyzer >> visitArgumentNode: aVariableNode [

	aVariableNode adaptToSemanticNode
]

{ #category : #visiting }
OCASTClosureAnalyzer >> visitBlockNode: aBlockNode [
	"here look at the temps and make copying vars / tempVector out of them"
	self visitArgumentNodes: aBlockNode arguments.
	scope := aBlockNode scope.	
	self doRemotes; doCopying.
	self visitNode: aBlockNode body.
	scope := scope popScope.
]

{ #category : #visiting }
OCASTClosureAnalyzer >> visitMethodNode: aMethodNode [
	"here look at the temps and make copying vars / tempVector out of them"
	self visitArgumentNodes: aMethodNode arguments.
	scope := aMethodNode scope.	
	self doRemotes; doCopying.
	self visitNode: aMethodNode body.
]

{ #category : #visiting }
OCASTClosureAnalyzer >> visitVariableNode: aVariableNode [
	"re-lookup the temorary variables..."
	
	| var |
	aVariableNode adaptToSemanticNode.
	aVariableNode isTemp ifFalse: [^self].
	var := scope lookupVar: aVariableNode name.
	aVariableNode binding: var.
	var isTempVectorTemp ifTrue: [scope addCopyingTempToAllScopesUpToDefVector: var vectorName].
	var isCopying ifTrue: [scope addCopyingTempToAllScopesUpToDefTemp: var].
]
