"
We need to analyse the #after hook: it is a block
"
Class {
	#name : #RFASTClosureAnalyzer,
	#superclass : #OCASTClosureAnalyzer,
	#category : #'Reflectivity-Compiler'
}

{ #category : #visiting }
RFASTClosureAnalyzer >> visitMethodNode: aMethodNode [
	"here look at the temps and make copying vars / tempVector out of them"
	self visitArgumentNodes: aMethodNode arguments.
	scope := aMethodNode scope.	
	self doRemotes; doCopying.
	self visitNode: aMethodNode body.
	"scope := scope popScope."
]

{ #category : #visiting }
RFASTClosureAnalyzer >> visitNode: aNode [
	super visitNode: aNode.
	
	aNode hasMetalinkBefore ifTrue: [  
 		aNode beforeHooks do: [:each | super visitNode: each]].
	
	aNode hasMetalinkAfter ifTrue: [  
 		aNode afterHooks do: [:each | super visitNode: each]].
]
