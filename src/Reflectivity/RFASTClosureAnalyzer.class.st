"
We need to analyse the #after hook: it is a block
"
Class {
	#name : #RFASTClosureAnalyzer,
	#superclass : #OCASTClosureAnalyzer,
	#category : #'Reflectivity-Compiler'
}

{ #category : #visiting }
RFASTClosureAnalyzer >> visitNode: aNode [
	super visitNode: aNode.
	
	aNode hasMetalinkBefore ifTrue: [  
 		aNode beforeHooks do: [:each | super visitNode: each]].
	
	aNode hasMetalinkAfter ifTrue: [  
 		aNode afterHooks do: [:each | super visitNode: each]].
]
