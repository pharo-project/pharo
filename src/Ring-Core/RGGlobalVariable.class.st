"
A global variable definition
"
Class {
	#name : 'RGGlobalVariable',
	#superclass : 'RGVariable',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'visiting' }
RGGlobalVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [

	^ aProgramNodeVisitor visitGlobalVariableNode: aNode
]

{ #category : 'managing container' }
RGGlobalVariable >> addoptToParentStub [

	super addoptToParentStub.
	self parent addGlobalVariable: self
]

{ #category : 'analyzing' }
RGGlobalVariable >> analyzeRead: aRBVariableNode by: anOCASTSemanticAnalyzer [
]

{ #category : 'managing container' }
RGGlobalVariable >> defaultParentStub [

	^ self defaultEnvironmentStub
]

{ #category : 'testing' }
RGGlobalVariable >> isGlobalVariable [

	^true
]

{ #category : 'testing' }
RGGlobalVariable >> isSuperVariable [

	^ false
]
