"
I am the strategy for variables that are still not defined.
"
Class {
	#name : #RBUndefinedKind,
	#superclass : #RBVariableKind,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBUndefinedKind >> acceptVisitor: aProgramNodeVisitor [

	^ aProgramNodeVisitor visitVariableNode: variable
]

{ #category : #testing }
RBUndefinedKind >> isGlobal [ 
	
	^ variable binding isGlobal
]

{ #category : #testing }
RBUndefinedKind >> isInstance [ 

	^ variable binding isInstance
]

{ #category : #testing }
RBUndefinedKind >> isUndefined [

	^ true
]
