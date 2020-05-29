"
I am an strategy to represent the different behavior of a variable node.
"
Class {
	#name : #RBVariableKind,
	#superclass : #Object,
	#instVars : [
		'variable'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBVariableKind >> acceptVisitor: aRBVisitor [

	self subclassResponsibility
]

{ #category : #testing }
RBVariableKind >> isArgument [

	^ false
]

{ #category : #testing }
RBVariableKind >> isBlockVar [

	^ false
]

{ #category : #testing }
RBVariableKind >> isGlobal [
	
	^ false
]

{ #category : #testing }
RBVariableKind >> isInstance [ 

	^ false
]

{ #category : #testing }
RBVariableKind >> isSelf [
	
	^ false
]

{ #category : #testing }
RBVariableKind >> isSuper [
	
	^ false
]

{ #category : #testing }
RBVariableKind >> isTemp [
	
	^ false
]

{ #category : #testing }
RBVariableKind >> isThisContext [ 

	^ false
]

{ #category : #testing }
RBVariableKind >> isUndefined [
	
	^ false
]

{ #category : #accessing }
RBVariableKind >> variable [
	^ variable
]

{ #category : #accessing }
RBVariableKind >> variable: anObject [
	variable := anObject
]
