"
Deprecated. The design to change the class of RBVariableNode during name analysis was not a good one.

 We now instead annotate all RBVariableNode intances with a Variable object (see the inherirance Hierachy of Variable).

 The visitor dispatches through the variable object now, calling the visit methods that are specialised for the kind of Variable
"
Class {
	#name : #RBSelfNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
}

{ #category : #'instance creation' }
RBSelfNode class >> new [
	^super new 
		named: 'self' start: 0;
		yourself.
]

{ #category : #visiting }
RBSelfNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitSelfNode: self
]

{ #category : #initialization }
RBSelfNode >> initialize [
  super initialize.
  variable := SelfVariable instance
]
