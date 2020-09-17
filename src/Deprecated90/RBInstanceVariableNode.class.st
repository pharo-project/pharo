"
Deprecated. The design to change the class of RBVariableNode during name analysis was not a good one.

We now instead annotate all RBVariableNode intances with a Variable object (see the inherirance Hierachy of Variable).

The visitor dispatches through the variable object now, calling the visit methods that are specialised for the kind of Variable
"
Class {
	#name : #RBInstanceVariableNode,
	#superclass : #RBVariableNode,
	#category : #'Deprecated90-AST'
}

{ #category : #visting }
RBInstanceVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitInstanceVariableNode: self
]
