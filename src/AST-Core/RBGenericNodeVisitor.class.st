"
Visit any RBProgrmNode in the syntax tree and evaluate a generic block
"
Class {
	#name : #RBGenericNodeVisitor,
	#superclass : #RBAbstractBlockVisitor,
	#category : #'AST-Core-Visitors'
}

{ #category : #accessing }
RBGenericNodeVisitor >> visitNode: aNode [
	super visitNode: aNode.
	self visitBlock value: aNode
]
