"
Visit any RBProgrmNode in the syntax tree and evaluate a generic block
"
Class {
	#name : 'OCGenericNodeVisitor',
	#superclass : 'OCAbstractBlockVisitor',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'visiting' }
OCGenericNodeVisitor >> visitNode: aNode [
	super visitNode: aNode.
	self visitBlock value: aNode
]
