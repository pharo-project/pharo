"
Visit any ASTProgrmNode in the syntax tree and evaluate a generic block
"
Class {
	#name : #ASTGenericNodeVisitor,
	#superclass : #ASTAbstractBlockVisitor,
	#category : #'AST-Core-Visitors'
}

{ #category : #visiting }
ASTGenericNodeVisitor >> visitNode: aNode [
	super visitNode: aNode.
	self visitBlock value: aNode
]
