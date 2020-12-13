"
Visit any RBParseErrorNode in the syntax tree and evaluate a generic block
"
Class {
	#name : #RBParseErrorNodeVisitor,
	#superclass : #RBAbstractBlockVisitor,
	#category : #'AST-Core-Visitors'
}

{ #category : #visiting }
RBParseErrorNodeVisitor >> visitEnglobingErrorNode: aNode [
	self visitParseErrorNode: aNode
]

{ #category : #visiting }
RBParseErrorNodeVisitor >> visitParseErrorNode: aNode [
	self visitBlock value: aNode
]
