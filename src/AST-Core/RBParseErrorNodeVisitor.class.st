"
Visit any RBParseErrorNode in the syntax tree and evaluate a generic block
"
Class {
	#name : #RBParseErrorNodeVisitor,
	#superclass : #RBAbstractBlockVisitor,
	#category : #'AST-Core-Visitors'
}

{ #category : #accessing }
RBParseErrorNodeVisitor >> visitEnglobingErrorNode: aNode [
	self visitParseErrorNode: aNode
]

{ #category : #accessing }
RBParseErrorNodeVisitor >> visitParseErrorNode: aNode [
	self visitBlock value: aNode
]

{ #category : #accessing }
RBParseErrorNodeVisitor >> visitUnreachableStatement: aNode [

	self visitEnglobingErrorNode: aNode
]
