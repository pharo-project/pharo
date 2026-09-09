"
Visit any ASTParseErrorNode in the syntax tree and evaluate a generic block
"
Class {
	#name : 'OCParseErrorNodeVisitor',
	#superclass : 'OCAbstractBlockVisitor',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'visiting' }
OCParseErrorNodeVisitor >> visitEnglobingErrorNode: aNode [
	self visitParseErrorNode: aNode
]

{ #category : 'visiting' }
OCParseErrorNodeVisitor >> visitParseErrorNode: aNode [
	self visitBlock value: aNode
]
