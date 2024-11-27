"
Visit any ASTParseErrorNode in the syntax tree and evaluate a generic block
"
Class {
	#name : 'ASTParseErrorNodeVisitor',
	#superclass : 'ASTAbstractBlockVisitor',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'visiting' }
ASTParseErrorNodeVisitor >> visitEnglobingErrorNode: aNode [
	self visitParseErrorNode: aNode
]

{ #category : 'visiting' }
ASTParseErrorNodeVisitor >> visitParseErrorNode: aNode [
	self visitBlock value: aNode
]
