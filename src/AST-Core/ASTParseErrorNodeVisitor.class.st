"
Visit any RBParseErrorNode in the syntax tree and evaluate a generic block
"
Class {
	#name : 'ASTParseErrorNodeVisitor',
	#superclass : 'ASTAbstractBlockVisitor',
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'class initialization' }
ASTParseErrorNodeVisitor class >> initialize [

	self deprecatedAliases: { #ASTParseErrorNodeVisitor }
]

{ #category : 'visiting' }
ASTParseErrorNodeVisitor >> visitEnglobingErrorNode: aNode [
	self visitParseErrorNode: aNode
]

{ #category : 'visiting' }
ASTParseErrorNodeVisitor >> visitParseErrorNode: aNode [
	self visitBlock value: aNode
]
