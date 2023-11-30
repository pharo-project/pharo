Class {
	#name : 'ASTUnfinishedStatementErrorNode',
	#superclass : 'ASTEnglobingErrorNode',
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'class initialization' }
ASTUnfinishedStatementErrorNode class >> initialize [

	self deprecatedAliases: { #ASTUnfinishedStatementErrorNode }
]

{ #category : 'testing' }
ASTUnfinishedStatementErrorNode >> isUnfinishedStatement [

	^ true
]

{ #category : 'accessing' }
ASTUnfinishedStatementErrorNode >> statement [

	^ contents first
]
