Class {
	#name : #ASTUnfinishedStatementErrorNode,
	#superclass : #ASTEnglobingErrorNode,
	#category : #'AST-Core-Nodes - ErrorNodes'
}

{ #category : #testing }
ASTUnfinishedStatementErrorNode >> isUnfinishedStatement [
	
	^ true
]

{ #category : #accessing }
ASTUnfinishedStatementErrorNode >> statement [
	
	^ content first
]
