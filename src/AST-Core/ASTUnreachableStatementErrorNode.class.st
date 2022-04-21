Class {
	#name : #ASTUnreachableStatementErrorNode,
	#superclass : #ASTEnglobingErrorNode,
	#category : #'AST-Core-Nodes - ErrorNodes'
}

{ #category : #visiting }
ASTUnreachableStatementErrorNode >> acceptVisitor: aVisitor [

	^ aVisitor visitUnreachableStatement: self
]
