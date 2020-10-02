Class {
	#name : #RBUnreachableStatementErrorNode,
	#superclass : #RBEnglobingErrorNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #visiting }
RBUnreachableStatementErrorNode >> acceptVisitor: aVisitor [

	^ aVisitor visitUnreachableStatement: self
]
