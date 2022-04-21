Class {
	#name : #ASTInvalidCascadeErrorNode,
	#superclass : #ASTEnglobingErrorNode,
	#category : #'AST-Core-Nodes - ErrorNodes'
}

{ #category : #accessing }
ASTInvalidCascadeErrorNode >> cascade [
	
	^ content first
]
