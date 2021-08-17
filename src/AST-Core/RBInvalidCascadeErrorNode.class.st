Class {
	#name : #RBInvalidCascadeErrorNode,
	#superclass : #RBEnglobingErrorNode,
	#category : #'AST-Core-Nodes - ErrorNodes'
}

{ #category : #accessing }
RBInvalidCascadeErrorNode >> cascade [
	
	^ content first
]
