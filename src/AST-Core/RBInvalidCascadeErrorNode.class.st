Class {
	#name : #RBInvalidCascadeErrorNode,
	#superclass : #RBEnglobingErrorNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #'as yet unclassified' }
RBInvalidCascadeErrorNode >> cascade [
	
	^ content first
]
