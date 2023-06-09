Class {
	#name : #RBUnfinishedStatementErrorNode,
	#superclass : #RBEnglobingErrorNode,
	#category : #'AST-Core-Nodes - ErrorNodes'
}

{ #category : #testing }
RBUnfinishedStatementErrorNode >> isUnfinishedStatement [

	^ true
]

{ #category : #accessing }
RBUnfinishedStatementErrorNode >> statement [

	^ contents first
]
