Class {
	#name : 'OCUnfinishedStatementErrorNode',
	#superclass : 'OCEnglobingErrorNode',
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'testing' }
OCUnfinishedStatementErrorNode >> isUnfinishedStatement [

	^ true
]

{ #category : 'accessing' }
OCUnfinishedStatementErrorNode >> statement [

	^ contents first
]
