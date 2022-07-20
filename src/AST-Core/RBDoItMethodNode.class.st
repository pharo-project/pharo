Class {
	#name : #RBDoItMethodNode,
	#superclass : #RBMethodNode,
	#category : #'AST-Core-Nodes'
}

{ #category : #initialization }
RBDoItMethodNode >> initialize [ 	
	super initialize.
	
	selector := #DoIt
]

{ #category : #testing }
RBDoItMethodNode >> isDoIt [ 
	^true
]
