"
RBDoItMethodNode is the node that represents AST of DoIt expressions.
It allows to distinguish between ASTs of real methods and DoIts. 

- aMethodNode isDoIt

"
Class {
	#name : 'ASTDoItMethodNode',
	#superclass : 'ASTMethodNode',
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'initialization' }
ASTDoItMethodNode >> initialize [
	super initialize.

	selector := #DoIt
]

{ #category : 'testing' }
ASTDoItMethodNode >> isDoIt [
	^true
]

{ #category : 'accessing' }
ASTDoItMethodNode >> sourceCode [

	| interval |
	interval := self sourceInterval.
	interval ifEmpty: [ ^ '' ].

	^ self source
		copyFrom: interval first
		to: interval last
]

{ #category : 'accessing' }
ASTDoItMethodNode >> start [
	^ body start
]

{ #category : 'accessing' }
ASTDoItMethodNode >> stop [
	^ body stop
]
