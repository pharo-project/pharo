"
RBDoItMethodNode is the node that represents AST of DoIt expressions.
It allows to distinguish between ASTs of real methods and DoIts. 

- aMethodNode isDoIt

"
Class {
	#name : 'RBDoItMethodNode',
	#superclass : 'RBMethodNode',
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'initialization' }
RBDoItMethodNode >> initialize [
	super initialize.

	selector := #DoIt
]

{ #category : 'testing' }
RBDoItMethodNode >> isDoIt [
	^true
]

{ #category : 'accessing' }
RBDoItMethodNode >> sourceCode [

	| interval |
	interval := self sourceInterval.
	interval ifEmpty: [ ^ '' ].

	^ self source
		copyFrom: interval first
		to: interval last
]

{ #category : 'accessing' }
RBDoItMethodNode >> start [
	^ body start
]

{ #category : 'accessing' }
RBDoItMethodNode >> stop [
	^ body stop
]
