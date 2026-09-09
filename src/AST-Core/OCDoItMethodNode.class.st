"
ASTDoItMethodNode is the node that represents AST of DoIt expressions.
It allows to distinguish between ASTs of real methods and DoIts. 

- aMethodNode isDoIt

"
Class {
	#name : 'OCDoItMethodNode',
	#superclass : 'OCMethodNode',
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'initialization' }
OCDoItMethodNode >> initialize [
	super initialize.

	selector := #DoIt
]

{ #category : 'testing' }
OCDoItMethodNode >> isDoIt [
	^true
]

{ #category : 'accessing' }
OCDoItMethodNode >> sourceCode [

	| interval |
	interval := self sourceInterval.
	interval ifEmpty: [ ^ '' ].

	^ self source
		copyFrom: interval first
		to: interval last
]

{ #category : 'accessing' }
OCDoItMethodNode >> start [
	^ body start
]

{ #category : 'accessing' }
OCDoItMethodNode >> stop [
	^ body stop
]
