"
I am a visitor to find differents combinations of sequences nodes
"
Class {
	#name : #RBCombinatorVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'combinations'
	],
	#category : #'Refactoring-Core'
}

{ #category : #accessing }
RBCombinatorVisitor >> combinations [
	^ combinations
]

{ #category : #visiting }
RBCombinatorVisitor >> initialize [
	combinations := OrderedCollection new.
]

{ #category : #visiting }
RBCombinatorVisitor >> visitSequenceNode: aSequenceNode [
	| starts ends |
	starts := aSequenceNode statements collect: [ :node | node start ].
	ends := aSequenceNode statements collect: [ :node | node stop ].
	starts do: [ :start | ends do: [ :end | start < end ifTrue: [
		combinations add: (Array with: start with: end) ] ] ].
	super visitSequenceNode: aSequenceNode
	
	
]
