"
I lookaround is used for lookaheads and lookbehinds. They are used to check if the input matches a certain subexpression without consuming any characters (e.g. not advancing the match position).

Lookarounds can be positive or negative. If they are positive the condition fails if the subexpression fails, if they are negative it is inverse.
"
Class {
	#name : #RxsLookaround,
	#superclass : #RxsNode,
	#instVars : [
		'piece',
		'positive'
	],
	#category : #'Regex-Core-Nodes'
}

{ #category : #'instance creation' }
RxsLookaround class >> with: anRsxPiece [
	^ self new
		initializePiece: anRsxPiece
]

{ #category : #'initailize-release' }
RxsLookaround >> beNegative [
	positive := false
]

{ #category : #'initailize-release' }
RxsLookaround >> bePositive [
	positive := true
]

{ #category : #accessing }
RxsLookaround >> dispatchTo: aBuilder [
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."
	^aBuilder syntaxLookaround: self
]

{ #category : #'initailize-release' }
RxsLookaround >> initializePiece: anRsxPiece [
	super initialize.
	piece := anRsxPiece.
]

{ #category : #accessing }
RxsLookaround >> piece [
	^ piece
]
