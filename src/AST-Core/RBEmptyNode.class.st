"
I am a node to represent nothing. 

I can be useful for Pharo rewriters to represent a rewrite that should nemove the matching nodes.
"
Class {
	#name : #RBEmptyNode,
	#superclass : #RBProgramNode,
	#instVars : [
		'start'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #accessing }
RBEmptyNode class >> at: aPosition [
	^ self new
		start: aPosition;
		yourself
]

{ #category : #visiting }
RBEmptyNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitEmptyNode: self
]

{ #category : #testing }
RBEmptyNode >> isEmptyNode [
	^ true
]

{ #category : #testing }
RBEmptyNode >> isFaulty [
	^ true
]

{ #category : #accessing }
RBEmptyNode >> start [
	^ start
]

{ #category : #accessing }
RBEmptyNode >> start: anObject [
	start := anObject
]

{ #category : #accessing }
RBEmptyNode >> stop [
	^ self start
]
