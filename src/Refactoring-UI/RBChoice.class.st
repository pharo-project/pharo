Class {
	#name : #RBChoice,
	#superclass : #Object,
	#instVars : [
		'refactoring'
	],
	#category : #'Refactoring-UI-Choices'
}

{ #category : #accessing }
RBChoice >> refactoring [

	^ refactoring
]

{ #category : #accessing }
RBChoice >> refactoring: anObject [

	refactoring := anObject
]
