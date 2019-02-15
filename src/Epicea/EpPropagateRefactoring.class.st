"
I represent a refactoring that consists in the propagation (i.e. the ""redo"") of another refactoring.
"
Class {
	#name : #EpPropagateRefactoring,
	#superclass : #EpRefactoring,
	#instVars : [
		'targetRefactoring'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpPropagateRefactoring class >> target: aTargetRefactoring [
	^ self basicNew
		initializeWith: aTargetRefactoring;
		yourself
]

{ #category : #converting }
EpPropagateRefactoring >> asRBRefactoring [
	^ EpRBPropagateRefactoring target: targetRefactoring asRBRefactoring
]

{ #category : #initialization }
EpPropagateRefactoring >> initializeWith: aTargetRefactoring [
	self initialize.
	
	targetRefactoring := aTargetRefactoring.
]

{ #category : #accessing }
EpPropagateRefactoring >> targetRefactoring [
	^ targetRefactoring
]
