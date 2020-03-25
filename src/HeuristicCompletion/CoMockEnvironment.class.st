Class {
	#name : #CoMockEnvironment,
	#superclass : #Object,
	#instVars : [
		'globals'
	],
	#category : #'HeuristicCompletion-Tests'
}

{ #category : #accessing }
CoMockEnvironment >> globals [
	^ globals
]

{ #category : #accessing }
CoMockEnvironment >> globals: anObject [
	globals := anObject
]
