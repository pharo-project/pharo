"
I am a mock environment. The tests use me to scope the environment to a known one.
"
Class {
	#name : #CoMockEnvironment,
	#superclass : #Object,
	#instVars : [
		'globals'
	],
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #accessing }
CoMockEnvironment >> globals [
	^ globals
]

{ #category : #accessing }
CoMockEnvironment >> globals: anObject [
	globals := anObject
]
