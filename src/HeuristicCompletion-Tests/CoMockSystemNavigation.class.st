"
I am a mock environment. The tests use me to scope the environment to a known one.
"
Class {
	#name : #CoMockSystemNavigation,
	#superclass : #Object,
	#instVars : [
		'globals'
	],
	#category : #'HeuristicCompletion-Tests-Core'
}

{ #category : #accessing }
CoMockSystemNavigation >> allClassNamesStartingWith: aString do: aBlock [

	globals do: [ :e |
		(aString isEmpty or: [ e beginsWith: aString ])
			ifTrue: [ aBlock value: e ] ]
]

{ #category : #accessing }
CoMockSystemNavigation >> environment [

	^ globals
]

{ #category : #accessing }
CoMockSystemNavigation >> globals [
	^ globals
]

{ #category : #accessing }
CoMockSystemNavigation >> globals: anObject [
	globals := anObject
]
