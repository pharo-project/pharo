Class {
	#name : #MalHitsNode,
	#superclass : #MalNodeWithPreviousAndNext,
	#instVars : [
		'auth',
		'hub'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #accessing }
MalHitsNode >> auth [
	^ auth
]

{ #category : #accessing }
MalHitsNode >> auth: anObject [
	auth := anObject
]

{ #category : #accessing }
MalHitsNode >> hub [
	^ hub
]

{ #category : #accessing }
MalHitsNode >> hub: anObject [
	hub := anObject
]

{ #category : #accessing }
MalHitsNode >> label [

	^ 'H '
]

{ #category : #printing }
MalHitsNode >> printOn: aStream [
	aStream nextPut: $(.
	self model printOn: aStream.
	aStream space.
	self auth printOn: aStream showingDecimalPlaces: 2.
	aStream space.
	self hub printOn: aStream showingDecimalPlaces: 2.
	aStream nextPutAll: ')'.

]
