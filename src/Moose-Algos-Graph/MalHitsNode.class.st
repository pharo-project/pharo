Class {
	#name : #MalHitsNode,
	#superclass : #MalNodeWithPreviousAndNext,
	#instVars : [
		'auth',
		'hub'
	],
	#category : 'Moose-Algos-Graph'
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

{ #category : #printing }
MalHitsNode >> label [

	^ 'H '
]

{ #category : #printing }
MalHitsNode >> printString [

	^ 	'(',
		self model printString,
		' ',
		(self auth printShowingDecimalPlaces: 2),
		' ',
		(self hub printShowingDecimalPlaces: 2),
		') '
]
