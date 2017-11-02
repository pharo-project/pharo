"
A Renraku event with a single critic.
"
Class {
	#name : #ReSingleCritiqueEvent,
	#superclass : #ReCritiqueEvent,
	#instVars : [
		'critique'
	],
	#category : 'Renraku-Announcements'
}

{ #category : #accessing }
ReSingleCritiqueEvent >> critique [
	^ critique
]

{ #category : #accessing }
ReSingleCritiqueEvent >> critique: aCritique [
	critique := aCritique
]
