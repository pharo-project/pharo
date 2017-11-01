"
A Renraku event with multiple critics.
"
Class {
	#name : #ReMultipleCritiquesEvent,
	#superclass : #ReCritiqueEvent,
	#instVars : [
		'critiques'
	],
	#category : 'Renraku-Announcements'
}

{ #category : #accessing }
ReMultipleCritiquesEvent >> critiques [
	^ critiques
]

{ #category : #accessing }
ReMultipleCritiquesEvent >> critiques: aCollection [
	critiques := aCollection
]
