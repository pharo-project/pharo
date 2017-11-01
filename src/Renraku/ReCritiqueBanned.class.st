"
Announced when a critic gets banned.
"
Class {
	#name : #ReCritiqueBanned,
	#superclass : #ReSingleCritiqueEvent,
	#instVars : [
		'entityBanLevel'
	],
	#category : 'Renraku-Announcements'
}

{ #category : #accessing }
ReCritiqueBanned >> entityBanLevel [
	^ entityBanLevel
]

{ #category : #accessing }
ReCritiqueBanned >> entityBanLevel: anObject [
	entityBanLevel := anObject
]
