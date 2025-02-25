"
Announced when a critic gets banned.
"
Class {
	#name : 'ReCritiqueBanned',
	#superclass : 'ReSingleCritiqueEvent',
	#instVars : [
		'entityBanLevel'
	],
	#category : 'Renraku-Announcements',
	#package : 'Renraku',
	#tag : 'Announcements'
}

{ #category : 'accessing' }
ReCritiqueBanned >> entityBanLevel [
	^ entityBanLevel
]

{ #category : 'accessing' }
ReCritiqueBanned >> entityBanLevel: anObject [
	entityBanLevel := anObject
]
