Class {
	#name : #GLMMenuInvoked,
	#superclass : #GLMActionAnnouncement,
	#instVars : [
		'menuMorph'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #'instance creation' }
GLMMenuInvoked class >> action: anAction on: aMenuMorph [ 
	^self new initializeAction: anAction on: aMenuMorph
]

{ #category : #initialization }
GLMMenuInvoked >> initializeAction: anAction on: aMenuMorph [ 
	self action: anAction.
	menuMorph := aMenuMorph
]

{ #category : #accessing }
GLMMenuInvoked >> menuMorph [
	^menuMorph
]
