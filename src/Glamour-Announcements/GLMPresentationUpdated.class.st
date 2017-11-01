"
A GLMPresentationUpdated is announced by the presentation when an update is wanted.
"
Class {
	#name : #GLMPresentationUpdated,
	#superclass : #Announcement,
	#instVars : [
		'presentation'
	],
	#category : 'Glamour-Announcements'
}

{ #category : #accessing }
GLMPresentationUpdated >> presentation [
	^ presentation
]

{ #category : #accessing }
GLMPresentationUpdated >> presentation: anObject [
	presentation := anObject
]
