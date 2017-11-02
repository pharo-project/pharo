"
A GLMPresentationToolbarUpdated is announced by the presentation when it's toolbar actions ned to be updated. By toolbar actions we refer to those actions that apply on the entire presentations and not those that apply on the selection (which are usually displayed in a context menu).

Only the part of the morph that displays the actions is updated. 
"
Class {
	#name : #GLMPresentationToolbarUpdated,
	#superclass : #Announcement,
	#instVars : [
		'presentation'
	],
	#category : 'Glamour-Announcements'
}

{ #category : #accessing }
GLMPresentationToolbarUpdated >> presentation [
	^ presentation
]

{ #category : #accessing }
GLMPresentationToolbarUpdated >> presentation: anObject [
	presentation := anObject
]
