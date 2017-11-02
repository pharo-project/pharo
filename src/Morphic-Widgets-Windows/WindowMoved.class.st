"
I am emitted when a window is moved.
I provide the old and new window position. 
A window position changes even when the topleft corner changed by a window resize operation 
or the windows collapses.
"
Class {
	#name : #WindowMoved,
	#superclass : #WindowAnnouncement,
	#instVars : [
		'oldPosition',
		'newPosition'
	],
	#category : 'Morphic-Widgets-Windows-Announcements'
}

{ #category : #testing }
WindowMoved >> isMoved [
	^ true
]

{ #category : #accessing }
WindowMoved >> newPosition [
	^ newPosition
]

{ #category : #accessing }
WindowMoved >> newPosition: anObject [
	newPosition := anObject
]

{ #category : #accessing }
WindowMoved >> oldPosition [
	^ oldPosition
]

{ #category : #accessing }
WindowMoved >> oldPosition: anObject [
	oldPosition := anObject
]
