"
I am emitted when a window content pane is scrolled.
I provide the scroll step value.
"
Class {
	#name : #WindowScrolling,
	#superclass : #WindowAnnouncement,
	#instVars : [
		'step'
	],
	#category : 'Morphic-Widgets-Windows-Announcements'
}

{ #category : #testing }
WindowScrolling >> isScrolled [
	^ true
]

{ #category : #accessing }
WindowScrolling >> step [
	^ step
]

{ #category : #accessing }
WindowScrolling >> step: anObject [
	step := anObject
]
