"
I am emitted when a window is resized.
Collapsing and expanding a window changes the size and therefore it may 
trigger this announcement.

I provide the old and new window size.
"
Class {
	#name : #WindowResizing,
	#superclass : #WindowAnnouncement,
	#instVars : [
		'oldSize',
		'newSize'
	],
	#category : 'Morphic-Widgets-Windows-Announcements'
}

{ #category : #testing }
WindowResizing >> isResized [
	^ true
]

{ #category : #accessing }
WindowResizing >> newSize [
	^ newSize
]

{ #category : #accessing }
WindowResizing >> newSize: anObject [
	newSize := anObject
]

{ #category : #accessing }
WindowResizing >> oldSize [
	^ oldSize
]

{ #category : #accessing }
WindowResizing >> oldSize: anObject [
	oldSize := anObject
]
