"
I am emitted when a window is resized.
Collapsing and expanding a window changes the size and therefore it may 
trigger this announcement.

I provide the old and new window size.
"
Class {
	#name : #SpWindowResizing,
	#superclass : #SpWindowAnnouncement,
	#instVars : [
		'newSize',
		'oldSize'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #testing }
SpWindowResizing >> isResized [
	^ true
]

{ #category : #testing }
SpWindowResizing >> newSize [
	^ newSize
]

{ #category : #testing }
SpWindowResizing >> newSize: anObject [
	newSize := anObject
]

{ #category : #testing }
SpWindowResizing >> oldSize [
	^ oldSize
]

{ #category : #testing }
SpWindowResizing >> oldSize: anObject [
	oldSize := anObject
]
