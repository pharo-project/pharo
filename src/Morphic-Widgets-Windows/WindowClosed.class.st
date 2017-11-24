"
I am emitted when a window is closed.
"
Class {
	#name : #WindowClosed,
	#superclass : #WindowAnnouncement,
	#category : #'Morphic-Widgets-Windows-Announcements'
}

{ #category : #testing }
WindowClosed >> isClosed [
	^ true
]
