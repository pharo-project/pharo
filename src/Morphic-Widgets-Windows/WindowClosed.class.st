"
I am emitted when a window is closed.
"
Class {
	#name : 'WindowClosed',
	#superclass : 'WindowAnnouncement',
	#category : 'Morphic-Widgets-Windows-Announcements',
	#package : 'Morphic-Widgets-Windows',
	#tag : 'Announcements'
}

{ #category : 'testing' }
WindowClosed >> isClosed [
	^ true
]
