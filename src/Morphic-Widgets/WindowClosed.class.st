"
I am emitted when a window is closed.
"
Class {
	#name : 'WindowClosed',
	#superclass : 'WindowAnnouncement',
	#category : 'Morphic-Widgets-Window Announcements',
	#package : 'Morphic-Widgets',
	#tag : 'Window Announcements'
}

{ #category : 'testing' }
WindowClosed >> isClosed [
	^ true
]
