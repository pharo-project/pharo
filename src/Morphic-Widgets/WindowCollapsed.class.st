"
This announcement is emitted when a window is collapsed, by pressing the '-' top left button of a window.
"
Class {
	#name : 'WindowCollapsed',
	#superclass : 'WindowAnnouncement',
	#category : 'Morphic-Widgets-Window Announcements',
	#package : 'Morphic-Widgets',
	#tag : 'Window Announcements'
}

{ #category : 'testing' }
WindowCollapsed >> isCollapsed [
	^ true
]
