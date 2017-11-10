"
This announcement is emitted when a window is collapsed, by pressing the '-' top left button of a window.
"
Class {
	#name : #WindowCollapsed,
	#superclass : #WindowAnnouncement,
	#category : #Morphic-Widgets-Windows-Announcements
}

{ #category : #testing }
WindowCollapsed >> isCollapsed [
	^ true
]
