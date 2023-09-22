"
This announcement is emitted when a window leaves a collapsed mode. This happens when you press on a window icon in the taskbar.
"
Class {
	#name : 'WindowExpanded',
	#superclass : 'WindowAnnouncement',
	#category : 'Morphic-Widgets-Windows-Announcements',
	#package : 'Morphic-Widgets-Windows',
	#tag : 'Announcements'
}

{ #category : 'testing' }
WindowExpanded >> isExpanded [
	^ true
]
