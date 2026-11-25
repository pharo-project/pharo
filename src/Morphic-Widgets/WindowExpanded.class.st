"
This announcement is emitted when a window leaves a collapsed mode. This happens when you press on a window icon in the taskbar.
"
Class {
	#name : 'WindowExpanded',
	#superclass : 'WindowAnnouncement',
	#category : 'Morphic-Widgets-Window Announcements',
	#package : 'Morphic-Widgets',
	#tag : 'Window Announcements'
}

{ #category : 'testing' }
WindowExpanded >> isExpanded [
	^ true
]
