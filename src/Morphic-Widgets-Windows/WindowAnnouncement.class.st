"
I am the base class for all WindowAnnouncements

I define the common interface for WindowAnnouncements. 
You can check the type of the announcement (isMoved/isScrolled ...)
My subclasses implement the specific window announcements and may provide further states.

"
Class {
	#name : #WindowAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'window'
	],
	#category : #Morphic-Widgets-Windows-Announcements
}

{ #category : #testing }
WindowAnnouncement >> isActivated [

	^false
]

{ #category : #testing }
WindowAnnouncement >> isClosed [
	^ false
]

{ #category : #testing }
WindowAnnouncement >> isCollapsed [
	^ false
]

{ #category : #testing }
WindowAnnouncement >> isDeActivated [

	^false
]

{ #category : #testing }
WindowAnnouncement >> isExpanded [
	^ false
]

{ #category : #testing }
WindowAnnouncement >> isLabelled [
	^ false
]

{ #category : #testing }
WindowAnnouncement >> isMoved [
	^ false
]

{ #category : #testing }
WindowAnnouncement >> isResized [
	^ false
]

{ #category : #testing }
WindowAnnouncement >> isScrolled [
	^ false
]

{ #category : #accessing }
WindowAnnouncement >> window [
	^ window
]

{ #category : #accessing }
WindowAnnouncement >> window: anObject [
	window := anObject
]
