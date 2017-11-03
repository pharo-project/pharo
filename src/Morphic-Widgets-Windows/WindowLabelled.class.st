"
I am emitted when a window title changed. You can ask me for the window
and the changed label (no old/new label data is provided, just the actual one).

This is usefull for window manager or window grouper that provide an overview
for the active windows. Or like the window grouper, that creates tab panes and labels
the tabs with the current window title.
"
Class {
	#name : #WindowLabelled,
	#superclass : #WindowAnnouncement,
	#instVars : [
		'label'
	],
	#category : #Morphic-Widgets-Windows-Announcements
}

{ #category : #testing }
WindowLabelled >> isLabelled [

	^true
]

{ #category : #accessing }
WindowLabelled >> label [

	^ label
]

{ #category : #accessing }
WindowLabelled >> label: anObject [

	label := anObject
]
