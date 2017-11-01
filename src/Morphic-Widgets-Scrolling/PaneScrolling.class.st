"
I am an announcement raised when a scroll pane is scrolled.

My two attributes are
	- step: a point representing the scrollbar movement
	- scrollPane: a pointer to the widget raising the announcement
"
Class {
	#name : #PaneScrolling,
	#superclass : #Announcement,
	#instVars : [
		'step',
		'scrollPane'
	],
	#category : 'Morphic-Widgets-Scrolling'
}

{ #category : #accessing }
PaneScrolling >> scrollPane [
	^ scrollPane
]

{ #category : #accessing }
PaneScrolling >> scrollPane: anObject [
	scrollPane := anObject
]

{ #category : #accessing }
PaneScrolling >> step [
	^ step
]

{ #category : #accessing }
PaneScrolling >> step: anObject [
	step := anObject
]
