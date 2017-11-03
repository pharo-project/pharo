Class {
	#name : #LazyTabPageChanged,
	#superclass : #LazyTabPageAnnouncement,
	#instVars : [
		'oldPage'
	],
	#category : #Glamour-Morphic-Widgets
}

{ #category : #accessing }
LazyTabPageChanged >> oldPage [
	^ oldPage
]

{ #category : #accessing }
LazyTabPageChanged >> oldPage: anObject [
	oldPage := anObject
]
