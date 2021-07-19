Class {
	#name : #LazyTabPageRemoved,
	#superclass : #LazyTabPageAnnouncement,
	#instVars : [
		'newIndex'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #testing }
LazyTabPageRemoved class >> isDeprecated [
	^ true
]

{ #category : #accessing }
LazyTabPageRemoved >> newIndex [
	^ newIndex
]

{ #category : #accessing }
LazyTabPageRemoved >> newIndex: anObject [
	newIndex := anObject
]
