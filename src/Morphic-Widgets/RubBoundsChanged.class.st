"
I am an abstract announcement relates to text area bounds changing
"
Class {
	#name : 'RubBoundsChanged',
	#superclass : 'RubMorphAnnouncement',
	#instVars : [
		'previousBounds'
	],
	#category : 'Morphic-Widgets-Editing-Announcements',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Announcements'
}

{ #category : 'accessing' }
RubBoundsChanged >> previousBounds [
	^ previousBounds
]

{ #category : 'accessing' }
RubBoundsChanged >> previousBounds: anObject [
	previousBounds := anObject
]
