"
I am raised when a morph is edited
"
Class {
	#name : 'RubMorphEdited',
	#superclass : 'RubMorphAnnouncement',
	#instVars : [
		'previousContents'
	],
	#category : 'Morphic-Widgets-Editing-Announcements',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Announcements'
}

{ #category : 'accessing' }
RubMorphEdited >> previousContents [
	^ previousContents
]

{ #category : 'accessing' }
RubMorphEdited >> previousContents: anObject [
	previousContents := anObject
]
