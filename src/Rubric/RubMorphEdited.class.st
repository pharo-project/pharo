"
I am raised when a morph is edited
"
Class {
	#name : 'RubMorphEdited',
	#superclass : 'RubMorphAnnouncement',
	#instVars : [
		'previousContents'
	],
	#category : 'Rubric-Editing-Announcements',
	#package : 'Rubric',
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
