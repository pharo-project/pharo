"
I am raised when enter is pressed
"
Class {
	#name : 'RubReturnEntered',
	#superclass : 'RubMorphAnnouncement',
	#instVars : [
		'accepted'
	],
	#category : 'Morphic-Widgets-Editing-Announcements',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Announcements'
}

{ #category : 'accessing' }
RubReturnEntered >> accepted [
	^ accepted ifNil: [ accepted := false ]
]

{ #category : 'accessing' }
RubReturnEntered >> accepted: aBoolean [
	accepted := aBoolean
]
