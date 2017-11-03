"
I am raised when enter is pressed
"
Class {
	#name : #RubReturnEntered,
	#superclass : #RubMorphAnnouncement,
	#instVars : [
		'accepted'
	],
	#category : #Rubric-Editing-Announcements
}

{ #category : #accessing }
RubReturnEntered >> accepted [
	^ accepted ifNil: [ accepted := false ]
]

{ #category : #accessing }
RubReturnEntered >> accepted: aBoolean [
	accepted := aBoolean
]
