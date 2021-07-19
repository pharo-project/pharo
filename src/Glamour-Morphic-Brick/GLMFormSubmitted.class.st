Class {
	#name : #GLMFormSubmitted,
	#superclass : #Announcement,
	#instVars : [
		'result'
	],
	#category : #'Glamour-Morphic-Brick-Announcements'
}

{ #category : #testing }
GLMFormSubmitted class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMFormSubmitted >> result [
	^ result
]

{ #category : #accessing }
GLMFormSubmitted >> result: anObject [
	result := anObject
]
