Class {
	#name : #GLMTestAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'parameter'
	],
	#category : #'Glamour-Tests-Core'
}

{ #category : #testing }
GLMTestAnnouncement class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMTestAnnouncement >> parameter [
	^ parameter
]

{ #category : #accessing }
GLMTestAnnouncement >> parameter: anObject [
	parameter := anObject
]
