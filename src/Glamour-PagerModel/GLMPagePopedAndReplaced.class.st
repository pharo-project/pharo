Class {
	#name : #GLMPagePopedAndReplaced,
	#superclass : #Announcement,
	#instVars : [
		'brick'
	],
	#category : #'Glamour-PagerModel-Announcements'
}

{ #category : #testing }
GLMPagePopedAndReplaced class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMPagePopedAndReplaced >> brick [
	^ brick
]

{ #category : #accessing }
GLMPagePopedAndReplaced >> brick: anObject [
	brick := anObject
]
