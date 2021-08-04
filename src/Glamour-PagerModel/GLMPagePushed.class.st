Class {
	#name : #GLMPagePushed,
	#superclass : #Announcement,
	#instVars : [
		'morph'
	],
	#category : #'Glamour-PagerModel-Announcements'
}

{ #category : #testing }
GLMPagePushed class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMPagePushed >> morph [
	^ morph
]

{ #category : #accessing }
GLMPagePushed >> morph: aMorph [
	morph := aMorph
]
