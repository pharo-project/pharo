Class {
	#name : #GLMPagePoped,
	#superclass : #Announcement,
	#instVars : [
		'smooth'
	],
	#category : #'Glamour-PagerModel-Announcements'
}

{ #category : #initialization }
GLMPagePoped >> initialize [
	super initialize.
	
	smooth := false.
]

{ #category : #accessing }
GLMPagePoped >> smooth [
	^ smooth
]

{ #category : #accessing }
GLMPagePoped >> smooth: anObject [
	smooth := anObject
]
