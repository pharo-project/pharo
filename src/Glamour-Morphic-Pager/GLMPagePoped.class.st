Class {
	#name : #GLMPagePoped,
	#superclass : #Announcement,
	#instVars : [
		'smooth'
	],
	#category : #'Glamour-Morphic-Pager-Announcement'
}

{ #category : #'as yet unclassified' }
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
