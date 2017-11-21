Class {
	#name : #GLMPagerScrollPositionChanged,
	#superclass : #Announcement,
	#instVars : [
		'smooth',
		'callback'
	],
	#category : #Glamour-Morphic-Pager-Announcement
}

{ #category : #accessing }
GLMPagerScrollPositionChanged >> callback [
	^ callback
]

{ #category : #accessing }
GLMPagerScrollPositionChanged >> callback: anObject [
	callback := anObject
]

{ #category : #accessing }
GLMPagerScrollPositionChanged >> smooth [
	^ smooth
]

{ #category : #accessing }
GLMPagerScrollPositionChanged >> smooth: anObject [
	smooth := anObject
]
