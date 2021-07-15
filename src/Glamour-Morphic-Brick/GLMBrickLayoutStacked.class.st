Class {
	#name : #GLMBrickLayoutStacked,
	#superclass : #GLMBrickLayoutEvent,
	#instVars : [
		'index',
		'position'
	],
	#category : #'Glamour-Morphic-Brick-Announcements'
}

{ #category : #testing }
GLMBrickLayoutStacked class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMBrickLayoutStacked >> index [
	^ index
]

{ #category : #accessing }
GLMBrickLayoutStacked >> index: anObject [
	index := anObject
]

{ #category : #accessing }
GLMBrickLayoutStacked >> position [
	^ position
]

{ #category : #accessing }
GLMBrickLayoutStacked >> position: anObject [
	position := anObject
]
