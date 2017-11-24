Class {
	#name : #GLMPhlowTableColumn,
	#superclass : #Object,
	#instVars : [
		'computation',
		'title',
		'style'
	],
	#category : #'Glamour-Morphic-Brick-Widgets-Phlow'
}

{ #category : #accessing }
GLMPhlowTableColumn >> computation [
	^ computation
]

{ #category : #accessing }
GLMPhlowTableColumn >> computation: anObject [
	computation := anObject
]

{ #category : #accessing }
GLMPhlowTableColumn >> style [
	^ style
]

{ #category : #accessing }
GLMPhlowTableColumn >> style: anObject [
	style := anObject
]

{ #category : #accessing }
GLMPhlowTableColumn >> title [
	^ title
]

{ #category : #accessing }
GLMPhlowTableColumn >> title: anObject [
	title := anObject
]
